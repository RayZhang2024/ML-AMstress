"""Trusted, exact-head A5.4a review and bounded repair coordinator.

Only this module talks to GitHub.  It imports the reviewed A5.1/A5.2/A5.3
modules from the trusted ``main`` checkout before any PR branch is checked
out; their Codex child processes remain credential-isolated.
"""
from __future__ import annotations

import dataclasses
import hashlib
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request
from typing import Any, Callable, Mapping, Sequence

from scripts import a5_repair_worker as repair
from scripts import a5_review_state as state_contract
from scripts import a5_reviewer as reviewer
from scripts import codex_issue_worker as green_worker


REPOSITORY = "RayZhang2024/ML-AMstress"
BASE_BRANCH = "main"
CI_WORKFLOW_NAME = "Normal Python CI"
TERMINAL_CONCLUSIONS = frozenset(("success", "failure", "cancelled", "skipped", "timed_out", "action_required", "neutral", "startup_failure", "stale"))
REVIEW_LABELS = frozenset(("review:pending", "review:blocker", "review:clean", "review:escalated"))
REVIEW_LABEL_SPECS = {
    name: {"name": name, "color": "0366d6", "description": "A5.2 PR review state"}
    for name in sorted(REVIEW_LABELS)
}
MAX_REPAIR_ATTEMPTS = repair.MAX_REPAIR_ATTEMPTS
MAX_COMMENTS = 200
MAX_CHANGED_FILES = reviewer.MAX_CHANGED_FILES
MAX_AUDIT = 4096
TRUSTED_AUDIT_AUTHOR = "github-actions[bot]"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
BRANCH_RE = re.compile(r"^codex/issue-[1-9][0-9]*-[a-z0-9][a-z0-9-]{0,80}$")
CLOSES_RE = re.compile(r"(?im)^\s*(?:closes?|fix(?:es)?|resolves?)\s+#([1-9][0-9]*)\s*$")
STATE_MARKER_RE = re.compile(r"^<!-- a5\.4a-state:(\{.*\}) -->$")
CI_MARKER_RE = re.compile(r"^<!-- a5\.4a-ci:(\{.*\}) -->$")
REPAIR_MARKER_RE = re.compile(r"^<!-- a5\.4a-repair:(\{.*\}) -->$")
A5_GITHUB_USER_AGENT = "ml-amstress-a5-review-loop"
REST_OPERATIONS = frozenset((
    "list-open-prs", "get-pr", "get-issue", "get-dependency-issue",
    "list-comments", "list-labels", "create-label", "list-changed-files",
    "set-labels", "create-audit-comment",
))


class OrchestrationError(Exception):
    """A fail-closed trusted orchestration failure."""


def require_automation_app_token() -> str:
    token = os.environ.get("AUTOMATION_APP_TOKEN")
    if not token:
        raise OrchestrationError("AUTOMATION_APP_TOKEN is required for trusted repair pushes")
    return token


@dataclasses.dataclass(frozen=True)
class WorkflowRun:
    run_id: int
    head_sha: str
    conclusion: str


@dataclasses.dataclass(frozen=True)
class CurrentReviewState:
    issue_status: str
    review_label: str | None
    review_head_sha: str | None


def _sha(value: Any, name: str) -> str:
    if not isinstance(value, str) or not SHA_RE.fullmatch(value):
        raise OrchestrationError(name + " must be a lowercase 40-character SHA")
    return value


def _bounded_text(value: Any, name: str, maximum: int = reviewer.MAX_TEXT) -> str:
    if not isinstance(value, str) or len(value) > maximum:
        raise OrchestrationError(name + " must be a bounded string")
    return value


def parse_workflow_run(event: Mapping[str, Any]) -> WorkflowRun:
    """Accept only an exact completed terminal Normal Python CI event."""
    run = event.get("workflow_run") if isinstance(event, Mapping) else None
    if not isinstance(run, Mapping) or run.get("name") != CI_WORKFLOW_NAME:
        raise OrchestrationError("event is not an exact Normal Python CI workflow run")
    if run.get("status") != "completed":
        raise OrchestrationError("workflow run is not completed")
    conclusion = run.get("conclusion")
    if conclusion not in TERMINAL_CONCLUSIONS:
        raise OrchestrationError("workflow run conclusion is not terminal")
    run_id = run.get("id")
    if isinstance(run_id, bool) or not isinstance(run_id, int) or run_id < 1:
        raise OrchestrationError("workflow run id is invalid")
    return WorkflowRun(run_id, _sha(run.get("head_sha"), "workflow run head"), conclusion)


def canonical_linked_issue(pr: Mapping[str, Any]) -> int:
    """Return the one canonical closing issue reference from a bounded PR body."""
    body = _bounded_text(pr.get("body", ""), "PR body")
    references = {int(match.group(1)) for match in CLOSES_RE.finditer(body)}
    if len(references) != 1:
        raise OrchestrationError("PR must link exactly one canonical closing issue")
    return next(iter(references))


def _label_names(item: Mapping[str, Any]) -> tuple[str, ...]:
    raw = item.get("labels", ())
    if not isinstance(raw, Sequence) or isinstance(raw, (str, bytes)):
        raise OrchestrationError("labels are malformed")
    names = tuple(sorted(value.get("name") if isinstance(value, Mapping) else str(value) for value in raw))
    if len(names) != len(set(names)):
        raise OrchestrationError("labels are duplicated")
    return names


def _one_label(names: Sequence[str], prefix: str, optional: bool = False) -> str | None:
    selected = [name for name in names if name.startswith(prefix)]
    if len(selected) != 1:
        if optional and not selected:
            return None
        raise OrchestrationError("requires exactly one " + prefix + " label")
    return selected[0]


def _pr_branch(pr: Mapping[str, Any]) -> str:
    head = pr.get("head")
    if not isinstance(head, Mapping):
        raise OrchestrationError("PR head is malformed")
    branch = head.get("ref")
    repository = head.get("repo")
    if not isinstance(repository, Mapping) or repository.get("full_name") != REPOSITORY:
        raise OrchestrationError("PR head must be an in-repository branch")
    if not isinstance(branch, str) or not BRANCH_RE.fullmatch(branch):
        raise OrchestrationError("PR branch is not a deterministic Codex worker branch")
    return branch


def validate_pr_identity(pr: Mapping[str, Any], run: WorkflowRun) -> tuple[int, str]:
    if str(pr.get("state", "")).lower() != "open":
        raise OrchestrationError("PR is not open")
    base = pr.get("base")
    head = pr.get("head")
    if not isinstance(base, Mapping) or base.get("ref") != BASE_BRANCH or not isinstance(head, Mapping):
        raise OrchestrationError("PR must target main with a valid head")
    if _sha(head.get("sha"), "PR head") != run.head_sha:
        raise OrchestrationError("PR head differs from workflow run head")
    number = pr.get("number")
    if isinstance(number, bool) or not isinstance(number, int) or number < 1:
        raise OrchestrationError("PR number is invalid")
    return number, _pr_branch(pr)


def validate_issue_identity(issue: Mapping[str, Any], branch: str, issue_number: int) -> green_worker.Contract:
    if issue.get("number") != issue_number or str(issue.get("state", "")).lower() != "open":
        raise OrchestrationError("linked issue is not the exact open issue")
    labels = _label_names(issue)
    if "risk:green" not in labels or "agent:codex" not in labels:
        raise OrchestrationError("linked issue is not eligible GREEN Codex work")
    if len([name for name in labels if name.startswith("risk:")]) != 1:
        raise OrchestrationError("linked issue has ambiguous risk labels")
    status = _one_label(labels, "status:")
    if status not in state_contract.ISSUE_STATUSES:
        raise OrchestrationError("linked issue has an unsupported implementation status")
    contract = green_worker.parse_contract(_bounded_text(issue.get("body", ""), "issue body"), REPOSITORY)
    if contract.risk != "risk:green":
        raise OrchestrationError("linked issue contract is not GREEN-only")
    expected = green_worker.deterministic_branch_name(issue_number, _bounded_text(issue.get("title", ""), "issue title", 300))
    if branch != expected:
        raise OrchestrationError("PR branch does not match the deterministic issue branch")
    return contract


def validate_dependencies(client: Any, contract: green_worker.Contract) -> None:
    for dependency in contract.dependencies:
        evidence = client.dependency_issue(dependency)
        if not isinstance(evidence, Mapping) or str(evidence.get("state", "")).lower() != "closed":
            raise OrchestrationError("issue dependency is not satisfied")


def authorization_fingerprint(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any],
                              run: WorkflowRun) -> str:
    """Bind a reviewer result to the complete trusted authorization evidence."""
    pr_number, branch = validate_pr_identity(pr, run)
    issue_number = canonical_linked_issue(pr)
    contract = validate_issue_identity(issue, branch, issue_number)
    dependency_states = []
    for dependency in contract.dependencies:
        evidence = client.dependency_issue(dependency)
        if not isinstance(evidence, Mapping) or str(evidence.get("state", "")).lower() != "closed":
            raise OrchestrationError("issue dependency is not satisfied")
        dependency_states.append((dependency.repository, dependency.number, "closed"))
    identity = {"pr_number": pr_number, "pr_title": pr.get("title"), "pr_body": pr.get("body"),
                "base": pr.get("base"), "head": pr.get("head"), "pr_labels": _label_names(pr),
                "issue_number": issue_number, "issue_title": issue.get("title"), "issue_body": issue.get("body"),
                "issue_labels": _label_names(issue), "dependencies": dependency_states}
    return hashlib.sha256(json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")).hexdigest()


def _marker(pattern: re.Pattern[str], body: str) -> dict[str, Any] | None:
    match = pattern.fullmatch(body.strip())
    if not match:
        return None
    try:
        value = json.loads(match.group(1))
    except (TypeError, ValueError):
        raise OrchestrationError("audit marker JSON is malformed")
    if not isinstance(value, dict):
        raise OrchestrationError("audit marker is malformed")
    return value


def _trusted_comment(comment: Any) -> bool:
    author = comment.get("user") if isinstance(comment, Mapping) else None
    return isinstance(author, Mapping) and author.get("login") == TRUSTED_AUDIT_AUTHOR


def _comments_with_marker(comments: Sequence[Mapping[str, Any]], pattern: re.Pattern[str]) -> list[dict[str, Any]]:
    if len(comments) > MAX_COMMENTS:
        raise OrchestrationError("too many comments to inspect safely")
    found = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or len(body) > MAX_AUDIT:
            continue
        if not _trusted_comment(comment):
            continue
        value = _marker(pattern, body)
        if value is not None:
            found.append(value)
    return found


def current_review_state(pr: Mapping[str, Any], issue: Mapping[str, Any], comments: Sequence[Mapping[str, Any]]) -> CurrentReviewState:
    issue_status = _one_label(_label_names(issue), "status:")
    review_label = _one_label(_label_names(pr), "review:", optional=True)
    if review_label is not None and review_label not in REVIEW_LABELS:
        raise OrchestrationError("PR has an unsupported review label")
    if review_label is None:
        if issue_status != "status:review":
            raise OrchestrationError("uninitialized PR review state requires status:review")
        if _comments_with_marker(comments, STATE_MARKER_RE):
            raise OrchestrationError("uninitialized review labels conflict with trusted state audit")
        return CurrentReviewState(issue_status, None, None)
    markers = _comments_with_marker(comments, STATE_MARKER_RE)
    matching = [item for item in markers if item.get("new_issue_status") == issue_status and item.get("new_pr_review_state") == review_label]
    if not matching:
        raise OrchestrationError("review label has no matching trusted state audit")
    marker = matching[-1]
    review_head = _sha(marker.get("new_review_state_head_sha"), "review-state audit head")
    return CurrentReviewState(issue_status, review_label, review_head)


def _state_input(pr_number: int, issue_number: int, head: str, current: CurrentReviewState,
                 event_kind: str, verdict: reviewer.ReviewVerdict | None = None) -> state_contract.ReviewStateInput:
    validated = None
    if verdict is not None:
        validated = state_contract.ValidatedVerdict(verdict.verdict, verdict.reviewed_head_sha,
                                                    verdict.effective_risk, tuple(item.id for item in verdict.findings))
    return state_contract.ReviewStateInput(1, REPOSITORY, pr_number, issue_number, head,
                                           current.issue_status, current.review_label, current.review_head_sha,
                                           validated, event_kind)


def _audit_body(state: state_contract.ReviewStateInput, plan: state_contract.TransitionPlan) -> str:
    body = "<!-- a5.4a-state:" + state_contract.serialize_audit(state, plan) + " -->"
    if len(body) > MAX_AUDIT:
        raise OrchestrationError("state audit is unexpectedly unbounded")
    return body


def _has_state_decision(comments: Sequence[Mapping[str, Any]], key: str) -> bool:
    return any(item.get("decision_key") == key for item in _comments_with_marker(comments, STATE_MARKER_RE))


def _replace_label(names: Sequence[str], family: str, target: str) -> list[str]:
    return sorted([name for name in names if not name.startswith(family)] + [target])


def ensure_review_labels(client: Any) -> None:
    """Provision the exact A5.2 label vocabulary before any state mutation."""
    available = client.repository_labels()
    if not isinstance(available, Sequence) or isinstance(available, (str, bytes)):
        raise OrchestrationError("repository label evidence is malformed")
    names = []
    for item in available:
        name = item.get("name") if isinstance(item, Mapping) else None
        if not isinstance(name, str):
            raise OrchestrationError("repository label evidence is malformed")
        names.append(name)
    for required in REVIEW_LABELS:
        variants = [name for name in names if name.casefold() == required.casefold()]
        if len(variants) > 1 or (variants and variants[0] != required):
            raise OrchestrationError("repository review label evidence is ambiguous")
    for required in sorted(REVIEW_LABELS):
        if required not in names:
            client.create_label(REVIEW_LABEL_SPECS[required])
    verified = client.repository_labels()
    verified_names = [item.get("name") for item in verified if isinstance(item, Mapping)]
    if any(verified_names.count(required) != 1 for required in REVIEW_LABELS):
        raise OrchestrationError("repository review label provisioning could not be verified")


def apply_transition(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any], comments: Sequence[Mapping[str, Any]],
                     state: state_contract.ReviewStateInput, plan: state_contract.TransitionPlan) -> None:
    """Apply only one A5.2-derived transition and its idempotent bounded audit."""
    if _has_state_decision(comments, plan.decision_key):
        return
    if plan.idempotent_no_op:
        return
    client.set_labels(pr["number"], _replace_label(_label_names(pr), "review:", plan.next_pr_review_state))
    client.set_labels(issue["number"], _replace_label(_label_names(issue), "status:", plan.next_issue_status))
    client.comment(pr["number"], _audit_body(state, plan))


def _ci_marker(run: WorkflowRun, pr_number: int) -> str:
    payload = {"schema_version": 1, "run_id": run.run_id, "head_sha": run.head_sha,
               "pr_number": pr_number, "conclusion": run.conclusion}
    return "<!-- a5.4a-ci:" + json.dumps(payload, sort_keys=True, separators=(",", ":")) + " -->"


def record_ci_observation(client: Any, comments: Sequence[Mapping[str, Any]], run: WorkflowRun, pr_number: int) -> None:
    marker = _ci_marker(run, pr_number)
    if marker not in [comment.get("body") for comment in comments if _trusted_comment(comment)]:
        client.comment(pr_number, marker)


def _trusted_green_paths(files: Sequence[reviewer.ChangedFile]) -> tuple[str, ...]:
    paths = tuple(item.path for item in files)
    allowed, disallowed = green_worker.green_changed_paths(paths)
    if disallowed or tuple(allowed) != paths or not paths:
        raise OrchestrationError("PR changed paths are not an exact trusted GREEN repair allowlist")
    return paths


def build_snapshot(pr: Mapping[str, Any], issue: Mapping[str, Any], run: WorkflowRun,
                   files: Sequence[Mapping[str, Any]]) -> tuple[dict[str, Any], tuple[str, ...]]:
    if len(files) == 0 or len(files) > MAX_CHANGED_FILES:
        raise OrchestrationError("changed-file evidence is missing or exceeds the safe bound")
    changed = []
    for item in files:
        if not isinstance(item, Mapping) or not isinstance(item.get("filename"), str) or not isinstance(item.get("patch"), str):
            raise OrchestrationError("changed-file patch evidence is incomplete")
        changed.append(reviewer.ChangedFile(item["filename"], item["patch"]))
    paths = _trusted_green_paths(changed)
    head = _sha(pr["head"].get("sha"), "PR head")
    snapshot = {"schema_version": 1, "repository": REPOSITORY, "pull_request_number": pr["number"],
                "issue_number": issue["number"], "base_sha": _sha(pr["base"].get("sha"), "PR base"),
                "head_sha": head, "pr_title": _bounded_text(pr.get("title", ""), "PR title"),
                "pr_body": _bounded_text(pr.get("body", ""), "PR body"),
                "issue_title": _bounded_text(issue.get("title", ""), "issue title"),
                "issue_body": _bounded_text(issue.get("body", ""), "issue body"),
                "issue_labels": list(_label_names(issue)), "declared_risk": "green", "trusted_risk_floor": "green",
                "changed_files": [{"path": item.path, "patch": item.patch} for item in changed],
                "ci_checks": [{"name": CI_WORKFLOW_NAME, "status": "success"}],
                "worker_metadata": {"worker_run_id": str(run.run_id), "branch": _pr_branch(pr)}}
    reviewer.validate_snapshot(snapshot)
    return snapshot, paths


def _repair_marker(pr_number: int, issue_number: int, head: str, decision_key: str, attempt: int,
                   finding_ids: Sequence[str]) -> str:
    payload = {"schema_version": 1, "pr_number": pr_number, "issue_number": issue_number, "head_sha": head,
               "decision_key": decision_key, "attempt": attempt, "finding_ids": list(finding_ids)}
    result = "<!-- a5.4a-repair:" + json.dumps(payload, sort_keys=True, separators=(",", ":")) + " -->"
    if len(result) > MAX_AUDIT:
        raise OrchestrationError("repair audit is unexpectedly unbounded")
    return result


def repair_attempt_count(comments: Sequence[Mapping[str, Any]], pr_number: int) -> int:
    values = _comments_with_marker(comments, REPAIR_MARKER_RE)
    attempts = []
    for item in values:
        if item.get("pr_number") != pr_number:
            continue
        if item.get("attempt") not in (1, 2) or not isinstance(item.get("decision_key"), str):
            raise OrchestrationError("repair attempt marker is malformed")
        attempts.append(item)
    if len(attempts) != len({item.get("attempt") for item in attempts}):
        raise OrchestrationError("repair attempt markers are ambiguous")
    return len(attempts)


def accepted_blocker_decision_key(comments: Sequence[Mapping[str, Any]], pr_number: int,
                                  issue_number: int, head: str) -> str:
    """Recover only the original pending-to-blocker A5.2 decision identity."""
    matches = []
    for item in _comments_with_marker(comments, STATE_MARKER_RE):
        if (item.get("pull_request_number") == pr_number and item.get("issue_number") == issue_number
                and item.get("event_kind") == "verdict" and item.get("verdict") == "blocker"
                and item.get("current_head_sha") == head and item.get("new_review_state_head_sha") == head
                and item.get("old_pr_review_state") == "review:pending"
                and item.get("new_pr_review_state") == "review:blocker"
                and item.get("old_issue_status") == "status:review"
                and item.get("new_issue_status") == "status:in-progress"):
            key = item.get("decision_key")
            if not isinstance(key, str) or not repair.A5_2_DECISION_KEY_RE.fullmatch(key):
                raise OrchestrationError("accepted blocker audit has an invalid decision key")
            matches.append(key)
    if len(set(matches)) != 1:
        raise OrchestrationError("accepted blocker decision audit is missing or ambiguous")
    return matches[0]


def checkout_exact_pr_branch(branch: str, expected_head: str, cwd: str) -> None:
    """Fetch and switch only the exact in-repository PR branch for A5.3."""
    def run(command: Sequence[str]) -> str:
        result = subprocess.run(list(command), cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False)
        if result.returncode:
            raise OrchestrationError("trusted exact-head checkout failed")
        return result.stdout.strip()
    run(("git", "fetch", "origin", "refs/heads/%s:refs/remotes/origin/%s" % (branch, branch)))
    if run(("git", "rev-parse", "origin/" + branch)) != expected_head:
        raise OrchestrationError("remote PR branch no longer matches expected head")
    run(("git", "switch", "-C", branch, "--track", "origin/" + branch))
    if run(("git", "rev-parse", "HEAD")) != expected_head:
        raise OrchestrationError("checked-out PR branch no longer matches expected head")


def _refetch_unchanged(client: Any, pr_number: int, issue_number: int, head: str, run: WorkflowRun,
                        prior: CurrentReviewState, authorization: str) -> tuple[Mapping[str, Any], Mapping[str, Any], list[Mapping[str, Any]]]:
    pr, issue, comments = client.pr(pr_number), client.issue(issue_number), client.comments(pr_number)
    observed_pr_number, branch = validate_pr_identity(pr, run)
    if observed_pr_number != pr_number or _sha(pr.get("head", {}).get("sha"), "re-fetched PR head") != head:
        raise OrchestrationError("PR head changed after reviewer evidence")
    if canonical_linked_issue(pr) != issue_number:
        raise OrchestrationError("PR issue link changed after reviewer evidence")
    contract = validate_issue_identity(issue, branch, issue_number)
    validate_dependencies(client, contract)
    if authorization_fingerprint(client, pr, issue, run) != authorization:
        raise OrchestrationError("authorization evidence changed after reviewer execution")
    if current_review_state(pr, issue, comments) != prior:
        raise OrchestrationError("review state changed after reviewer evidence")
    return pr, issue, comments


def _revalidate_repair_authorization(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any],
                                     comments: Sequence[Mapping[str, Any]], run: WorkflowRun,
                                     issue_number: int) -> CurrentReviewState:
    """Re-check trusted scope after applying blocker state and before A5.3."""
    pr_number, branch = validate_pr_identity(pr, run)
    if canonical_linked_issue(pr) != issue_number:
        raise OrchestrationError("PR issue link changed before repair")
    contract = validate_issue_identity(issue, branch, issue_number)
    validate_dependencies(client, contract)
    current = current_review_state(pr, issue, comments)
    if (pr_number < 1 or current.issue_status != "status:in-progress"
            or current.review_label != "review:blocker" or current.review_head_sha != run.head_sha):
        raise OrchestrationError("blocker repair authorization no longer matches the exact head")
    return current


def _repair(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any], comments: Sequence[Mapping[str, Any]],
            current: CurrentReviewState, verdict: reviewer.ReviewVerdict, accepted_blocker_key: str,
            paths: tuple[str, ...], cwd: str) -> str:
    attempts = repair_attempt_count(comments, pr["number"])
    if attempts >= MAX_REPAIR_ATTEMPTS:
        exhausted = "<!-- a5.4a-repair-exhausted:{\"schema_version\":1,\"pr_number\":%d} -->" % pr["number"]
        if exhausted not in [item.get("body") for item in comments if _trusted_comment(item)]:
            client.comment(pr["number"], exhausted)
        return "repair-exhausted"
    attempt = attempts + 1
    request = repair.RepairRequest(1, REPOSITORY, pr["number"], issue["number"], _pr_branch(pr), pr["head"]["sha"],
                                   accepted_blocker_key, current.issue_status, current.review_label, current.review_head_sha,
                                   "green", tuple(repair.BlockerFinding(item.id, item.category, item.message,
                                   item.required_action, item.required_evidence) for item in verdict.findings), paths, attempt)
    repair.validate_request(request)
    client.comment(pr["number"], _repair_marker(pr["number"], issue["number"], pr["head"]["sha"], accepted_blocker_key, attempt,
                                                  tuple(item.id for item in verdict.findings)))
    try:
        checkout_exact_pr_branch(request.branch, request.expected_head_sha, cwd)
        result = repair.execute_repair(request, cwd)
    except Exception:
        failed = "<!-- a5.4a-repair-failed:{\"schema_version\":1,\"attempt\":%d,\"category\":\"trusted-repair-failed\"} -->" % attempt
        if failed not in [item.get("body") for item in client.comments(pr["number"]) if _trusted_comment(item)]:
            client.comment(pr["number"], failed)
        return "repair-failed"
    refreshed = client.pr(pr["number"])
    if _sha(refreshed.get("head", {}).get("sha"), "repaired PR head") != result.new_head_sha:
        raise OrchestrationError("repair push head could not be verified")
    refreshed_issue, refreshed_comments = client.issue(issue["number"]), client.comments(pr["number"])
    refreshed_state = current_review_state(refreshed, refreshed_issue, refreshed_comments)
    plan_input = _state_input(pr["number"], issue["number"], result.new_head_sha, refreshed_state, "new_head")
    plan = state_contract.transition(plan_input)
    apply_transition(client, refreshed, refreshed_issue, refreshed_comments, plan_input, plan)
    return "repair-pushed"


def orchestrate(client: Any, event: Mapping[str, Any], cwd: str,
                review_runner: Callable[[Mapping[str, Any], str], reviewer.ReviewVerdict] = reviewer.review_snapshot) -> str:
    """Run one exact-head CI observation; returns a bounded terminal category."""
    run = parse_workflow_run(event)
    matches = client.open_prs_for_head(run.head_sha)
    if len(matches) != 1:
        raise OrchestrationError("workflow head must resolve to exactly one open PR")
    pr = client.pr(matches[0]["number"])
    pr_number, branch = validate_pr_identity(pr, run)
    issue_number = canonical_linked_issue(pr)
    issue, comments = client.issue(issue_number), client.comments(pr_number)
    contract = validate_issue_identity(issue, branch, issue_number)
    validate_dependencies(client, contract)
    if run.conclusion != "success":
        record_ci_observation(client, comments, run, pr_number)
        return "ci-non-success"

    ensure_review_labels(client)
    current = current_review_state(pr, issue, comments)
    if current.review_label == "review:escalated":
        return "review-escalated"
    if current.review_label in ("review:clean", "review:blocker") and current.review_head_sha != run.head_sha:
        transition_input = _state_input(pr_number, issue_number, run.head_sha, current, "new_head")
        plan = state_contract.transition(transition_input)
        apply_transition(client, pr, issue, comments, transition_input, plan)
        pr, issue, comments = client.pr(pr_number), client.issue(issue_number), client.comments(pr_number)
        current = current_review_state(pr, issue, comments)
    if current.review_label is None:
        transition_input = _state_input(pr_number, issue_number, run.head_sha, current, "initialize")
        plan = state_contract.transition(transition_input)
        apply_transition(client, pr, issue, comments, transition_input, plan)
        pr, issue, comments = client.pr(pr_number), client.issue(issue_number), client.comments(pr_number)
        current = current_review_state(pr, issue, comments)
    if current.review_label == "review:clean" and current.review_head_sha == run.head_sha:
        return "review-clean"

    snapshot, paths = build_snapshot(pr, issue, run, client.changed_files(pr_number))
    authorization = authorization_fingerprint(client, pr, issue, run)
    if current.review_label not in ("review:pending", "review:blocker") or current.review_head_sha != run.head_sha:
        raise OrchestrationError("current review state is not pending for the exact CI head")
    verdict = review_runner(snapshot, cwd)
    if not isinstance(verdict, reviewer.ReviewVerdict):
        raise OrchestrationError("reviewer returned an invalid verdict object")
    if verdict.effective_risk != "green" and verdict.verdict != "escalate":
        raise OrchestrationError("non-GREEN reviewer risk must not advance or repair automatically")
    pr, issue, comments = _refetch_unchanged(client, pr_number, issue_number, run.head_sha, run, current, authorization)
    transition_input = _state_input(pr_number, issue_number, run.head_sha, current, "verdict", verdict)
    plan = state_contract.transition(transition_input)
    apply_transition(client, pr, issue, comments, transition_input, plan)
    if verdict.verdict == "clean":
        return "review-clean"
    if verdict.verdict == "escalate":
        return "review-escalated"
    pr, issue, comments = client.pr(pr_number), client.issue(issue_number), client.comments(pr_number)
    current = _revalidate_repair_authorization(client, pr, issue, comments, run, issue_number)
    accepted_key = plan.decision_key if not plan.idempotent_no_op else accepted_blocker_decision_key(
        comments, pr_number, issue_number, run.head_sha)
    return _repair(client, pr, issue, comments, current, verdict, accepted_key, paths, cwd)


class GitHubClient:
    """Small trusted REST boundary; it contains no merge or auto-merge route."""
    def __init__(self, token: str | None, repository: str = REPOSITORY):
        if not token:
            raise OrchestrationError("GITHUB_TOKEN is required by trusted orchestration")
        self.token, self.repository = token, repository

    def _request(self, operation: str, method: str, path: str, payload: Any = None,
                 repository: str | None = None) -> Any:
        if operation not in REST_OPERATIONS:
            raise OrchestrationError("trusted GitHub operation is invalid")
        data = None if payload is None else json.dumps(payload).encode("utf-8")
        request = urllib.request.Request("https://api.github.com/repos/%s%s" % (repository or self.repository, path), data=data, method=method)
        request.add_header("Accept", "application/vnd.github+json")
        request.add_header("Authorization", "Bearer " + self.token)
        request.add_header("X-GitHub-Api-Version", "2022-11-28")
        request.add_header("User-Agent", A5_GITHUB_USER_AGENT)
        if data is not None:
            request.add_header("Content-Type", "application/json")
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            raise OrchestrationError("GitHub %s: HTTP %d" % (operation, error.code)) from None
        except urllib.error.URLError:
            raise OrchestrationError("GitHub %s: transport failure" % operation) from None
        except (UnicodeError, ValueError):
            raise OrchestrationError("GitHub %s: invalid response" % operation) from None

    def pr(self, number: int) -> Mapping[str, Any]:
        return self._request("get-pr", "GET", "/pulls/%d" % number)

    def issue(self, number: int) -> Mapping[str, Any]:
        return self._request("get-issue", "GET", "/issues/%d" % number)

    def dependency_issue(self, dependency: green_worker.Dependency) -> Mapping[str, Any]:
        return self._request("get-dependency-issue", "GET", "/issues/%d" % dependency.number,
                             repository=dependency.repository)

    def repository_labels(self) -> list[Mapping[str, Any]]:
        value = self._request("list-labels", "GET", "/labels?per_page=100")
        if not isinstance(value, list) or len(value) >= 100:
            raise OrchestrationError("GitHub repository labels response is malformed")
        return value

    def create_label(self, specification: Mapping[str, str]) -> None:
        if not isinstance(specification, Mapping) or set(specification) != {"name", "color", "description"}:
            raise OrchestrationError("review label specification is malformed")
        self._request("create-label", "POST", "/labels", dict(specification))

    def comments(self, number: int) -> list[Mapping[str, Any]]:
        value = self._request("list-comments", "GET", "/issues/%d/comments?per_page=100" % number)
        if not isinstance(value, list) or len(value) >= 100:
            raise OrchestrationError("GitHub comments response is malformed")
        return value

    def changed_files(self, number: int) -> list[Mapping[str, Any]]:
        value = self._request("list-changed-files", "GET", "/pulls/%d/files?per_page=100" % number)
        if not isinstance(value, list) or len(value) >= 100:
            raise OrchestrationError("GitHub changed-file response is malformed")
        return value

    def open_prs_for_head(self, head_sha: str) -> list[Mapping[str, Any]]:
        value = self._request("list-open-prs", "GET", "/pulls?state=open&per_page=100")
        if not isinstance(value, list) or len(value) >= 100:
            raise OrchestrationError("GitHub PR response is malformed")
        return [item for item in value if isinstance(item, Mapping) and item.get("head", {}).get("sha") == head_sha]

    def set_labels(self, number: int, labels: Sequence[str]) -> None:
        self._request("set-labels", "PUT", "/issues/%d/labels" % number, {"labels": list(labels)})

    def comment(self, number: int, body: str) -> None:
        if not isinstance(body, str) or len(body) > MAX_AUDIT:
            raise OrchestrationError("refusing unbounded audit comment")
        self._request("create-audit-comment", "POST", "/issues/%d/comments" % number, {"body": body})


def main(arguments: Sequence[str] | None = None) -> None:
    if arguments is None:
        arguments = sys.argv[1:]
    if arguments:
        raise OrchestrationError("orchestrator accepts no command-line arguments")
    event_path = os.environ.get("GITHUB_EVENT_PATH")
    if not event_path:
        raise OrchestrationError("GITHUB_EVENT_PATH is required")
    require_automation_app_token()
    with open(event_path, "r", encoding="utf-8") as stream:
        event = json.load(stream)
    orchestrate(GitHubClient(os.environ.get("GITHUB_TOKEN")), event, os.getcwd())


if __name__ == "__main__":
    try:
        main()
    except OrchestrationError as error:
        print("A5.4a orchestration blocked: " + str(error), file=sys.stderr)
        sys.exit(1)
