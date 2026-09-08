"""Trusted, exact-head A5.4a review and bounded repair coordinator.

Only this module talks to GitHub.  It imports the reviewed A5.1/A5.2/A5.3
modules from the trusted ``main`` checkout before any PR branch is checked
out; their Codex child processes remain credential-isolated.
"""
from __future__ import annotations

import dataclasses
import errno
import hashlib
import json
import os
import re
import socket
import ssl
import subprocess
import sys
import urllib.error
import urllib.request
from typing import Any, Callable, Mapping, Sequence

from scripts import a5_repair_worker as repair
from scripts import a5_yellow_repair_worker as yellow_repair
from scripts import a5_review_state as state_contract
from scripts import a5_reviewer as reviewer
from scripts import codex_issue_worker as green_worker
from scripts import yellow_lane_policy as yellow_policy


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
MAX_PROTECTED_BLOCKER_FINDINGS = 50
TRUSTED_AUDIT_AUTHOR = "github-actions[bot]"
TRUSTED_MAINTAINER_AUTHOR = "RayZhang2024"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
BRANCH_RE = re.compile(r"^codex/issue-[1-9][0-9]*-[a-z0-9][a-z0-9-]{0,80}$")
PROTECTED_BRANCH_RE = re.compile(r"^protected/issue-([1-9][0-9]*)-[a-z0-9][a-z0-9-]{0,80}$")
REFS_RE = re.compile(r"(?m)^Refs #([1-9][0-9]*)\s*$")
REFS_LINE_RE = re.compile(r"(?im)^\s*refs\b.*$")
LEGACY_CLOSING_RE = re.compile(r"(?im)^\s*(?:closes?|fix(?:es)?|resolves?)\s+#([1-9][0-9]*)\s*$")
LEGACY_CLOSING_LINE_RE = re.compile(r"(?im)^\s*(?:closes?|fix(?:es)?|resolves?)\b.*$")
STATE_MARKER_RE = re.compile(r"^<!-- a5\.4a-state:(\{.*\}) -->$")
CI_MARKER_RE = re.compile(r"^<!-- a5\.4a-ci:(\{.*\}) -->$")
REPAIR_MARKER_RE = re.compile(r"^<!-- a5\.4a-repair:(\{.*\}) -->$")
PROTECTED_BLOCKER_EVIDENCE_RE = re.compile(r"^<!-- a5\.4b-protected-blocker:(\{.*\}) -->$")
YELLOW_REPAIR_AUTHORITY_RE = re.compile(r"^<!-- a5\.yellow-repair-authority:(\{.*\}) -->$")
PROTECTED_IMPLEMENTATION_AUTHORIZATION_RE = re.compile(
    r"^<!-- protected-implementation-authorization:(\{.*\}) -->$"
)
AUTOMATED_YELLOW_PRESTART_RE = re.compile(r"^<!-- a5\.yellow-prestart:(\{.*\}) -->$")
AUTOMATED_YELLOW_CLAIM_RE = re.compile(r"^<!-- a5\.yellow-claim:(\{.*\}) -->$")
A5_GITHUB_USER_AGENT = "ml-amstress-a5-review-loop"
REST_OPERATIONS = frozenset((
    "list-open-prs", "get-pr", "get-issue", "get-dependency-issue",
    "list-comments", "list-labels", "create-label", "list-changed-files",
    "set-labels", "create-audit-comment",
))
TRANSPORT_CATEGORIES = frozenset((
    "timeout", "dns", "tls", "proxy", "connection-refused",
    "connection-reset", "connection-unreachable", "other-transport",
))
_CONNECTION_REFUSED_ERRNOS = frozenset((errno.ECONNREFUSED, 10061))
_CONNECTION_RESET_ERRNOS = frozenset((errno.ECONNRESET, errno.ECONNABORTED, 10053, 10054))
_CONNECTION_UNREACHABLE_ERRNOS = frozenset((
    errno.ENETUNREACH, errno.EHOSTUNREACH, errno.ENETDOWN, 10050, 10051, 10064, 10065,
))


class OrchestrationError(Exception):
    """A fail-closed trusted orchestration failure."""


def _proxy_reason(reason: object) -> bool:
    """Identify only urllib's stable proxy/tunnel wording without reporting it."""
    if not isinstance(reason, OSError) or not isinstance(reason.args, tuple):
        return False
    return any(
        isinstance(value, str)
        and any(marker in value.lower() for marker in (
            "tunnel connection failed", "proxy error", "proxy authentication required",
        ))
        for value in reason.args
    )


def classify_transport_failure(error: urllib.error.URLError) -> str:
    """Return an allowlisted category; never return transport exception text."""
    reason = error.reason
    if isinstance(reason, (TimeoutError, socket.timeout)):
        return "timeout"
    if isinstance(reason, socket.gaierror):
        return "dns"
    if isinstance(reason, ssl.SSLError):
        return "tls"
    if _proxy_reason(reason):
        return "proxy"
    if isinstance(reason, ConnectionRefusedError):
        return "connection-refused"
    if isinstance(reason, (ConnectionResetError, ConnectionAbortedError)):
        return "connection-reset"
    code = getattr(reason, "errno", None)
    if code in _CONNECTION_REFUSED_ERRNOS:
        return "connection-refused"
    if code in _CONNECTION_RESET_ERRNOS:
        return "connection-reset"
    if code in _CONNECTION_UNREACHABLE_ERRNOS:
        return "connection-unreachable"
    return "other-transport"


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


def canonical_linked_issue(pr: Mapping[str, Any], require_refs: bool = False) -> int:
    """Return exactly one canonical ``Refs #N`` link, or one unambiguous legacy link."""
    body = _bounded_text(pr.get("body", ""), "PR body")
    references = REFS_RE.findall(body)
    ref_lines = REFS_LINE_RE.findall(body)
    legacy = LEGACY_CLOSING_RE.findall(body)
    legacy_lines = LEGACY_CLOSING_LINE_RE.findall(body)
    if references:
        if len(references) != 1 or len(ref_lines) != 1 or legacy or legacy_lines:
            raise OrchestrationError("PR must link exactly one canonical issue")
        return int(references[0])
    if require_refs or ref_lines or len(legacy) != 1 or len(legacy_lines) != 1:
        raise OrchestrationError("PR must link exactly one canonical issue")
    return int(legacy[0])


def _label_names(item: Mapping[str, Any]) -> tuple[str, ...]:
    raw = item.get("labels", ())
    if not isinstance(raw, Sequence) or isinstance(raw, (str, bytes)):
        raise OrchestrationError("labels are malformed")
    names = tuple(sorted(value.get("name") if isinstance(value, Mapping) else str(value) for value in raw))
    if len(names) != len(set(names)):
        raise OrchestrationError("labels are duplicated")
    return names


def _repository_identity(value: Any, name: str) -> str:
    """Return the only repository identity A5 permits in a PR authorization."""
    if not isinstance(value, Mapping) or value.get("full_name") != REPOSITORY:
        raise OrchestrationError(name + " must be the trusted repository")
    return REPOSITORY


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
    _repository_identity(head.get("repo"), "PR head repository")
    automated_yellow = False
    try:
        yellow_policy.validate_yellow_branch(branch)
        automated_yellow = True
    except yellow_policy.PolicyError:
        pass
    if not isinstance(branch, str) or not (
            BRANCH_RE.fullmatch(branch) or PROTECTED_BRANCH_RE.fullmatch(branch) or automated_yellow):
        raise OrchestrationError("PR branch is not a deterministic Codex worker branch")
    return branch


def review_lane(branch: str) -> str:
    if BRANCH_RE.fullmatch(branch):
        return "green"
    if PROTECTED_BRANCH_RE.fullmatch(branch):
        return "protected-yellow"
    try:
        yellow_policy.validate_yellow_branch(branch)
    except yellow_policy.PolicyError:
        pass
    else:
        return yellow_policy.AUTOMATED_YELLOW_LANE
    raise OrchestrationError("PR branch is not a deterministic Codex worker branch")


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


def _protected_implementation_authorization(comments: Sequence[Mapping[str, Any]], issue_number: int,
                                             branch: str, expected_base_sha: str) -> dict[str, Any]:
    if len(comments) > MAX_COMMENTS:
        raise OrchestrationError("too many issue comments to inspect safely")
    markers = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or len(body) > MAX_AUDIT:
            continue
        value = _marker(PROTECTED_IMPLEMENTATION_AUTHORIZATION_RE, body)
        if value is None:
            continue
        author = comment.get("user") if isinstance(comment, Mapping) else None
        if not isinstance(author, Mapping) or author.get("login") != TRUSTED_MAINTAINER_AUTHOR:
            raise OrchestrationError("protected implementation authorization has an untrusted author")
        markers.append(value)
    if len(markers) != 1:
        raise OrchestrationError("protected implementation authorization is missing or ambiguous")
    marker = markers[0]
    if set(marker) != {"base_sha", "branch", "issue_number", "schema_version", "scope"}:
        raise OrchestrationError("protected implementation authorization is malformed")
    if marker.get("schema_version") != 1 or marker.get("scope") != "implementation-only":
        raise OrchestrationError("protected implementation authorization has an unsupported scope")
    if _sha(marker.get("base_sha"), "protected authorization base SHA") != expected_base_sha:
        raise OrchestrationError("protected implementation authorization base SHA does not match")
    if marker.get("issue_number") != issue_number:
        raise OrchestrationError("protected implementation authorization issue does not match")
    if marker.get("branch") != branch:
        raise OrchestrationError("protected implementation authorization branch does not match")
    return marker


def _automated_yellow_authorization(comments: Sequence[Mapping[str, Any]], issue_number: int,
                                    branch: str, expected_base_sha: str) -> dict[str, str]:
    """Return one canonical trusted #147 pre-start/claim evidence pair."""
    if len(comments) > MAX_COMMENTS:
        raise OrchestrationError("too many issue comments to inspect safely")
    serialized: dict[str, list[str]] = {"prestart": [], "claim": []}
    patterns = (
        ("prestart", AUTOMATED_YELLOW_PRESTART_RE),
        ("claim", AUTOMATED_YELLOW_CLAIM_RE),
    )
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str):
            continue
        stripped = body.strip()
        for name, pattern in patterns:
            prefix = "<!-- a5.yellow-%s:" % name
            if not stripped.startswith(prefix):
                continue
            if len(body) > MAX_AUDIT or body != stripped:
                raise OrchestrationError("automated YELLOW authorization is invalid")
            match = pattern.fullmatch(stripped)
            if match is None:
                raise OrchestrationError("automated YELLOW authorization is invalid")
            author = comment.get("user") if isinstance(comment, Mapping) else None
            if not isinstance(author, Mapping) or author.get("login") != TRUSTED_AUDIT_AUTHOR:
                raise OrchestrationError("automated YELLOW authorization has an untrusted author")
            serialized[name].append(match.group(1))
    if len(serialized["prestart"]) != 1 or len(serialized["claim"]) != 1:
        raise OrchestrationError("automated YELLOW authorization is missing or ambiguous")
    try:
        prestart = yellow_policy.parse_prestart(
            serialized["prestart"][0], expected_repository=REPOSITORY,
            expected_issue=issue_number, expected_base=expected_base_sha,
        )
        claim = yellow_policy.parse_claim(
            serialized["claim"][0], expected_repository=REPOSITORY,
            expected_issue=issue_number, expected_base=expected_base_sha,
            expected_branch=branch,
        )
        canonical_prestart = yellow_policy.serialize_prestart(
            prestart, expected_repository=REPOSITORY,
            expected_issue=issue_number, expected_base=expected_base_sha,
        )
        canonical_claim = yellow_policy.serialize_claim(
            claim, expected_repository=REPOSITORY,
            expected_issue=issue_number, expected_base=expected_base_sha,
            expected_branch=branch,
        )
    except yellow_policy.PolicyError:
        raise OrchestrationError("automated YELLOW authorization is invalid") from None
    if (serialized["prestart"][0] != canonical_prestart
            or serialized["claim"][0] != canonical_claim):
        raise OrchestrationError("automated YELLOW authorization is not canonical")
    return {"prestart": canonical_prestart, "claim": canonical_claim}


def _automated_yellow_authorized_paths(comments: Sequence[Mapping[str, Any]], issue_number: int,
                                       branch: str, expected_base_sha: str) -> tuple[str, ...]:
    authorization = _automated_yellow_authorization(
        comments, issue_number, branch, expected_base_sha
    )
    try:
        prestart = yellow_policy.parse_prestart(
            authorization["prestart"], expected_repository=REPOSITORY,
            expected_issue=issue_number, expected_base=expected_base_sha,
        )
    except yellow_policy.PolicyError:
        raise OrchestrationError("automated YELLOW authorization is invalid") from None
    return prestart.authorized_paths


def validate_issue_identity(issue: Mapping[str, Any], branch: str, issue_number: int,
                            authorization_comments: Sequence[Mapping[str, Any]] = (),
                            authorization_base_sha: str | None = None) -> green_worker.Contract:
    if issue.get("number") != issue_number or str(issue.get("state", "")).lower() != "open":
        raise OrchestrationError("linked issue is not the exact open issue")
    labels = _label_names(issue)
    if len([name for name in labels if name.startswith("risk:")]) != 1:
        raise OrchestrationError("linked issue has ambiguous risk labels")
    status = _one_label(labels, "status:")
    if status not in state_contract.ISSUE_STATUSES:
        raise OrchestrationError("linked issue has an unsupported implementation status")
    contract = green_worker.parse_contract(_bounded_text(issue.get("body", ""), "issue body"), REPOSITORY)
    lane = review_lane(branch)
    if lane == "green":
        if "risk:green" not in labels or "agent:codex" not in labels:
            raise OrchestrationError("linked issue is not eligible GREEN Codex work")
        if contract.risk != "risk:green":
            raise OrchestrationError("linked issue contract is not GREEN-only")
        expected = green_worker.deterministic_branch_name(issue_number, _bounded_text(issue.get("title", ""), "issue title", 300))
        if branch != expected:
            raise OrchestrationError("PR branch does not match the deterministic issue branch")
        return contract
    if lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        try:
            yellow_policy.validate_yellow_branch(branch, issue_number)
            expected = yellow_policy.yellow_branch(
                issue_number, _bounded_text(issue.get("title", ""), "issue title", 300)
            )
        except yellow_policy.PolicyError:
            raise OrchestrationError("automated YELLOW PR branch does not match the linked issue") from None
        if branch != expected:
            raise OrchestrationError("automated YELLOW PR branch does not match the deterministic issue branch")
        if "risk:yellow" not in labels or contract.risk != "risk:yellow":
            raise OrchestrationError("linked issue is not eligible automated YELLOW work")
        if authorization_base_sha is None:
            raise OrchestrationError("automated YELLOW authorization base SHA is unavailable")
        _automated_yellow_authorization(
            authorization_comments, issue_number, branch, authorization_base_sha
        )
        return contract
    protected_match = PROTECTED_BRANCH_RE.fullmatch(branch)
    if protected_match is None or int(protected_match.group(1)) != issue_number:
        raise OrchestrationError("protected PR branch does not match the linked issue")
    if "risk:yellow" not in labels or contract.risk != "risk:yellow":
        raise OrchestrationError("linked issue is not eligible protected YELLOW work")
    if authorization_base_sha is None:
        raise OrchestrationError("protected implementation authorization base SHA is unavailable")
    _protected_implementation_authorization(authorization_comments, issue_number, branch, authorization_base_sha)
    return contract


def validate_dependencies(client: Any, contract: green_worker.Contract) -> None:
    for dependency in contract.dependencies:
        evidence = client.dependency_issue(dependency)
        if not isinstance(evidence, Mapping) or str(evidence.get("state", "")).lower() != "closed":
            raise OrchestrationError("issue dependency is not satisfied")


def authorization_fingerprint(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any], run: WorkflowRun,
                              authorization_comments: Sequence[Mapping[str, Any]] = ()) -> str:
    """Bind a reviewer result to canonical, security-relevant authorization evidence."""
    pr_number, branch = validate_pr_identity(pr, run)
    lane = review_lane(branch)
    issue_number = canonical_linked_issue(pr, require_refs=lane != "green")
    base, head = pr.get("base"), pr.get("head")
    if not isinstance(base, Mapping) or not isinstance(head, Mapping):
        raise OrchestrationError("PR base or head is malformed")
    base_sha = _sha(base.get("sha"), "PR base")
    base_ref = _bounded_text(base.get("ref"), "PR base ref", 300)
    if base_ref != BASE_BRANCH:
        raise OrchestrationError("PR must target main with a valid head")
    base_repository = _repository_identity(base.get("repo"), "PR base repository")
    head_repository = _repository_identity(head.get("repo"), "PR head repository")
    contract = validate_issue_identity(issue, branch, issue_number, authorization_comments, base_sha)
    dependency_states = []
    for dependency in contract.dependencies:
        evidence = client.dependency_issue(dependency)
        if not isinstance(evidence, Mapping) or str(evidence.get("state", "")).lower() != "closed":
            raise OrchestrationError("issue dependency is not satisfied")
        dependency_states.append({"repository": dependency.repository, "number": dependency.number, "state": "closed"})
    identity = {
        "repository": REPOSITORY,
        "pr": {
            "number": pr_number,
            "title": _bounded_text(pr.get("title"), "PR title", 300),
            "body": _bounded_text(pr.get("body"), "PR body"),
            "labels": _label_names(pr),
        },
        "base": {"repository": base_repository, "ref": base_ref, "sha": base_sha},
        "head": {"repository": head_repository, "branch": branch, "sha": _sha(head.get("sha"), "PR head")},
        "issue": {
            "number": issue_number,
            "title": _bounded_text(issue.get("title"), "issue title", 300),
            "body": _bounded_text(issue.get("body"), "issue body"),
            "labels": _label_names(issue),
        },
        "dependencies": dependency_states,
    }
    if lane == "protected-yellow":
        marker = _protected_implementation_authorization(
            authorization_comments, issue_number, branch, base_sha
        )
        identity["protected_implementation_authorization"] = {
            key: marker[key] for key in ("base_sha", "branch", "issue_number", "schema_version", "scope")
        }
    elif lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        identity["automated_yellow_authorization"] = _automated_yellow_authorization(
            authorization_comments, issue_number, branch, base_sha
        )
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
                   files: Sequence[Mapping[str, Any]], lane: str = "green",
                   authorized_paths: Sequence[str] = ()) -> tuple[dict[str, Any], tuple[str, ...]]:
    if len(files) == 0 or len(files) > MAX_CHANGED_FILES:
        raise OrchestrationError("changed-file evidence is missing or exceeds the safe bound")
    changed = []
    for item in files:
        if not isinstance(item, Mapping) or not isinstance(item.get("filename"), str) or not isinstance(item.get("patch"), str):
            raise OrchestrationError("changed-file patch evidence is incomplete")
        changed.append(reviewer.ChangedFile(item["filename"], item["patch"]))
    if lane == "green":
        paths = _trusted_green_paths(changed)
    elif lane == "protected-yellow":
        paths = tuple(item.path for item in changed)
    elif lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        paths = tuple(item.path for item in changed)
        allowed = tuple(authorized_paths)
        allowed_set = set(allowed)
        if (not paths or len(paths) != len(set(paths)) or not allowed
                or len(allowed) != len(allowed_set)
                or any(path not in allowed_set for path in paths)):
            raise OrchestrationError("automated YELLOW changed paths exceed the authorized scope")
    else:
        raise OrchestrationError("review lane is unsupported")
    head = _sha(pr["head"].get("sha"), "PR head")
    snapshot = {"schema_version": 1, "repository": REPOSITORY, "pull_request_number": pr["number"],
                "issue_number": issue["number"], "base_sha": _sha(pr["base"].get("sha"), "PR base"),
                "head_sha": head, "pr_title": _bounded_text(pr.get("title", ""), "PR title"),
                "pr_body": _bounded_text(pr.get("body", ""), "PR body"),
                "issue_title": _bounded_text(issue.get("title", ""), "issue title"),
                "issue_body": _bounded_text(issue.get("body", ""), "issue body"),
                "issue_labels": list(_label_names(issue)), "declared_risk": "yellow" if lane != "green" else "green",
                "trusted_risk_floor": "yellow" if lane != "green" else "green",
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


def _yellow_repair_marker(pr_number: int, issue_number: int, head: str, decision_key: str,
                          authorization_key: str, blocker_evidence_key: str, attempt: int,
                          finding_ids: Sequence[str]) -> str:
    """Use the shared attempt stream while binding the YELLOW-only authority."""
    payload = {
        "schema_version": 1, "lane": yellow_policy.AUTOMATED_YELLOW_LANE,
        "pr_number": pr_number, "issue_number": issue_number, "head_sha": head,
        "decision_key": decision_key, "authorization_key": authorization_key,
        "blocker_evidence_key": blocker_evidence_key, "attempt": attempt,
        "finding_ids": list(finding_ids),
    }
    result = "<!-- a5.4a-repair:" + json.dumps(payload, sort_keys=True, separators=(",", ":")) + " -->"
    if len(result) > MAX_AUDIT:
        raise OrchestrationError("YELLOW repair audit is unexpectedly unbounded")
    return result


def _audit_safe_finding_text(value: Any, name: str) -> str:
    """Accept only parser-valid text that is safe to place in a trusted audit."""
    try:
        text = reviewer._text(value, name, 1000)
    except reviewer.ReviewError:
        raise OrchestrationError("protected blocker evidence has an unsafe finding field") from None
    unsafe_patterns = (
        reviewer.LOCAL_ABSOLUTE_PATH_RE, reviewer.APP_TOKEN_ASSIGNMENT_RE,
        reviewer.GITHUB_PAT_RE, reviewer.QUOTED_AUTHORIZATION_VALUE_RE,
        green_worker.AUTHORIZATION_VALUE_RE, green_worker.TOKEN_ASSIGNMENT_RE,
        green_worker.OAUTH_TOKEN_ASSIGNMENT_RE, green_worker.JWT_LIKE_TOKEN_RE,
        green_worker.COOKIE_VALUE_RE, green_worker.COMMON_API_KEY_RE,
    )
    if (any(marker in text for marker in reviewer.REVIEWER_PROMPT_MARKERS)
            or any(pattern.search(text) for pattern in unsafe_patterns)):
        raise OrchestrationError("protected blocker evidence has an unsafe finding field")
    if any(os.environ.get(name) and os.environ[name] in text for name in (
        "GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN",
    )):
        raise OrchestrationError("protected blocker evidence has an unsafe finding field")
    return text


def _protected_blocker_payload(pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                               verdict: reviewer.ReviewVerdict) -> dict[str, Any]:
    """Freeze only strict reviewer fields for protected-lane maintainer evidence."""
    if (not isinstance(verdict, reviewer.ReviewVerdict)
            or verdict.schema_version != reviewer.VERDICT_SCHEMA_VERSION
            or verdict.verdict != "blocker" or verdict.effective_risk != "yellow"
            or verdict.reviewed_head_sha != head or _sha(pr.get("head", {}).get("sha"), "PR head") != head):
        raise OrchestrationError("protected blocker evidence is not a strictly validated YELLOW blocker")
    pr_number, issue_number = pr.get("number"), issue.get("number")
    if (isinstance(pr_number, bool) or not isinstance(pr_number, int) or pr_number < 1
            or isinstance(issue_number, bool) or not isinstance(issue_number, int) or issue_number < 1):
        raise OrchestrationError("protected blocker evidence has invalid identity")
    if not isinstance(verdict.findings, tuple) or not 1 <= len(verdict.findings) <= MAX_PROTECTED_BLOCKER_FINDINGS:
        raise OrchestrationError("protected blocker evidence findings are unbounded")
    findings, identifiers = [], set()
    for finding in verdict.findings:
        if (not isinstance(finding, reviewer.Finding) or not isinstance(finding.id, str)
                or not reviewer.FINDING_ID_RE.fullmatch(finding.id) or finding.id in identifiers
                or finding.category not in reviewer.FINDING_CATEGORIES):
            raise OrchestrationError("protected blocker evidence has an invalid finding")
        identifiers.add(finding.id)
        findings.append({
            "id": finding.id,
            "message": _audit_safe_finding_text(finding.message, "finding message"),
            "required_action": _audit_safe_finding_text(finding.required_action, "finding required_action"),
            "required_evidence": _audit_safe_finding_text(finding.required_evidence, "finding required_evidence"),
        })
    return {
        "schema_version": 1,
        "repository": REPOSITORY,
        "issue_number": issue_number,
        "pr_number": pr_number,
        "reviewed_head_sha": head,
        "effective_risk": "yellow",
        "verdict": "blocker",
        "findings": findings,
    }


def _protected_blocker_marker(payload: Mapping[str, Any]) -> str:
    expected = {
        "schema_version", "repository", "issue_number", "pr_number", "reviewed_head_sha",
        "effective_risk", "verdict", "findings",
    }
    if not isinstance(payload, Mapping) or set(payload) != expected:
        raise OrchestrationError("protected blocker evidence is malformed")
    if (payload.get("schema_version") != 1 or payload.get("repository") != REPOSITORY
            or payload.get("effective_risk") != "yellow" or payload.get("verdict") != "blocker"):
        raise OrchestrationError("protected blocker evidence is malformed")
    _sha(payload.get("reviewed_head_sha"), "protected blocker evidence head")
    if (isinstance(payload.get("issue_number"), bool) or not isinstance(payload.get("issue_number"), int)
            or payload["issue_number"] < 1 or isinstance(payload.get("pr_number"), bool)
            or not isinstance(payload.get("pr_number"), int) or payload["pr_number"] < 1):
        raise OrchestrationError("protected blocker evidence is malformed")
    findings = payload.get("findings")
    if not isinstance(findings, list) or not 1 <= len(findings) <= MAX_PROTECTED_BLOCKER_FINDINGS:
        raise OrchestrationError("protected blocker evidence findings are unbounded")
    normalized, identifiers = [], set()
    for finding in findings:
        if not isinstance(finding, Mapping) or set(finding) != {"id", "message", "required_action", "required_evidence"}:
            raise OrchestrationError("protected blocker evidence has an invalid finding")
        identifier = finding.get("id")
        if not isinstance(identifier, str) or not reviewer.FINDING_ID_RE.fullmatch(identifier) or identifier in identifiers:
            raise OrchestrationError("protected blocker evidence has an invalid finding")
        identifiers.add(identifier)
        normalized.append({
            "id": identifier,
            "message": _audit_safe_finding_text(finding.get("message"), "finding message"),
            "required_action": _audit_safe_finding_text(finding.get("required_action"), "finding required_action"),
            "required_evidence": _audit_safe_finding_text(finding.get("required_evidence"), "finding required_evidence"),
        })
    canonical = dict(payload)
    canonical["findings"] = normalized
    result = "<!-- a5.4b-protected-blocker:" + json.dumps(canonical, sort_keys=True, separators=(",", ":")) + " -->"
    if len(result) > MAX_AUDIT:
        raise OrchestrationError("protected blocker evidence is oversized")
    return result


def _protected_blocker_markers(comments: Sequence[Mapping[str, Any]]) -> list[dict[str, Any]]:
    if len(comments) > MAX_COMMENTS:
        raise OrchestrationError("too many comments to inspect safely")
    markers = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or len(body) > MAX_AUDIT or not _trusted_comment(comment):
            continue
        payload = _marker(PROTECTED_BLOCKER_EVIDENCE_RE, body)
        if payload is None:
            continue
        if _protected_blocker_marker(payload) != body.strip():
            raise OrchestrationError("protected blocker evidence is not deterministic")
        markers.append(payload)
    return markers


def persist_protected_blocker_evidence(client: Any, comments: Sequence[Mapping[str, Any]],
                                       pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                                       verdict: reviewer.ReviewVerdict) -> None:
    """Persist one exact-head protected blocker audit, never an authorization."""
    payload = _protected_blocker_payload(pr, issue, head, verdict)
    marker = _protected_blocker_marker(payload)
    same_identity = []
    for existing in _protected_blocker_markers(comments):
        if (existing["repository"] == REPOSITORY and existing["issue_number"] == issue["number"]
                and existing["pr_number"] == pr["number"] and existing["reviewed_head_sha"] == head):
            same_identity.append(existing)
    if len(same_identity) > 1:
        raise OrchestrationError("protected blocker evidence is ambiguous")
    if same_identity:
        if _protected_blocker_marker(same_identity[0]) != marker:
            raise OrchestrationError("protected blocker evidence conflicts with the validated verdict")
        return
    client.comment(pr["number"], marker)


def accepted_protected_blocker_evidence(comments: Sequence[Mapping[str, Any]],
                                        pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                                        verdict: reviewer.ReviewVerdict) -> str:
    """Return the one canonical, refetched #143 marker matching the verdict."""
    expected = _protected_blocker_marker(_protected_blocker_payload(pr, issue, head, verdict))
    matches = []
    for payload in _protected_blocker_markers(comments):
        if (payload["repository"] == REPOSITORY and payload["pr_number"] == pr["number"]
                and payload["issue_number"] == issue["number"]
                and payload["reviewed_head_sha"] == head):
            matches.append(_protected_blocker_marker(payload))
    if len(matches) != 1 or matches[0] != expected:
        raise OrchestrationError("accepted protected blocker evidence is missing, ambiguous, or conflicting")
    return matches[0]


def _yellow_repair_authority_payload(pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                                     verdict: reviewer.ReviewVerdict) -> dict[str, Any]:
    payload = _protected_blocker_payload(pr, issue, head, verdict)
    payload["findings"] = [dict(item, category=finding.category)
                           for item, finding in zip(payload["findings"], verdict.findings)]
    return payload


def _yellow_repair_authority_marker(payload: Mapping[str, Any]) -> str:
    expected = {
        "schema_version", "repository", "issue_number", "pr_number", "reviewed_head_sha",
        "effective_risk", "verdict", "findings",
    }
    if not isinstance(payload, Mapping) or set(payload) != expected:
        raise OrchestrationError("YELLOW repair authority is malformed")
    findings = payload.get("findings")
    if not isinstance(findings, list) or not 1 <= len(findings) <= MAX_PROTECTED_BLOCKER_FINDINGS:
        raise OrchestrationError("YELLOW repair authority findings are unbounded")
    audit_findings, categories = [], []
    for finding in findings:
        keys = {"id", "category", "message", "required_action", "required_evidence"}
        if not isinstance(finding, Mapping) or set(finding) != keys:
            raise OrchestrationError("YELLOW repair authority has an invalid finding")
        category = finding.get("category")
        if category not in reviewer.FINDING_CATEGORIES:
            raise OrchestrationError("YELLOW repair authority has an invalid finding category")
        categories.append(category)
        audit_findings.append({key: finding[key] for key in (
            "id", "message", "required_action", "required_evidence",
        )})
    audit_payload = dict(payload)
    audit_payload["findings"] = audit_findings
    # Reuse the reviewed #143 canonical validator for identity and safe text.
    _protected_blocker_marker(audit_payload)
    canonical = dict(payload)
    canonical["findings"] = [dict(item, category=category)
                              for item, category in zip(audit_findings, categories)]
    marker = "<!-- a5.yellow-repair-authority:" + json.dumps(
        canonical, sort_keys=True, separators=(",", ":")
    ) + " -->"
    if len(marker) > MAX_AUDIT:
        raise OrchestrationError("YELLOW repair authority is oversized")
    return marker


def _yellow_repair_authority_markers(comments: Sequence[Mapping[str, Any]]) -> list[dict[str, Any]]:
    if len(comments) > MAX_COMMENTS:
        raise OrchestrationError("too many comments to inspect safely")
    markers = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or len(body) > MAX_AUDIT or not _trusted_comment(comment):
            continue
        payload = _marker(YELLOW_REPAIR_AUTHORITY_RE, body)
        if payload is None:
            continue
        if _yellow_repair_authority_marker(payload) != body.strip():
            raise OrchestrationError("YELLOW repair authority is not deterministic")
        markers.append(payload)
    return markers


def persist_yellow_repair_authority(client: Any, comments: Sequence[Mapping[str, Any]],
                                    pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                                    verdict: reviewer.ReviewVerdict) -> None:
    """Persist one category-complete finding identity before repair evaluation."""
    payload = _yellow_repair_authority_payload(pr, issue, head, verdict)
    marker = _yellow_repair_authority_marker(payload)
    matching = [item for item in _yellow_repair_authority_markers(comments)
                if item["repository"] == REPOSITORY and item["pr_number"] == pr["number"]
                and item["issue_number"] == issue["number"] and item["reviewed_head_sha"] == head]
    if len(matching) > 1:
        raise OrchestrationError("YELLOW repair authority is ambiguous")
    if matching:
        if _yellow_repair_authority_marker(matching[0]) != marker:
            raise OrchestrationError("YELLOW repair authority conflicts with the validated verdict")
        return
    client.comment(pr["number"], marker)


def accepted_yellow_repair_authority(comments: Sequence[Mapping[str, Any]],
                                     pr: Mapping[str, Any], issue: Mapping[str, Any], head: str,
                                     verdict: reviewer.ReviewVerdict) -> str:
    expected = _yellow_repair_authority_marker(
        _yellow_repair_authority_payload(pr, issue, head, verdict)
    )
    matching = [_yellow_repair_authority_marker(item)
                for item in _yellow_repair_authority_markers(comments)
                if item["repository"] == REPOSITORY and item["pr_number"] == pr["number"]
                and item["issue_number"] == issue["number"] and item["reviewed_head_sha"] == head]
    if len(matching) != 1 or matching[0] != expected:
        raise OrchestrationError("accepted YELLOW repair authority is missing, ambiguous, or conflicting")
    return matching[0]


def _repair_failure_marker(attempt: int, error: Exception | None = None) -> str:
    payload: dict[str, Any] = {"schema_version": 1, "attempt": attempt, "category": "trusted-repair-failed"}
    detail = repair.audit_safe_error_detail(error) if error is not None else None
    if detail is not None:
        payload["detail"] = detail
    result = "<!-- a5.4a-repair-failed:" + json.dumps(payload, sort_keys=True, separators=(",", ":")) + " -->"
    if len(result) > MAX_AUDIT:
        raise OrchestrationError("repair failure audit is unexpectedly unbounded")
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
    lane = review_lane(branch)
    authorization_comments = client.comments(issue_number) if lane != "green" else ()
    if observed_pr_number != pr_number or _sha(pr.get("head", {}).get("sha"), "re-fetched PR head") != head:
        raise OrchestrationError("PR head changed after reviewer evidence")
    if canonical_linked_issue(pr, require_refs=lane != "green") != issue_number:
        raise OrchestrationError("PR issue link changed after reviewer evidence")
    contract = validate_issue_identity(
        issue, branch, issue_number, authorization_comments, _sha(pr.get("base", {}).get("sha"), "re-fetched PR base")
    )
    validate_dependencies(client, contract)
    if authorization_fingerprint(client, pr, issue, run, authorization_comments) != authorization:
        raise OrchestrationError("authorization evidence changed after reviewer execution")
    if lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        authorized_paths = _automated_yellow_authorized_paths(
            authorization_comments, issue_number, branch,
            _sha(pr.get("base", {}).get("sha"), "re-fetched PR base"),
        )
        build_snapshot(
            pr, issue, run, client.changed_files(pr_number), lane, authorized_paths
        )
    if current_review_state(pr, issue, comments) != prior:
        raise OrchestrationError("review state changed after reviewer evidence")
    return pr, issue, comments


def _revalidate_repair_authorization(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any],
                                     comments: Sequence[Mapping[str, Any]], run: WorkflowRun,
                                     issue_number: int) -> CurrentReviewState:
    """Re-check trusted scope after applying blocker state and before A5.3."""
    pr_number, branch = validate_pr_identity(pr, run)
    if review_lane(branch) != "green":
        raise OrchestrationError("protected YELLOW review cannot authorize automatic repair")
    if canonical_linked_issue(pr) != issue_number:
        raise OrchestrationError("PR issue link changed before repair")
    contract = validate_issue_identity(issue, branch, issue_number)
    validate_dependencies(client, contract)
    current = current_review_state(pr, issue, comments)
    if (pr_number < 1 or current.issue_status != "status:in-progress"
            or current.review_label != "review:blocker" or current.review_head_sha != run.head_sha):
        raise OrchestrationError("blocker repair authorization no longer matches the exact head")
    return current


def _revalidate_yellow_repair_authorization(
        client: Any, pr_number: int, issue_number: int, run: WorkflowRun,
        verdict: reviewer.ReviewVerdict) -> tuple[Mapping[str, Any], Mapping[str, Any],
                                                  list[Mapping[str, Any]], CurrentReviewState,
                                                  tuple[str, ...], str, str]:
    """Re-fetch every automated-YELLOW authority immediately before repair."""
    pr, issue = client.pr(pr_number), client.issue(issue_number)
    comments, authorization_comments = client.comments(pr_number), client.comments(issue_number)
    observed_pr_number, branch = validate_pr_identity(pr, run)
    if observed_pr_number != pr_number or review_lane(branch) != yellow_policy.AUTOMATED_YELLOW_LANE:
        raise OrchestrationError("repair lane is not the exact automated YELLOW PR")
    if canonical_linked_issue(pr, require_refs=True) != issue_number:
        raise OrchestrationError("PR issue link changed before YELLOW repair")
    base_sha = _sha(pr.get("base", {}).get("sha"), "PR base")
    contract = validate_issue_identity(issue, branch, issue_number, authorization_comments, base_sha)
    validate_dependencies(client, contract)
    authorization = _automated_yellow_authorization(
        authorization_comments, issue_number, branch, base_sha
    )
    paths = _automated_yellow_authorized_paths(
        authorization_comments, issue_number, branch, base_sha
    )
    snapshot, _ = build_snapshot(
        pr, issue, run, client.changed_files(pr_number), yellow_policy.AUTOMATED_YELLOW_LANE, paths
    )
    trusted_snapshot = reviewer.validate_snapshot(snapshot)
    reviewer.validate_external_requirements(trusted_snapshot)
    reviewer.validate_repairable_findings(trusted_snapshot, verdict)
    current = current_review_state(pr, issue, comments)
    if (current.issue_status != "status:in-progress" or current.review_label != "review:blocker"
            or current.review_head_sha != run.head_sha):
        raise OrchestrationError("YELLOW blocker repair authorization no longer matches the exact head")
    blocker_marker = accepted_protected_blocker_evidence(comments, pr, issue, run.head_sha, verdict)
    repair_authority = accepted_yellow_repair_authority(comments, pr, issue, run.head_sha, verdict)
    authorization_text = json.dumps(authorization, sort_keys=True, separators=(",", ":"))
    return (pr, issue, comments, current, paths,
            yellow_repair.evidence_key("authorization", authorization_text),
            yellow_repair.evidence_key("blocker", json.dumps(
                {"audit": blocker_marker, "repair_authority": repair_authority},
                sort_keys=True, separators=(",", ":"),
            )))


def _repair(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any], comments: Sequence[Mapping[str, Any]],
            current: CurrentReviewState, verdict: reviewer.ReviewVerdict, accepted_blocker_key: str,
            paths: tuple[str, ...], cwd: str) -> str:
    if review_lane(_pr_branch(pr)) != "green":
        raise OrchestrationError("protected YELLOW review cannot invoke automatic repair")
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
    except repair.RepairError as error:
        failed = _repair_failure_marker(attempt, error)
        if failed not in [item.get("body") for item in client.comments(pr["number"]) if _trusted_comment(item)]:
            client.comment(pr["number"], failed)
        return "repair-failed"
    except Exception:
        failed = _repair_failure_marker(attempt)
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


def _yellow_repair(client: Any, pr: Mapping[str, Any], issue: Mapping[str, Any],
                   comments: Sequence[Mapping[str, Any]], current: CurrentReviewState,
                   verdict: reviewer.ReviewVerdict, accepted_blocker_key: str,
                   authorization_key: str, blocker_evidence_key: str,
                   paths: tuple[str, ...], cwd: str) -> str:
    """Execute one distinct automated-YELLOW repair using the shared history."""
    if review_lane(_pr_branch(pr)) != yellow_policy.AUTOMATED_YELLOW_LANE:
        raise OrchestrationError("only automated YELLOW may invoke YELLOW repair")
    attempts = repair_attempt_count(comments, pr["number"])
    if attempts >= MAX_REPAIR_ATTEMPTS:
        exhausted = "<!-- a5.4a-repair-exhausted:{\"schema_version\":1,\"pr_number\":%d} -->" % pr["number"]
        if exhausted not in [item.get("body") for item in comments if _trusted_comment(item)]:
            client.comment(pr["number"], exhausted)
        return "repair-exhausted"
    attempt = attempts + 1
    request = yellow_repair.YellowRepairRequest(
        1, REPOSITORY, pr["number"], issue["number"], _pr_branch(pr), pr["head"]["sha"],
        accepted_blocker_key, blocker_evidence_key, authorization_key,
        current.issue_status, current.review_label, current.review_head_sha, "yellow",
        tuple(repair.BlockerFinding(item.id, item.category, item.message, item.required_action,
                                    item.required_evidence) for item in verdict.findings),
        paths, attempt,
    )
    yellow_repair.validate_request(request)
    client.comment(pr["number"], _yellow_repair_marker(
        pr["number"], issue["number"], pr["head"]["sha"], accepted_blocker_key,
        authorization_key, blocker_evidence_key, attempt,
        tuple(item.id for item in verdict.findings),
    ))
    try:
        checkout_exact_pr_branch(request.branch, request.expected_head_sha, cwd)
        result = yellow_repair.execute_repair(request, cwd)
    except repair.RepairError as error:
        failed = _repair_failure_marker(attempt, error)
        if failed not in [item.get("body") for item in client.comments(pr["number"]) if _trusted_comment(item)]:
            client.comment(pr["number"], failed)
        return "repair-failed"
    except Exception:
        failed = _repair_failure_marker(attempt)
        if failed not in [item.get("body") for item in client.comments(pr["number"]) if _trusted_comment(item)]:
            client.comment(pr["number"], failed)
        return "repair-failed"
    refreshed = client.pr(pr["number"])
    if _sha(refreshed.get("head", {}).get("sha"), "repaired PR head") != result.new_head_sha:
        raise OrchestrationError("repair push head could not be verified")
    refreshed_issue, refreshed_comments = client.issue(issue["number"]), client.comments(pr["number"])
    refreshed_state = current_review_state(refreshed, refreshed_issue, refreshed_comments)
    plan_input = _state_input(pr["number"], issue["number"], result.new_head_sha, refreshed_state, "new_head")
    apply_transition(client, refreshed, refreshed_issue, refreshed_comments,
                     plan_input, state_contract.transition(plan_input))
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
    lane = review_lane(branch)
    issue_number = canonical_linked_issue(pr, require_refs=lane != "green")
    issue, comments = client.issue(issue_number), client.comments(pr_number)
    authorization_comments = client.comments(issue_number) if lane != "green" else ()
    contract = validate_issue_identity(
        issue, branch, issue_number, authorization_comments, _sha(pr.get("base", {}).get("sha"), "PR base")
    )
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

    authorized_paths: Sequence[str] = ()
    if lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        authorized_paths = _automated_yellow_authorized_paths(
            authorization_comments, issue_number, branch,
            _sha(pr.get("base", {}).get("sha"), "PR base"),
        )
    snapshot, paths = build_snapshot(
        pr, issue, run, client.changed_files(pr_number), lane, authorized_paths
    )
    trusted_snapshot = reviewer.validate_snapshot(snapshot)
    reviewer.validate_external_requirements(trusted_snapshot)
    authorization = authorization_fingerprint(client, pr, issue, run, authorization_comments)
    if current.review_label not in ("review:pending", "review:blocker") or current.review_head_sha != run.head_sha:
        raise OrchestrationError("current review state is not pending for the exact CI head")
    verdict = review_runner(snapshot, cwd)
    if not isinstance(verdict, reviewer.ReviewVerdict):
        raise OrchestrationError("reviewer returned an invalid verdict object")
    if lane == "green":
        try:
            reviewer.validate_repairable_findings(trusted_snapshot, verdict)
        except reviewer.ReviewError as error:
            raise OrchestrationError("reviewer repair boundary rejected the verdict") from error
        if verdict.effective_risk != "green" and verdict.verdict != "escalate":
            raise OrchestrationError("non-GREEN reviewer risk must not advance or repair automatically")
    elif verdict.verdict == "blocker" and lane == "protected-yellow":
        pr, issue, comments = _refetch_unchanged(
            client, pr_number, issue_number, run.head_sha, run, current, authorization
        )
        persist_protected_blocker_evidence(client, comments, pr, issue, run.head_sha, verdict)
        raise OrchestrationError("protected YELLOW review cannot authorize automatic repair")
    elif verdict.verdict == "blocker" and lane == yellow_policy.AUTOMATED_YELLOW_LANE:
        pr, issue, comments = _refetch_unchanged(
            client, pr_number, issue_number, run.head_sha, run, current, authorization
        )
        if current.review_label == "review:pending":
            # Persist category-complete authority first. A partial/crashed write
            # can then never be reclassified from the category-less #143 audit.
            persist_yellow_repair_authority(client, comments, pr, issue, run.head_sha, verdict)
            persist_protected_blocker_evidence(
                client, client.comments(pr_number), pr, issue, run.head_sha, verdict
            )
        pr, issue, comments = client.pr(pr_number), client.issue(issue_number), client.comments(pr_number)
        accepted_protected_blocker_evidence(comments, pr, issue, run.head_sha, verdict)
        accepted_yellow_repair_authority(comments, pr, issue, run.head_sha, verdict)
        try:
            reviewer.validate_repairable_findings(trusted_snapshot, verdict)
        except reviewer.ReviewError as error:
            raise OrchestrationError("reviewer repair boundary rejected the YELLOW verdict") from error
        transition_input = _state_input(pr_number, issue_number, run.head_sha, current, "verdict", verdict)
        plan = state_contract.transition(transition_input)
        apply_transition(client, pr, issue, comments, transition_input, plan)
        (pr, issue, comments, current, paths, authorization_key,
         blocker_evidence_key) = _revalidate_yellow_repair_authorization(
            client, pr_number, issue_number, run, verdict
        )
        accepted_key = plan.decision_key if not plan.idempotent_no_op else accepted_blocker_decision_key(
            comments, pr_number, issue_number, run.head_sha
        )
        return _yellow_repair(
            client, pr, issue, comments, current, verdict, accepted_key,
            authorization_key, blocker_evidence_key, paths, cwd,
        )
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
        except urllib.error.URLError as error:
            category = classify_transport_failure(error)
            raise OrchestrationError("GitHub %s: transport %s" % (operation, category)) from None
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
