"""Trusted, GitHub-native terminal observer for the GREEN Codex worker.

This module deliberately has no Codex, repair, merge, or label-mutation path.
It resolves an issue only through the existing trusted worker claim marker and
records one bounded completion observation for an exact workflow run.
"""
from __future__ import annotations

import dataclasses
import hashlib
import json
import os
import re
import sys
import urllib.error
import urllib.request
from typing import Any, Mapping, Sequence


REPOSITORY = "RayZhang2024/ML-AMstress"
BASE_BRANCH = "main"
WORKFLOW_NAME = "GREEN Codex issue worker"
WORKFLOW_EVENT = "issues"
TERMINAL_CONCLUSIONS = frozenset((
    "success", "failure", "cancelled", "skipped", "timed_out",
    "action_required", "neutral", "startup_failure", "stale",
))
TRUSTED_CLAIM_AUTHOR = "github-actions[bot]"
SCHEMA_VERSION = 1
MAX_ISSUES = 100
MAX_COMMENTS = 100
MAX_AUDIT = 1024
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
BRANCH_RE = re.compile(r"^codex/issue-[1-9][0-9]*-[a-z0-9][a-z0-9-]{0,80}$")
TIMESTAMP_RE = re.compile(r"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
CLAIM_MARKER_RE = re.compile(
    r"^<!-- codex-worker-claim issue:([1-9][0-9]*) run:([1-9][0-9]*) "
    r"branch:(codex/issue-[1-9][0-9]*-[a-z0-9][a-z0-9-]{0,80}) -->$"
)
COMPLETION_MARKER_RE = re.compile(r"^<!-- a4\.18-completion:(\{.*\}) -->$")
OBSERVER_USER_AGENT = "ml-amstress-codex-completion-observer"
REST_OPERATIONS = frozenset((
    "list-open-issues", "list-issue-comments", "list-open-prs",
    "create-completion-audit",
))


class ObserverError(Exception):
    """A fail-closed completion-observer error with no raw API detail."""


@dataclasses.dataclass(frozen=True)
class WorkflowRun:
    run_id: int
    conclusion: str
    branch: str
    head_sha: str
    created_at: str
    updated_at: str


@dataclasses.dataclass(frozen=True)
class Claim:
    issue_number: int
    run_id: int
    branch: str


@dataclasses.dataclass(frozen=True)
class PullRequestIdentity:
    number: int
    head_sha: str


def _number(value: Any, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise ObserverError(name + " is invalid")
    return value


def _sha(value: Any, name: str) -> str:
    if not isinstance(value, str) or not SHA_RE.fullmatch(value):
        raise ObserverError(name + " is invalid")
    return value


def _branch(value: Any, name: str) -> str:
    if not isinstance(value, str) or not BRANCH_RE.fullmatch(value):
        raise ObserverError(name + " is invalid")
    return value


def _timestamp(value: Any, name: str) -> str:
    if not isinstance(value, str) or not TIMESTAMP_RE.fullmatch(value):
        raise ObserverError(name + " is invalid")
    return value


def parse_workflow_run(event: Mapping[str, Any]) -> WorkflowRun:
    """Accept only the exact terminal GREEN worker event from this repository."""
    if not isinstance(event, Mapping):
        raise ObserverError("workflow event is invalid")
    repository = event.get("repository")
    if not isinstance(repository, Mapping) or repository.get("full_name") != REPOSITORY:
        raise ObserverError("workflow event repository is invalid")
    run = event.get("workflow_run")
    if not isinstance(run, Mapping) or run.get("name") != WORKFLOW_NAME:
        raise ObserverError("workflow event name is invalid")
    if run.get("event") != WORKFLOW_EVENT or run.get("status") != "completed":
        raise ObserverError("workflow event is not a completed GREEN worker run")
    conclusion = run.get("conclusion")
    if conclusion not in TERMINAL_CONCLUSIONS:
        raise ObserverError("workflow event conclusion is not terminal")
    return WorkflowRun(
        _number(run.get("id"), "workflow run id"),
        conclusion,
        _branch(run.get("head_branch"), "workflow run branch"),
        _sha(run.get("head_sha"), "workflow run head"),
        _timestamp(run.get("created_at"), "workflow run created timestamp"),
        _timestamp(run.get("updated_at"), "workflow run updated timestamp"),
    )


def parse_claim_marker(body: Any) -> Claim | None:
    """Parse only the first, exact trusted worker claim-marker line."""
    if not isinstance(body, str):
        return None
    first_line = body.splitlines()[0] if body.splitlines() else ""
    match = CLAIM_MARKER_RE.fullmatch(first_line)
    if not match:
        return None
    return Claim(int(match.group(1)), int(match.group(2)), match.group(3))


def _trusted_comment(comment: Mapping[str, Any]) -> bool:
    author = comment.get("user")
    return isinstance(author, Mapping) and author.get("login") == TRUSTED_CLAIM_AUTHOR


def resolve_claimed_issue(client: Any, run: WorkflowRun) -> tuple[int, Sequence[Mapping[str, Any]]]:
    """Resolve exactly one issue by an exact trusted marker; never guess from text."""
    issues = client.open_issues()
    if not isinstance(issues, Sequence) or isinstance(issues, (str, bytes)) or len(issues) >= MAX_ISSUES:
        raise ObserverError("open issue evidence is ambiguous")
    matches = []
    for issue in issues:
        if not isinstance(issue, Mapping) or "pull_request" in issue:
            continue
        number = _number(issue.get("number"), "issue number")
        comments = client.comments(number)
        if not isinstance(comments, Sequence) or isinstance(comments, (str, bytes)) or len(comments) >= MAX_COMMENTS:
            raise ObserverError("issue comment evidence is ambiguous")
        for comment in comments:
            if not isinstance(comment, Mapping) or not _trusted_comment(comment):
                continue
            claim = parse_claim_marker(comment.get("body"))
            if claim is None:
                continue
            if claim.issue_number != number:
                raise ObserverError("trusted claim marker issue identity is invalid")
            if claim.run_id == run.run_id and claim.branch == run.branch:
                matches.append((number, comments))
    if len(matches) != 1:
        raise ObserverError("workflow run does not resolve to exactly one trusted issue claim")
    return matches[0]


def completion_key(run: WorkflowRun, issue_number: int) -> str:
    """Return the deterministic idempotency identity for one terminal observation."""
    identity = {
        "conclusion": run.conclusion,
        "created_at": run.created_at,
        "issue_number": _number(issue_number, "issue number"),
        "repository": REPOSITORY,
        "run_id": run.run_id,
        "schema_version": SCHEMA_VERSION,
        "updated_at": run.updated_at,
    }
    encoded = json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return "a4.18:" + hashlib.sha256(encoded).hexdigest()


def _pr_identity(client: Any, run: WorkflowRun) -> PullRequestIdentity | None:
    """Optionally attach a PR only when its in-repository identity is exact."""
    pulls = client.open_prs_for_branch(run.branch)
    if not isinstance(pulls, Sequence) or isinstance(pulls, (str, bytes)) or len(pulls) > 1:
        raise ObserverError("open PR evidence is ambiguous")
    if not pulls:
        return None
    pr = pulls[0]
    if not isinstance(pr, Mapping) or str(pr.get("state", "")).lower() != "open":
        raise ObserverError("open PR evidence is invalid")
    base, head = pr.get("base"), pr.get("head")
    if not isinstance(base, Mapping) or not isinstance(head, Mapping):
        raise ObserverError("open PR identity is invalid")
    repository = head.get("repo")
    if (base.get("ref") != BASE_BRANCH or head.get("ref") != run.branch
            or not isinstance(repository, Mapping) or repository.get("full_name") != REPOSITORY
            or _sha(head.get("sha"), "PR head") != run.head_sha):
        raise ObserverError("open PR does not match workflow run identity")
    return PullRequestIdentity(_number(pr.get("number"), "PR number"), run.head_sha)


def completion_marker(run: WorkflowRun, issue_number: int, pr: PullRequestIdentity | None) -> str:
    """Build one small machine-readable audit comment without model output."""
    payload = {
        "branch": run.branch,
        "conclusion": run.conclusion,
        "created_at": run.created_at,
        "head_sha": run.head_sha,
        "idempotency_key": completion_key(run, issue_number),
        "issue_number": _number(issue_number, "issue number"),
        "pr_number": None if pr is None else pr.number,
        "repository": REPOSITORY,
        "run_id": run.run_id,
        "schema_version": SCHEMA_VERSION,
        "updated_at": run.updated_at,
    }
    marker = "<!-- a4.18-completion:" + json.dumps(payload, sort_keys=True, separators=(",", ":")) + " -->"
    if len(marker) > MAX_AUDIT:
        raise ObserverError("completion audit is unexpectedly unbounded")
    return marker


def _completion_markers(comments: Sequence[Mapping[str, Any]]) -> list[dict[str, Any]]:
    values = []
    for comment in comments:
        if not isinstance(comment, Mapping) or not _trusted_comment(comment):
            continue
        body = comment.get("body")
        if not isinstance(body, str) or not body.startswith("<!-- a4.18-completion:"):
            continue
        match = COMPLETION_MARKER_RE.fullmatch(body.strip())
        if not match:
            raise ObserverError("trusted completion audit is malformed")
        try:
            value = json.loads(match.group(1))
        except (TypeError, ValueError):
            raise ObserverError("trusted completion audit is malformed") from None
        if not isinstance(value, dict):
            raise ObserverError("trusted completion audit is malformed")
        values.append(value)
    return values


def record_completion_observation(client: Any, issue_number: int, comments: Sequence[Mapping[str, Any]],
                                  run: WorkflowRun, pr: PullRequestIdentity | None) -> bool:
    """Write at most one observation; labels and worker state remain untouched."""
    key = completion_key(run, issue_number)
    matches = [item for item in _completion_markers(comments) if item.get("idempotency_key") == key]
    if len(matches) > 1:
        raise ObserverError("completion observation is ambiguous")
    if matches:
        return False
    client.comment(issue_number, completion_marker(run, issue_number, pr))
    return True


def observe(client: Any, event: Mapping[str, Any]) -> str:
    """Resolve and record one terminal worker outcome, without changing worker state."""
    run = parse_workflow_run(event)
    issue_number, comments = resolve_claimed_issue(client, run)
    pr = _pr_identity(client, run)
    record_completion_observation(client, issue_number, comments, run, pr)
    return "worker-success" if run.conclusion == "success" else "worker-terminal-non-success"


class GitHubClient:
    """Small REST boundary with bounded, operation-only failure diagnostics."""
    def __init__(self, token: str | None, repository: str = REPOSITORY):
        if not token:
            raise ObserverError("GITHUB_TOKEN is required by trusted completion observation")
        self.token, self.repository = token, repository

    def _request(self, operation: str, method: str, path: str, payload: Any = None) -> Any:
        if operation not in REST_OPERATIONS:
            raise ObserverError("trusted GitHub operation is invalid")
        data = None if payload is None else json.dumps(payload).encode("utf-8")
        request = urllib.request.Request("https://api.github.com/repos/%s%s" % (self.repository, path), data=data, method=method)
        request.add_header("Accept", "application/vnd.github+json")
        request.add_header("Authorization", "Bearer " + self.token)
        request.add_header("X-GitHub-Api-Version", "2022-11-28")
        request.add_header("User-Agent", OBSERVER_USER_AGENT)
        if data is not None:
            request.add_header("Content-Type", "application/json")
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            raise ObserverError("GitHub %s: HTTP %d" % (operation, error.code)) from None
        except urllib.error.URLError:
            raise ObserverError("GitHub %s: transport failure" % operation) from None
        except (UnicodeError, ValueError):
            raise ObserverError("GitHub %s: invalid response" % operation) from None

    def open_issues(self) -> list[Mapping[str, Any]]:
        value = self._request("list-open-issues", "GET", "/issues?state=open&per_page=100")
        if not isinstance(value, list):
            raise ObserverError("open issue response is invalid")
        return value

    def comments(self, number: int) -> list[Mapping[str, Any]]:
        value = self._request("list-issue-comments", "GET", "/issues/%d/comments?per_page=100" % number)
        if not isinstance(value, list):
            raise ObserverError("issue comment response is invalid")
        return value

    def open_prs_for_branch(self, branch: str) -> list[Mapping[str, Any]]:
        value = self._request("list-open-prs", "GET", "/pulls?state=open&head=RayZhang2024:%s&per_page=100" % branch)
        if not isinstance(value, list):
            raise ObserverError("open PR response is invalid")
        return value

    def comment(self, number: int, body: str) -> None:
        if not isinstance(body, str) or len(body) > MAX_AUDIT:
            raise ObserverError("refusing unbounded completion audit")
        self._request("create-completion-audit", "POST", "/issues/%d/comments" % number, {"body": body})


def main(arguments: Sequence[str] | None = None) -> None:
    if arguments is None:
        arguments = sys.argv[1:]
    if arguments:
        raise ObserverError("completion observer accepts no command-line arguments")
    event_path = os.environ.get("GITHUB_EVENT_PATH")
    if not event_path:
        raise ObserverError("GITHUB_EVENT_PATH is required")
    with open(event_path, "r", encoding="utf-8") as stream:
        event = json.load(stream)
    observe(GitHubClient(os.environ.get("GITHUB_TOKEN")), event)


if __name__ == "__main__":
    try:
        main()
    except ObserverError as error:
        print("A4.18 completion observation blocked: " + str(error), file=sys.stderr)
        sys.exit(1)
