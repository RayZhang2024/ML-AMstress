"""Bounded, trusted same-branch repair foundation for A5.3.

This module deliberately has no GitHub client.  Its caller supplies an already
accepted, immutable blocker decision and owns all control-plane transitions.
"""
from __future__ import annotations

import base64
import dataclasses
import hashlib
import json
import os
import re
import shutil
import subprocess
from typing import Any, Callable, Sequence


REPAIR_CONTRACT_VERSION = 1
MAX_REPAIR_ATTEMPTS = 2
MAX_FINDINGS = 25
MAX_ALLOWED_PATHS = 50
MAX_TEXT = 1000
MAX_PATH = 240
MAX_RESULT_CHANGES = 50
MAX_IDENTIFIER_NUMBER = 2_000_000_000
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
BRANCH_RE = re.compile(r"^(?!/)(?!.*//)(?!.*\.\.)(?!.*[~^:?*\[\\\s])[^/].{0,199}$")
FINDING_ID_RE = re.compile(r"^F-[1-9][0-9]*$")
A5_2_DECISION_KEY_RE = re.compile(r"^a5\.2:[0-9a-f]{64}$")
A5_3_DECISION_KEY_RE = re.compile(r"^a5\.3:[0-9a-f]{64}$")
WINDOWS_ABSOLUTE_RE = re.compile(r"^[A-Za-z]:[\\/]")
REPAIRABLE_FINDING_CATEGORIES = frozenset(("scope", "policy", "security", "tests", "ci", "evidence"))
PROTECTED_PATHS = frozenset((
    "scripts/codex_issue_worker.py", "AGENTS.md", "docs/AUTONOMOUS_DEVELOPMENT.md",
    "docs/AUTONOMOUS_ORCHESTRATION.md", "docs/AUTONOMOUS_WORKER_RUNBOOK.md",
))
PROTECTED_PREFIXES = (".github/",)
DEFAULT_VALIDATION_COMMANDS = (("python", "-m", "compileall", "-q", "."),
                               ("python", "-m", "unittest", "discover", "-s", "tests", "-p", "test_*.py"))


class RepairError(Exception):
    """A bounded, audit-safe repair failure."""


@dataclasses.dataclass(frozen=True)
class BlockerFinding:
    finding_id: str
    category: str
    message: str
    required_action: str
    required_evidence: str


@dataclasses.dataclass(frozen=True)
class RepairRequest:
    schema_version: int
    repository: str
    pull_request_number: int
    issue_number: int
    branch: str
    expected_head_sha: str
    review_decision_key: str
    current_issue_status: str
    current_pr_review_state: str
    review_state_head_sha: str
    effective_risk: str
    accepted_findings: tuple[BlockerFinding, ...]
    allowed_paths: tuple[str, ...]
    attempt_number: int


@dataclasses.dataclass(frozen=True)
class RepairResult:
    schema_version: int
    repository: str
    pull_request_number: int
    issue_number: int
    branch: str
    attempt_number: int
    old_head_sha: str
    new_head_sha: str
    accepted_finding_ids: tuple[str, ...]
    changed_paths: tuple[str, ...]
    validation_status: str
    repair_decision_key: str


def parse_repair_request(payload: Any) -> RepairRequest:
    """Parse a JSON-like trusted payload without accepting extra authority."""
    required = frozenset(("schema_version", "repository", "pull_request_number", "issue_number", "branch",
                          "expected_head_sha", "review_decision_key", "current_issue_status",
                          "current_pr_review_state", "review_state_head_sha", "effective_risk",
                          "accepted_findings", "allowed_paths", "attempt_number"))
    if not isinstance(payload, dict) or frozenset(payload) != required:
        raise RepairError("repair request payload has an invalid shape")
    raw_findings = payload["accepted_findings"]
    if not isinstance(raw_findings, (list, tuple)):
        raise RepairError("accepted findings payload is malformed")
    finding_keys = frozenset(("finding_id", "category", "message", "required_action", "required_evidence"))
    findings = []
    for item in raw_findings:
        if not isinstance(item, dict) or frozenset(item) != finding_keys:
            raise RepairError("finding payload has an invalid shape")
        findings.append(BlockerFinding(**item))
    paths = payload["allowed_paths"]
    if not isinstance(paths, (list, tuple)):
        raise RepairError("allowed paths payload is malformed")
    request = RepairRequest(**dict(payload, accepted_findings=tuple(findings), allowed_paths=tuple(paths)))
    return validate_request(request)


def _positive(value: Any, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, int) or not 1 <= value <= MAX_IDENTIFIER_NUMBER:
        raise RepairError(name + " must be a bounded positive integer")


def _branch(value: Any) -> None:
    if (not isinstance(value, str) or not BRANCH_RE.fullmatch(value) or value.startswith("-")
            or value == "@" or "@{" in value or value.endswith(".") or value.endswith(".lock")
            or any(part.startswith(".") or part.endswith(".lock") for part in value.split("/"))):
        raise RepairError("branch is malformed")


def _bounded_text(value: Any, name: str) -> None:
    if not isinstance(value, str) or not value or len(value) > MAX_TEXT or any(ord(c) < 32 for c in value):
        raise RepairError(name + " must be bounded safe text")


def _safe_path(value: Any, name: str = "path") -> str:
    if not isinstance(value, str) or not value or len(value) > MAX_PATH:
        raise RepairError(name + " must be a bounded repository-relative path")
    path = value.replace("\\", "/")
    if (path != value or path.startswith("/") or WINDOWS_ABSOLUTE_RE.match(path)
            or path in (".", "..") or "//" in path or any(part in ("", ".", "..") for part in path.split("/"))
            or any(ord(c) < 32 for c in path)):
        raise RepairError(name + " is unsafe")
    return path


def is_protected_path(path: str) -> bool:
    canonical = path.casefold()
    return (canonical in {item.casefold() for item in PROTECTED_PATHS}
            or any(canonical.startswith(prefix.casefold()) for prefix in PROTECTED_PREFIXES))


def validate_request(request: RepairRequest) -> RepairRequest:
    """Validate every authorization fact before any subprocess is launched."""
    if not isinstance(request, RepairRequest) or request.schema_version != REPAIR_CONTRACT_VERSION:
        raise RepairError("unsupported repair contract version")
    if not isinstance(request.repository, str) or len(request.repository) > 200 or not REPOSITORY_RE.fullmatch(request.repository):
        raise RepairError("repository is malformed")
    _positive(request.pull_request_number, "pull_request_number")
    _positive(request.issue_number, "issue_number")
    _branch(request.branch)
    if not isinstance(request.expected_head_sha, str) or not SHA_RE.fullmatch(request.expected_head_sha):
        raise RepairError("expected_head_sha is malformed")
    if not isinstance(request.review_decision_key, str) or not A5_2_DECISION_KEY_RE.fullmatch(request.review_decision_key):
        raise RepairError("review_decision_key is malformed")
    if request.current_issue_status != "status:in-progress" or request.current_pr_review_state != "review:blocker":
        raise RepairError("only status:in-progress plus review:blocker authorizes repair")
    if request.review_state_head_sha != request.expected_head_sha:
        raise RepairError("review evidence head must equal expected head")
    if request.effective_risk != "green":
        raise RepairError("only green repair is autonomous")
    if not isinstance(request.attempt_number, int) or isinstance(request.attempt_number, bool) or not 1 <= request.attempt_number <= MAX_REPAIR_ATTEMPTS:
        raise RepairError("repair attempt is outside the bounded range")
    if not isinstance(request.accepted_findings, tuple) or not 1 <= len(request.accepted_findings) <= MAX_FINDINGS:
        raise RepairError("accepted findings must be a bounded non-empty tuple")
    ids = []
    for finding in request.accepted_findings:
        if not isinstance(finding, BlockerFinding):
            raise RepairError("finding is malformed")
        if not isinstance(finding.finding_id, str) or not FINDING_ID_RE.fullmatch(finding.finding_id):
            raise RepairError("finding ID is malformed")
        for name in ("category", "message", "required_action", "required_evidence"):
            _bounded_text(getattr(finding, name), "finding " + name)
        if finding.category not in REPAIRABLE_FINDING_CATEGORIES:
            raise RepairError("scientific or domain finding requires human escalation")
        ids.append(finding.finding_id)
    if len(set(ids)) != len(ids):
        raise RepairError("finding IDs must be unique")
    if not isinstance(request.allowed_paths, tuple) or not 1 <= len(request.allowed_paths) <= MAX_ALLOWED_PATHS:
        raise RepairError("allowed paths must be a bounded non-empty tuple")
    paths = tuple(_safe_path(path, "allowed path") for path in request.allowed_paths)
    if len(set(paths)) != len(paths):
        raise RepairError("allowed paths must be unique")
    if any(is_protected_path(path) for path in paths):
        raise RepairError("allowed paths include protected control-plane path")
    return request


def repair_decision_key(request: RepairRequest) -> str:
    validate_request(request)
    identity = {"version": REPAIR_CONTRACT_VERSION, "repository": request.repository,
                "pr": request.pull_request_number, "head": request.expected_head_sha,
                "review_key": request.review_decision_key, "attempt": request.attempt_number,
                "findings": [item.finding_id for item in request.accepted_findings]}
    return "a5.3:" + hashlib.sha256(json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")).hexdigest()


def build_repair_prompt(request: RepairRequest) -> str:
    validate_request(request)
    findings = "\n".join("- %s [%s]: %s\n  Required action: %s\n  Required evidence: %s" %
                         (f.finding_id, f.category, f.message, f.required_action, f.required_evidence)
                         for f in request.accepted_findings)
    return ("Trusted repair request (A5.3). The trusted worker already resolved GitHub, PR, and state identity. "
            "Do not query GitHub or require GitHub credentials. Repair only these accepted blocker findings:\n%s\n"
            "You may modify only these exact repository-relative paths: %s\n"
            "Do not change scientific behavior or protected control-plane files. Do not commit, push, create PRs, "
            "change labels/status, merge, or auto-merge. Return a concise final response only; trusted local validation, "
            "not model prose, determines success." % (findings, ", ".join(request.allowed_paths)))


def _isolated_environment(remove_tokens: bool = True) -> dict[str, str]:
    env = os.environ.copy()
    if remove_tokens:
        for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY"):
            env.pop(name, None)
    for name in list(env):
        if name == "GIT_CONFIG_COUNT" or name.startswith("GIT_CONFIG_KEY_") or name.startswith("GIT_CONFIG_VALUE_"):
            env.pop(name, None)
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    env["GIT_TERMINAL_PROMPT"] = "0"
    return env


def resolve_codex_executable(executable: str | None = None) -> str:
    configured = executable if executable is not None else os.environ.get("CODEX_EXECUTABLE", "codex")
    if not isinstance(configured, str) or not configured.strip():
        raise RepairError("Codex executable is not configured")
    resolved = shutil.which(configured) or (os.path.abspath(configured) if os.path.isfile(configured) else None)
    if not resolved:
        raise RepairError("Codex executable is not available")
    return resolved


def _run(command: Sequence[str], cwd: str, env: dict[str, str] | None = None, input_text: str | None = None) -> subprocess.CompletedProcess:
    try:
        return subprocess.run(list(command), cwd=cwd, env=env or _isolated_environment(), input=input_text,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False)
    except (OSError, ValueError) as error:
        raise RepairError("trusted subprocess could not start") from error


def _git_text(command: Sequence[str], cwd: str) -> str:
    result = _run(command, cwd)
    if result.returncode:
        raise RepairError("local git preflight failed")
    return result.stdout.strip()


def preflight(request: RepairRequest, cwd: str) -> None:
    validate_request(request)
    if _git_text(("git", "status", "--porcelain=v1", "--untracked-files=all"), cwd):
        raise RepairError("working tree is not clean")
    if _git_text(("git", "branch", "--show-current"), cwd) != request.branch:
        raise RepairError("current branch does not match repair branch")
    if _git_text(("git", "rev-parse", "HEAD"), cwd) != request.expected_head_sha:
        raise RepairError("local HEAD does not match expected repair head")


def run_codex(request: RepairRequest, cwd: str, executable: str | None = None) -> None:
    command = [resolve_codex_executable(executable), "exec", "--sandbox", "workspace-write", "-c", 'approval_policy="never"', "-"]
    result = _run(command, cwd, _isolated_environment(), build_repair_prompt(request))
    if result.returncode:
        raise RepairError("Codex execution failed")


def post_codex_identity(request: RepairRequest, cwd: str) -> None:
    """Reject a model-created commit, checkout, or branch switch before validation."""
    if _git_text(("git", "branch", "--show-current"), cwd) != request.branch:
        raise RepairError("Codex changed the repair branch")
    if _git_text(("git", "rev-parse", "HEAD"), cwd) != request.expected_head_sha:
        raise RepairError("Codex changed local HEAD")


def changed_paths(cwd: str, expected_head: str) -> tuple[str, ...]:
    result = _run(("git", "diff", "--name-status", "--no-renames", expected_head, "--"), cwd)
    if result.returncode:
        raise RepairError("could not inspect repair changes")
    paths = []
    for line in result.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) != 2 or parts[0] not in ("M", "A"):
            raise RepairError("repair contains unsupported change mode")
        paths.append(_safe_path(parts[1], "changed path"))
    # Untracked creation is allowed only when it is explicitly staged later;
    # all other porcelain modes are rejected rather than guessed about.
    status = _run(("git", "status", "--porcelain=v1", "--untracked-files=all"), cwd)
    if status.returncode:
        raise RepairError("could not inspect repair workspace")
    for line in status.stdout.splitlines():
        if len(line) < 4 or line[2] != " ":
            raise RepairError("repair contains unsupported git status")
        mode, raw_path = line[:2], line[3:]
        path = _safe_path(raw_path, "changed path")
        if mode == "??":
            paths.append(path)
        elif mode not in (" M", "M ", "MM"):
            raise RepairError("repair contains unsupported change mode")
    if not paths:
        raise RepairError("Codex made no repository changes")
    if len(paths) > MAX_RESULT_CHANGES or len(set(paths)) != len(paths):
        raise RepairError("repair change paths are unsafe")
    return tuple(sorted(paths))


def enforce_change_scope(request: RepairRequest, paths: tuple[str, ...]) -> None:
    allowed = set(request.allowed_paths)
    if any(path not in allowed or is_protected_path(path) for path in paths):
        raise RepairError("repair changed a path outside the trusted scope")


def run_validation(cwd: str, commands: tuple[tuple[str, ...], ...] = DEFAULT_VALIDATION_COMMANDS) -> None:
    if not isinstance(commands, tuple) or len(commands) > 10:
        raise RepairError("validation command list is invalid")
    for command in commands:
        if not isinstance(command, tuple) or not command or any(not isinstance(token, str) or not token for token in command):
            raise RepairError("validation command must be trusted argv")
        if _run(command, cwd).returncode:
            raise RepairError("local validation failed")
    if _run(("git", "diff", "--check"), cwd).returncode:
        raise RepairError("git diff check failed")


def commit_repair(request: RepairRequest, cwd: str) -> str:
    message = "A5.3 repair PR #%d attempt %d" % (request.pull_request_number, request.attempt_number)
    if _run(("git", "add", "--", *changed_paths(cwd, request.expected_head_sha)), cwd).returncode:
        raise RepairError("could not stage repair")
    if _run(("git", "commit", "-m", message), cwd).returncode:
        raise RepairError("could not create repair commit")
    new_head = _git_text(("git", "rev-parse", "HEAD"), cwd)
    if _run(("git", "merge-base", "--is-ancestor", request.expected_head_sha, new_head), cwd).returncode:
        raise RepairError("repair commit does not descend from expected head")
    return new_head


def push_repair(request: RepairRequest, cwd: str, new_head: str) -> None:
    token = os.environ.get("GITHUB_TOKEN")
    if not token:
        raise RepairError("trusted push credential is unavailable")
    encoded = base64.b64encode(("x-access-token:" + token).encode("utf-8")).decode("ascii")
    env = _isolated_environment()
    env["GIT_CONFIG_COUNT"] = "1"
    env["GIT_CONFIG_KEY_0"] = "http.https://github.com/.extraheader"
    env["GIT_CONFIG_VALUE_0"] = "AUTHORIZATION: basic " + encoded
    # The lease refuses a remotely moved PR branch without overwriting it.
    try:
        result = _run(("git", "push", "--force-with-lease=refs/heads:%s:%s" % (request.branch, request.expected_head_sha),
                       "origin", "HEAD:refs/heads/" + request.branch), cwd, env)
    finally:
        # This is process-local, but remove it even if launching Git fails.
        env.pop("GIT_CONFIG_VALUE_0", None)
    if result.returncode:
        raise RepairError("remote repair branch moved or push failed")


def execute_repair(request: RepairRequest, cwd: str, executable: str | None = None,
                   validation_commands: tuple[tuple[str, ...], ...] = DEFAULT_VALIDATION_COMMANDS,
                   push: Callable[[RepairRequest, str, str], None] = push_repair) -> RepairResult:
    """Run one bounded repair; only this function commits and invokes trusted push."""
    validate_request(request)
    preflight(request, cwd)
    run_codex(request, cwd, executable)
    post_codex_identity(request, cwd)
    paths = changed_paths(cwd, request.expected_head_sha)
    enforce_change_scope(request, paths)
    run_validation(cwd, validation_commands)
    new_head = commit_repair(request, cwd)
    push(request, cwd, new_head)
    return RepairResult(REPAIR_CONTRACT_VERSION, request.repository, request.pull_request_number,
                        request.issue_number, request.branch, request.attempt_number, request.expected_head_sha,
                        new_head, tuple(f.finding_id for f in request.accepted_findings), paths,
                        "passed", repair_decision_key(request))


def serialize_result(result: RepairResult) -> str:
    if not isinstance(result, RepairResult) or result.schema_version != REPAIR_CONTRACT_VERSION:
        raise RepairError("invalid repair result")
    if (not isinstance(result.repository, str) or not REPOSITORY_RE.fullmatch(result.repository)
            or not isinstance(result.old_head_sha, str) or not SHA_RE.fullmatch(result.old_head_sha)
            or not isinstance(result.new_head_sha, str) or not SHA_RE.fullmatch(result.new_head_sha)
            or result.validation_status != "passed"):
        raise RepairError("invalid repair result")
    _branch(result.branch)
    _positive(result.pull_request_number, "result pull_request_number")
    _positive(result.issue_number, "result issue_number")
    if not isinstance(result.attempt_number, int) or not 1 <= result.attempt_number <= MAX_REPAIR_ATTEMPTS:
        raise RepairError("invalid repair result")
    if (not isinstance(result.accepted_finding_ids, tuple) or not result.accepted_finding_ids
            or len(result.accepted_finding_ids) > MAX_FINDINGS
            or len(set(result.accepted_finding_ids)) != len(result.accepted_finding_ids)
            or any(not isinstance(item, str) or not FINDING_ID_RE.fullmatch(item) for item in result.accepted_finding_ids)):
        raise RepairError("invalid repair result")
    if (not isinstance(result.changed_paths, tuple) or not result.changed_paths
            or len(result.changed_paths) > MAX_RESULT_CHANGES
            or len(set(result.changed_paths)) != len(result.changed_paths)
            or any(is_protected_path(_safe_path(p, "result path")) for p in result.changed_paths)):
        raise RepairError("unsafe repair result")
    if not isinstance(result.repair_decision_key, str) or not A5_3_DECISION_KEY_RE.fullmatch(result.repair_decision_key):
        raise RepairError("invalid repair result")
    return json.dumps(dataclasses.asdict(result), sort_keys=True, separators=(",", ":"))
