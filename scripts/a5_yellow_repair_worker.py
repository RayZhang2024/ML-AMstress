"""Trusted bounded repair executor for pre-authorized automated-YELLOW PRs.

This is deliberately a separate authority from the GREEN A5.3 request.  The
trusted orchestrator supplies validated #147 authorization and #143 blocker
evidence; this module performs no GitHub reads or state transitions.
"""
from __future__ import annotations

import base64
import dataclasses
import hashlib
import json
import os
import re
import subprocess
from typing import Any, Callable

from scripts import a5_repair_worker as green_repair
from scripts import yellow_lane_policy


CONTRACT_VERSION = 1
DECISION_KEY_RE = re.compile(r"^a5\.yellow-repair:[0-9a-f]{64}$")
EVIDENCE_KEY_RE = re.compile(r"^a5\.yellow-(?:authorization|blocker):[0-9a-f]{64}$")
PROHIBITED_SCIENTIFIC_PATHS = frozenset((
    "AM_gui_v7.py", "apply_boundary.py", "apply_materials.py", "apply_meshing.py",
    "build_cae.py", "create_input.py", "data_extract.py", "import_and_partition.py",
))
MAX_CANDIDATE_BYTES = 50 * 1024 * 1024


class YellowRepairError(green_repair.RepairError):
    """A bounded, fail-closed automated-YELLOW repair error."""


@dataclasses.dataclass(frozen=True)
class YellowRepairRequest:
    schema_version: int
    repository: str
    pull_request_number: int
    issue_number: int
    branch: str
    expected_head_sha: str
    blocker_decision_key: str
    blocker_evidence_key: str
    authorization_key: str
    current_issue_status: str
    current_pr_review_state: str
    review_state_head_sha: str
    effective_risk: str
    accepted_findings: tuple[green_repair.BlockerFinding, ...]
    authorized_paths: tuple[str, ...]
    attempt_number: int


@dataclasses.dataclass(frozen=True)
class YellowRepairResult:
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


@dataclasses.dataclass(frozen=True)
class CandidateIdentity:
    workspace_digest: str
    content_digest: str


def evidence_key(kind: str, canonical_text: str) -> str:
    if kind not in ("authorization", "blocker") or not isinstance(canonical_text, str) or not canonical_text:
        raise YellowRepairError("YELLOW evidence identity is malformed")
    return "a5.yellow-%s:%s" % (
        kind, hashlib.sha256(canonical_text.encode("utf-8")).hexdigest(),
    )


def _safe_path(path: Any) -> str:
    try:
        return green_repair._safe_path(path, "authorized YELLOW path")
    except green_repair.RepairError as error:
        raise YellowRepairError(str(error)) from None


def validate_request(request: YellowRepairRequest) -> YellowRepairRequest:
    """Validate all lane, head, evidence, finding, and exact-path authority."""
    if not isinstance(request, YellowRepairRequest) or request.schema_version != CONTRACT_VERSION:
        raise YellowRepairError("unsupported YELLOW repair contract version")
    if request.repository != yellow_lane_policy.REPOSITORY:
        raise YellowRepairError("YELLOW repair repository is invalid")
    green_repair._positive(request.pull_request_number, "pull_request_number")
    green_repair._positive(request.issue_number, "issue_number")
    try:
        yellow_lane_policy.validate_yellow_branch(request.branch, request.issue_number)
    except yellow_lane_policy.PolicyError:
        raise YellowRepairError("YELLOW repair branch identity is invalid") from None
    if not isinstance(request.expected_head_sha, str) or not green_repair.SHA_RE.fullmatch(request.expected_head_sha):
        raise YellowRepairError("expected_head_sha is malformed")
    if not isinstance(request.blocker_decision_key, str) or not green_repair.A5_2_DECISION_KEY_RE.fullmatch(request.blocker_decision_key):
        raise YellowRepairError("blocker decision identity is malformed")
    for value in (request.blocker_evidence_key, request.authorization_key):
        if not isinstance(value, str) or not EVIDENCE_KEY_RE.fullmatch(value):
            raise YellowRepairError("YELLOW evidence identity is malformed")
    if not request.blocker_evidence_key.startswith("a5.yellow-blocker:") or not request.authorization_key.startswith("a5.yellow-authorization:"):
        raise YellowRepairError("YELLOW evidence identity type is invalid")
    if (request.current_issue_status != "status:in-progress"
            or request.current_pr_review_state != "review:blocker"
            or request.review_state_head_sha != request.expected_head_sha):
        raise YellowRepairError("exact-head YELLOW blocker state is required")
    if request.effective_risk != "yellow":
        raise YellowRepairError("only effective YELLOW work is authorized")
    if (not isinstance(request.attempt_number, int) or isinstance(request.attempt_number, bool)
            or not 1 <= request.attempt_number <= green_repair.MAX_REPAIR_ATTEMPTS):
        raise YellowRepairError("repair attempt is outside the bounded range")
    if (not isinstance(request.accepted_findings, tuple) or not request.accepted_findings
            or len(request.accepted_findings) > green_repair.MAX_FINDINGS):
        raise YellowRepairError("accepted findings must be bounded and non-empty")
    identifiers = []
    for finding in request.accepted_findings:
        if not isinstance(finding, green_repair.BlockerFinding):
            raise YellowRepairError("accepted YELLOW finding is malformed")
        if (not isinstance(finding.finding_id, str)
                or not green_repair.FINDING_ID_RE.fullmatch(finding.finding_id)):
            raise YellowRepairError("finding ID is malformed")
        if finding.category not in green_repair.REPAIRABLE_FINDING_CATEGORIES:
            raise YellowRepairError("scientific or domain finding requires human escalation")
        for name in ("category", "message", "required_action", "required_evidence"):
            green_repair._bounded_text(getattr(finding, name), "finding " + name)
        identifiers.append(finding.finding_id)
    if len(identifiers) != len(set(identifiers)):
        raise YellowRepairError("finding IDs must be unique")
    if (not isinstance(request.authorized_paths, tuple) or not request.authorized_paths
            or len(request.authorized_paths) > yellow_lane_policy.MAX_PATHS):
        raise YellowRepairError("authorized YELLOW paths must be bounded and non-empty")
    paths = tuple(_safe_path(path) for path in request.authorized_paths)
    if len(paths) != len(set(paths)) or paths != tuple(sorted(paths)):
        raise YellowRepairError("authorized YELLOW paths must be unique and canonical")
    if any(path.casefold() in {item.casefold() for item in PROHIBITED_SCIENTIFIC_PATHS} for path in paths):
        raise YellowRepairError("scientific or runtime path is not repairable in the YELLOW lane")
    return request


def repair_decision_key(request: YellowRepairRequest) -> str:
    validate_request(request)
    identity = {
        "version": CONTRACT_VERSION, "repository": request.repository,
        "pr": request.pull_request_number, "issue": request.issue_number,
        "branch": request.branch, "head": request.expected_head_sha,
        "blocker_decision": request.blocker_decision_key,
        "blocker_evidence": request.blocker_evidence_key,
        "authorization": request.authorization_key,
        "risk": request.effective_risk, "attempt": request.attempt_number,
        "findings": [finding.finding_id for finding in request.accepted_findings],
        "paths": list(request.authorized_paths),
    }
    return "a5.yellow-repair:" + hashlib.sha256(
        json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()


def build_prompt(request: YellowRepairRequest) -> str:
    validate_request(request)
    findings = "\n".join(
        "- %s [%s]: %s\n  Required action: %s\n  Required evidence: %s" %
        (item.finding_id, item.category, item.message, item.required_action, item.required_evidence)
        for item in request.accepted_findings
    )
    return (
        "Trusted automated-YELLOW repair request. Trusted orchestration already validated GitHub identity, "
        "the exact head, #147 authorization, #143 blocker evidence, dependencies, and attempt capacity. "
        "Do not query GitHub or require credentials. Repair only these repository-editable findings:\n%s\n"
        "Modify only these exact authorized paths: %s\nDo not alter scientific/model/Abaqus/GUI/ML behavior. "
        "Do not commit, push, create a PR, change state, merge, or run controlled runtime. Trusted validation "
        "and push own the final decision." % (findings, ", ".join(request.authorized_paths))
    )


def preflight(request: YellowRepairRequest, cwd: str) -> None:
    validate_request(request)
    if green_repair._git_text(("git", "status", "--porcelain=v1", "--untracked-files=all"), cwd):
        raise YellowRepairError("working tree is not clean")
    if green_repair._git_text(("git", "branch", "--show-current"), cwd) != request.branch:
        raise YellowRepairError("current branch does not match repair branch")
    if green_repair._git_text(("git", "rev-parse", "HEAD"), cwd) != request.expected_head_sha:
        raise YellowRepairError("local HEAD does not match expected repair head")


def run_codex(request: YellowRepairRequest, cwd: str, executable: str | None = None) -> None:
    try:
        command = [green_repair.resolve_codex_executable(executable), "exec", "--model", green_repair.CODEX_REPAIR_MODEL,
                   "--sandbox", "workspace-write", "-c", 'approval_policy="never"', "-"]
        result = green_repair._run(command, cwd, green_repair._isolated_environment(), build_prompt(request))
    except subprocess.TimeoutExpired:
        raise YellowRepairError(green_repair.CODEX_FAILURE_TIMEOUT) from None
    except green_repair.RepairError as error:
        if str(error) in ("Codex executable is not configured", "Codex executable is not available",
                          "trusted subprocess could not start"):
            raise YellowRepairError(green_repair.CODEX_FAILURE_LAUNCH) from None
        raise
    if result.returncode:
        raise YellowRepairError(green_repair.classify_codex_execution_failure(result))


def enforce_change_scope(request: YellowRepairRequest, paths: tuple[str, ...]) -> None:
    if any(path not in set(request.authorized_paths) for path in paths):
        raise YellowRepairError("repair changed a path outside the trusted YELLOW scope")
    if any(path.casefold() in {item.casefold() for item in PROHIBITED_SCIENTIFIC_PATHS} for path in paths):
        raise YellowRepairError("repair changed scientific or runtime behavior")


def _candidate_digest(head: str, entries: list[tuple[str, str]]) -> str:
    payload = {"expected_head": head, "files": [
        {"path": path, "blob": blob} for path, blob in entries
    ]}
    return hashlib.sha256(json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")).hexdigest()


def candidate_identity(cwd: str, expected_head: str, paths: tuple[str, ...]) -> CandidateIdentity:
    """Bind exact paths and working-tree blob content without persisting it."""
    if paths != tuple(sorted(set(paths))) or not paths:
        raise YellowRepairError("repair candidate paths are not canonical")
    entries, total = [], 0
    root = os.path.realpath(cwd)
    for path in paths:
        _safe_path(path)
        local = os.path.join(root, *path.split("/"))
        resolved = os.path.realpath(local)
        try:
            contained = os.path.commonpath((root, resolved)) == root
        except ValueError:
            contained = False
        if not contained or os.path.islink(local) or not os.path.isfile(local):
            raise YellowRepairError("repair candidate contains an unsafe file")
        total += os.path.getsize(local)
        if total > MAX_CANDIDATE_BYTES:
            raise YellowRepairError("repair candidate is unexpectedly large")
        blob = green_repair._git_text(("git", "hash-object", "--path=" + path, "--", path), cwd)
        if not green_repair.SHA_RE.fullmatch(blob):
            raise YellowRepairError("repair candidate blob identity is invalid")
        entries.append((path, blob))
    content_digest = _candidate_digest(expected_head, entries)
    status = green_repair._run(("git", "status", "--porcelain=v1", "--untracked-files=all"), cwd)
    if status.returncode:
        raise YellowRepairError("could not inspect repair candidate state")
    states = []
    for line in status.stdout.splitlines():
        if len(line) < 4 or line[2] != " ":
            raise YellowRepairError("repair candidate has unsupported state")
        path = _safe_path(line[3:])
        if path not in paths or line[:2] not in ("??", " M", "M ", "MM"):
            raise YellowRepairError("repair candidate state is outside the trusted scope")
        states.append((path, line[:2]))
    if tuple(sorted(path for path, _mode in states)) != paths or len(states) != len(paths):
        raise YellowRepairError("repair candidate state is incomplete or ambiguous")
    workspace = {"content": content_digest, "states": [
        {"path": path, "mode": mode} for path, mode in sorted(states)
    ]}
    return CandidateIdentity(
        hashlib.sha256(json.dumps(workspace, sort_keys=True, separators=(",", ":")).encode("utf-8")).hexdigest(),
        content_digest,
    )


def committed_paths(cwd: str, expected_head: str, new_head: str) -> tuple[str, ...]:
    result = green_repair._run(
        ("git", "diff", "--name-status", "--no-renames", expected_head, new_head, "--"), cwd
    )
    if result.returncode:
        raise YellowRepairError("could not inspect committed repair")
    paths = []
    for line in result.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) != 2 or parts[0] not in ("M", "A"):
            raise YellowRepairError("committed repair contains unsupported change mode")
        paths.append(_safe_path(parts[1]))
    canonical = tuple(sorted(paths))
    if not canonical or len(canonical) != len(set(canonical)):
        raise YellowRepairError("committed repair paths are empty or ambiguous")
    return canonical


def committed_identity(cwd: str, expected_head: str, new_head: str,
                       paths: tuple[str, ...]) -> str:
    entries = []
    for path in paths:
        blob = green_repair._git_text(("git", "rev-parse", new_head + ":" + path), cwd)
        if not green_repair.SHA_RE.fullmatch(blob):
            raise YellowRepairError("committed repair blob identity is invalid")
        entries.append((path, blob))
    return _candidate_digest(expected_head, entries)


def verify_committed_repair(request: YellowRepairRequest, cwd: str, new_head: str,
                            expected_paths: tuple[str, ...],
                            expected_identity: CandidateIdentity) -> tuple[str, ...]:
    """Revalidate exact committed content immediately before trusted push."""
    if green_repair._git_text(("git", "rev-parse", "HEAD"), cwd) != new_head:
        raise YellowRepairError("committed repair head changed before push")
    if green_repair._git_text(("git", "rev-parse", new_head + "^"), cwd) != request.expected_head_sha:
        raise YellowRepairError("repair must contain exactly one trusted commit")
    if green_repair._git_text(("git", "status", "--porcelain=v1", "--untracked-files=all"), cwd):
        raise YellowRepairError("repair workspace changed after commit")
    paths = committed_paths(cwd, request.expected_head_sha, new_head)
    enforce_change_scope(request, paths)
    if paths != expected_paths or committed_identity(
            cwd, request.expected_head_sha, new_head, paths) != expected_identity.content_digest:
        raise YellowRepairError("committed repair does not match the validated candidate")
    return paths


def commit_repair(request: YellowRepairRequest, cwd: str, paths: tuple[str, ...],
                  identity: CandidateIdentity) -> tuple[str, tuple[str, ...]]:
    current_paths = green_repair.changed_paths(cwd, request.expected_head_sha)
    enforce_change_scope(request, current_paths)
    if current_paths != paths or candidate_identity(cwd, request.expected_head_sha, current_paths) != identity:
        raise YellowRepairError("repair candidate changed before commit")
    if green_repair._run(("git", "add", "--", *paths), cwd).returncode:
        raise YellowRepairError("could not stage repair")
    command = ("git", "-c", "user.useConfigOnly=true", "-c", "user.name=" + green_repair.A5_GIT_COMMIT_NAME,
               "-c", "user.email=" + green_repair.A5_GIT_COMMIT_EMAIL, "commit", "-m",
               "A5 YELLOW repair PR #%d attempt %d" % (request.pull_request_number, request.attempt_number))
    if green_repair._run(command, cwd, green_repair._isolated_environment()).returncode:
        raise YellowRepairError("could not create repair commit")
    new_head = green_repair._git_text(("git", "rev-parse", "HEAD"), cwd)
    if green_repair._run(("git", "merge-base", "--is-ancestor", request.expected_head_sha, new_head), cwd).returncode:
        raise YellowRepairError("repair commit does not descend from expected head")
    return new_head, verify_committed_repair(request, cwd, new_head, paths, identity)


def push_repair(request: YellowRepairRequest, cwd: str, new_head: str) -> None:
    token = os.environ.get("AUTOMATION_APP_TOKEN")
    if not token:
        raise YellowRepairError("trusted App push credential is unavailable")
    encoded = base64.b64encode(("x-access-token:" + token).encode("utf-8")).decode("ascii")
    environment = green_repair._isolated_environment()
    environment.update({"GIT_CONFIG_COUNT": "1", "GIT_CONFIG_KEY_0": "http.https://github.com/.extraheader",
                        "GIT_CONFIG_VALUE_0": "AUTHORIZATION: basic " + encoded})
    try:
        result = green_repair._run(
            ("git", "push", "--force-with-lease=refs/heads/%s:%s" % (request.branch, request.expected_head_sha),
             "origin", "HEAD:refs/heads/" + request.branch), cwd, environment,
        )
    finally:
        environment.pop("GIT_CONFIG_VALUE_0", None)
    if result.returncode:
        raise YellowRepairError("remote repair branch moved or push failed")


def execute_repair(request: YellowRepairRequest, cwd: str, executable: str | None = None,
                   validation_commands: tuple[tuple[str, ...], ...] = green_repair.DEFAULT_VALIDATION_COMMANDS,
                   push: Callable[[YellowRepairRequest, str, str], None] = push_repair) -> YellowRepairResult:
    validate_request(request)
    preflight(request, cwd)
    run_codex(request, cwd, executable)
    if (green_repair._git_text(("git", "branch", "--show-current"), cwd) != request.branch
            or green_repair._git_text(("git", "rev-parse", "HEAD"), cwd) != request.expected_head_sha):
        raise YellowRepairError("Codex changed the repair identity")
    paths = green_repair.changed_paths(cwd, request.expected_head_sha)
    enforce_change_scope(request, paths)
    identity = candidate_identity(cwd, request.expected_head_sha, paths)
    green_repair.run_validation(cwd, validation_commands)
    post_validation_paths = green_repair.changed_paths(cwd, request.expected_head_sha)
    enforce_change_scope(request, post_validation_paths)
    if (post_validation_paths != paths
            or candidate_identity(cwd, request.expected_head_sha, post_validation_paths) != identity):
        raise YellowRepairError("validation changed the repair candidate")
    new_head, committed = commit_repair(request, cwd, post_validation_paths, identity)
    committed = verify_committed_repair(request, cwd, new_head, committed, identity)
    push(request, cwd, new_head)
    return YellowRepairResult(
        CONTRACT_VERSION, request.repository, request.pull_request_number, request.issue_number,
        request.branch, request.attempt_number, request.expected_head_sha, new_head,
        tuple(item.finding_id for item in request.accepted_findings), committed, "passed",
        repair_decision_key(request),
    )
