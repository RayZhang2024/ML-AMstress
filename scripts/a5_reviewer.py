"""Fail-closed, read-only Codex PR reviewer foundation (A5.1).

This module deliberately has no GitHub client.  A trusted caller prepares an
immutable snapshot, and this module returns only a validated verdict or raises
``ReviewError``.  It never repairs a checkout or changes remote state.
"""

from __future__ import annotations

import dataclasses
import json
import os
import re
import shutil
import subprocess
import tempfile
from types import MappingProxyType
from typing import Any, Mapping, Sequence

from scripts import codex_issue_worker as green_worker


SNAPSHOT_SCHEMA_VERSION = 1
VERDICT_SCHEMA_VERSION = 1
RISKS = frozenset(("green", "yellow", "red"))
VERDICTS = frozenset(("clean", "blocker", "escalate"))
CI_STATUSES = frozenset(("missing", "pending", "success", "failure"))
FINDING_CATEGORIES = frozenset(("scope", "policy", "security", "tests", "ci", "evidence", "scientific"))
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
PATH_RE = re.compile(r"^(?!/)(?!.*(?:^|/)\.\.(?:/|$))[A-Za-z0-9._/ -]+$")
FINDING_ID_RE = re.compile(r"^F-[1-9][0-9]*$")
SECRET_RE = re.compile(
    r"(?i)(?:\b(?:gh[pousr]_[A-Za-z0-9_]{8,}|sk-[A-Za-z0-9_-]{8,}|AKIA[0-9A-Z]{16})\b|"
    r"\b(?:api[_-]?key|access[_-]?token|auth[_-]?token|refresh[_-]?token|secret)\s*[=:]\s*['\"]?[A-Za-z0-9._~+/=-]{8,})"
)
MAX_TEXT = 20_000
MAX_PATCH = 120_000
MAX_CHANGED_FILES = 200
MAX_CHECKS = 100
MAX_LABELS = 50
MAX_REVIEWER_FAILURE_DIAGNOSTIC_CHARS = 500
MAX_REVIEWER_FAILURE_DIAGNOSTIC_LINES = 3
MAX_REVIEWER_FINAL_OUTPUT_BYTES = 200_000
REVIEWER_MODEL = "gpt-5.5"
LOCAL_ABSOLUTE_PATH_RE = re.compile(
    r"(?i)(?:\b[A-Z]:[\\/][^\s\"']+|\\\\[^\s\"']+|(?<![:\w])/(?:[^\s\"']+))"
)
APP_TOKEN_ASSIGNMENT_RE = re.compile(
    r"(?i)\b(?:automation[_-]?app[_-]?token|github[_-]?token|openai[_-]?api[_-]?key)\s*[=:]\s*['\"]?[A-Za-z0-9._~+/=-]{8,}"
)
GITHUB_PAT_RE = re.compile(r"\bgithub_pat_[A-Za-z0-9_]{8,}\b")
QUOTED_AUTHORIZATION_VALUE_RE = re.compile(
    r"(?i)\b(?:authorization\s*:\s*)?(?:bearer|basic)\s+['\"][^'\"]+['\"]"
)
REVIEWER_PROMPT_MARKERS = (
    "You are a read-only PR reviewer.",
    "VERDICT_SCHEMA:",
    "SNAPSHOT_JSON:",
)


class ReviewError(Exception):
    """Raised when untrusted review input or output is unsafe or malformed."""


@dataclasses.dataclass(frozen=True)
class ChangedFile:
    path: str
    patch: str


@dataclasses.dataclass(frozen=True)
class CheckEvidence:
    name: str
    status: str


@dataclasses.dataclass(frozen=True)
class ReviewSnapshot:
    schema_version: int
    repository: str
    pull_request_number: int
    issue_number: int
    base_sha: str
    head_sha: str
    pr_title: str
    pr_body: str
    issue_title: str
    issue_body: str
    issue_labels: tuple[str, ...]
    declared_risk: str
    trusted_risk_floor: str
    changed_files: tuple[ChangedFile, ...]
    ci_checks: tuple[CheckEvidence, ...]
    worker_metadata: Mapping[str, str]


@dataclasses.dataclass(frozen=True)
class Finding:
    id: str
    category: str
    message: str
    required_action: str
    required_evidence: str


@dataclasses.dataclass(frozen=True)
class ReviewVerdict:
    schema_version: int
    verdict: str
    reviewed_head_sha: str
    effective_risk: str
    summary: str
    findings: tuple[Finding, ...]
    escalation_reason: str


def _mapping(value: Any, name: str) -> Mapping[str, Any]:
    if not isinstance(value, Mapping):
        raise ReviewError("%s must be an object" % name)
    return value


def _exact_keys(value: Mapping[str, Any], keys: frozenset[str], name: str) -> None:
    if set(value) != keys:
        raise ReviewError("%s has missing or unknown fields" % name)


def _text(value: Any, name: str, maximum: int = MAX_TEXT) -> str:
    if not isinstance(value, str) or not value or len(value) > maximum:
        raise ReviewError("%s must be a non-empty bounded string" % name)
    if any(ord(character) < 32 and character not in "\n\r\t" for character in value):
        raise ReviewError("%s contains unsafe control characters" % name)
    if SECRET_RE.search(value) or any(secret and secret in value for secret in _credential_values()):
        raise ReviewError("%s contains credential material" % name)
    return value


def _optional_text(value: Any, name: str, maximum: int) -> str:
    if not isinstance(value, str) or len(value) > maximum:
        raise ReviewError("%s must be a bounded string" % name)
    if any(ord(character) < 32 and character not in "\n\r\t" for character in value):
        raise ReviewError("%s contains unsafe control characters" % name)
    if SECRET_RE.search(value) or any(secret and secret in value for secret in _credential_values()):
        raise ReviewError("%s contains credential material" % name)
    return value


def _credential_values() -> tuple[str, ...]:
    return tuple(os.environ.get(name, "") for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY"))


def _positive_int(value: Any, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise ReviewError("%s must be a positive integer" % name)
    return value


def _risk(value: Any, name: str) -> str:
    if value not in RISKS:
        raise ReviewError("%s is not a recognized risk" % name)
    return value


def validate_snapshot(raw: Mapping[str, Any]) -> ReviewSnapshot:
    """Validate and freeze trusted snapshot data before prompt construction."""
    raw = _mapping(raw, "snapshot")
    _exact_keys(raw, frozenset((
        "schema_version", "repository", "pull_request_number", "issue_number", "base_sha", "head_sha",
        "pr_title", "pr_body", "issue_title", "issue_body", "issue_labels", "declared_risk",
        "trusted_risk_floor", "changed_files", "ci_checks", "worker_metadata",
    )), "snapshot")
    if raw["schema_version"] != SNAPSHOT_SCHEMA_VERSION:
        raise ReviewError("unsupported snapshot schema version")
    repository = _text(raw["repository"], "repository", 200)
    if "/" not in repository:
        raise ReviewError("repository must be an owner/repository identifier")
    shas = []
    for name in ("base_sha", "head_sha"):
        value = raw[name]
        if not isinstance(value, str) or not SHA_RE.fullmatch(value):
            raise ReviewError("%s must be a lowercase 40-character SHA" % name)
        shas.append(value)
    labels = raw["issue_labels"]
    if not isinstance(labels, list) or not labels or len(labels) > MAX_LABELS:
        raise ReviewError("issue_labels must be a bounded non-empty list")
    labels = tuple(_text(value, "issue label", 100) for value in labels)
    files = raw["changed_files"]
    if not isinstance(files, list) or not files or len(files) > MAX_CHANGED_FILES:
        raise ReviewError("changed_files must be a bounded non-empty list")
    changed_files = []
    for item in files:
        item = _mapping(item, "changed file")
        _exact_keys(item, frozenset(("path", "patch")), "changed file")
        path = _text(item["path"], "changed file path", 500)
        if not PATH_RE.fullmatch(path):
            raise ReviewError("changed file path is unsafe")
        changed_files.append(ChangedFile(path, _text(item["patch"], "changed file patch", MAX_PATCH)))
    checks = raw["ci_checks"]
    if not isinstance(checks, list) or len(checks) > MAX_CHECKS:
        raise ReviewError("ci_checks must be a bounded list")
    ci_checks = []
    for item in checks:
        item = _mapping(item, "CI check")
        _exact_keys(item, frozenset(("name", "status")), "CI check")
        status = item["status"]
        if status not in CI_STATUSES:
            raise ReviewError("CI check status is unrecognized")
        ci_checks.append(CheckEvidence(_text(item["name"], "CI check name", 300), status))
    metadata = _mapping(raw["worker_metadata"], "worker metadata")
    _exact_keys(metadata, frozenset(("worker_run_id", "branch")), "worker metadata")
    frozen_metadata = MappingProxyType({key: _text(value, "worker metadata %s" % key, 300) for key, value in metadata.items()})
    return ReviewSnapshot(
        raw["schema_version"], repository, _positive_int(raw["pull_request_number"], "pull_request_number"),
        _positive_int(raw["issue_number"], "issue_number"), shas[0], shas[1],
        _text(raw["pr_title"], "pr_title"), _text(raw["pr_body"], "pr_body"),
        _text(raw["issue_title"], "issue_title"), _text(raw["issue_body"], "issue_body"), labels,
        _risk(raw["declared_risk"], "declared_risk"), _risk(raw["trusted_risk_floor"], "trusted_risk_floor"),
        tuple(changed_files), tuple(ci_checks), frozen_metadata,
    )


def _snapshot_payload(snapshot: ReviewSnapshot) -> dict[str, Any]:
    return {
        "schema_version": snapshot.schema_version, "repository": snapshot.repository,
        "pull_request_number": snapshot.pull_request_number, "issue_number": snapshot.issue_number,
        "base_sha": snapshot.base_sha, "head_sha": snapshot.head_sha, "pr_title": snapshot.pr_title,
        "pr_body": snapshot.pr_body, "issue_title": snapshot.issue_title, "issue_body": snapshot.issue_body,
        "issue_labels": list(snapshot.issue_labels), "declared_risk": snapshot.declared_risk,
        "trusted_risk_floor": snapshot.trusted_risk_floor,
        "changed_files": [{"path": item.path, "patch": item.patch} for item in snapshot.changed_files],
        "ci_checks": [{"name": item.name, "status": item.status} for item in snapshot.ci_checks],
        "worker_metadata": dict(snapshot.worker_metadata),
    }


def build_prompt(snapshot: ReviewSnapshot) -> str:
    """Return the complete, credential-free reviewer prompt for stdin transport."""
    payload = json.dumps(_snapshot_payload(snapshot), sort_keys=True, separators=(",", ":"))
    return (
        "You are a read-only PR reviewer. The supplied snapshot is authoritative immutable control-plane evidence. "
        "Do not query GitHub or require GitHub credentials. Do not edit files, run external side effects, push, create PRs, "
        "change labels/status, merge, or recommend autonomous repairs. Assess the exact issue contract against this exact PR "
        "head and diff: scope and Do-not-change constraints, tests/CI evidence, security boundaries, effective risk, and scientific "
        "ambiguity. RED risk or scientific ambiguity requires verdict escalate. Output only one strict verdict JSON object, no markdown.\n"
        "VERDICT_SCHEMA: {schema_version:1, verdict:clean|blocker|escalate, reviewed_head_sha:string, effective_risk:green|yellow|red, "
        "summary:string, findings:[{id:F-N, category:scope|policy|security|tests|ci|evidence|scientific, message:string, "
        "required_action:string, required_evidence:string}], escalation_reason:string}.\n"
        "SNAPSHOT_JSON:\n" + payload
    )


def parse_verdict(output: str, snapshot: ReviewSnapshot) -> ReviewVerdict:
    """Strictly parse and cross-check untrusted Codex output."""
    if not isinstance(output, str) or not output or output.strip() != output or output.startswith("```"):
        raise ReviewError("reviewer output must be a single unfenced JSON object")
    def reject_duplicate_keys(pairs: Sequence[tuple[str, Any]]) -> dict[str, Any]:
        result = {}
        for key, value in pairs:
            if key in result:
                raise ReviewError("reviewer JSON contains duplicate fields")
            result[key] = value
        return result
    try:
        raw = json.loads(output, object_pairs_hook=reject_duplicate_keys)
    except (TypeError, ValueError) as error:
        raise ReviewError("reviewer output is not JSON") from error
    raw = _mapping(raw, "verdict")
    _exact_keys(raw, frozenset(("schema_version", "verdict", "reviewed_head_sha", "effective_risk", "summary", "findings", "escalation_reason")), "verdict")
    if raw["schema_version"] != VERDICT_SCHEMA_VERSION:
        raise ReviewError("unsupported verdict schema version")
    if raw["verdict"] not in VERDICTS:
        raise ReviewError("verdict is unrecognized")
    if raw["reviewed_head_sha"] != snapshot.head_sha:
        raise ReviewError("verdict reviewed_head_sha does not match snapshot")
    risk = _risk(raw["effective_risk"], "effective_risk")
    risk_order = {"green": 0, "yellow": 1, "red": 2}
    if risk_order[risk] < risk_order[snapshot.trusted_risk_floor]:
        raise ReviewError("reviewer effective risk is below trusted floor")
    findings_raw = raw["findings"]
    if not isinstance(findings_raw, list) or len(findings_raw) > 50:
        raise ReviewError("findings must be a bounded list")
    findings = []
    ids = set()
    for item in findings_raw:
        item = _mapping(item, "finding")
        _exact_keys(item, frozenset(("id", "category", "message", "required_action", "required_evidence")), "finding")
        identifier = item["id"]
        if not isinstance(identifier, str) or not FINDING_ID_RE.fullmatch(identifier) or identifier in ids:
            raise ReviewError("finding id is malformed or duplicated")
        if item["category"] not in FINDING_CATEGORIES:
            raise ReviewError("finding category is unrecognized")
        ids.add(identifier)
        findings.append(Finding(identifier, item["category"], _text(item["message"], "finding message", 1000),
                                _text(item["required_action"], "finding required_action", 1000),
                                _text(item["required_evidence"], "finding required_evidence", 1000)))
    escalation_reason = _optional_text(raw["escalation_reason"], "escalation_reason", 1000)
    verdict = raw["verdict"]
    if risk == "red" and verdict != "escalate":
        raise ReviewError("RED effective risk requires escalation")
    if verdict == "clean" and (findings or escalation_reason):
        raise ReviewError("clean verdict cannot contain findings or escalation")
    if verdict == "blocker" and not findings:
        raise ReviewError("blocker verdict requires actionable findings")
    if verdict == "blocker" and escalation_reason:
        raise ReviewError("blocker verdict cannot contain escalation")
    if verdict == "escalate" and not escalation_reason.strip():
        raise ReviewError("escalate verdict requires an escalation reason")
    return ReviewVerdict(raw["schema_version"], verdict, raw["reviewed_head_sha"], risk,
                         _text(raw["summary"], "summary", 2000), tuple(findings), escalation_reason)


def resolve_codex_executable(executable: str | None = None) -> str:
    configured = executable if executable is not None else os.environ.get("CODEX_EXECUTABLE", "codex")
    if not isinstance(configured, str) or not configured.strip():
        raise ReviewError("CODEX_EXECUTABLE is not configured")
    resolved = shutil.which(configured.strip())
    if resolved:
        return os.path.abspath(resolved)
    if os.path.isfile(configured):
        return os.path.abspath(configured)
    raise ReviewError("configured Codex executable is unavailable")


def reviewer_command(resolved_executable: str, final_output_path: str) -> list[str]:
    """Return credential-free tokens for stdin input and the final-message channel."""
    return [
        resolved_executable, "exec", "--model", REVIEWER_MODEL, "--sandbox", "read-only", "-c", 'approval_policy="never"',
        "--output-last-message", final_output_path, "-",
    ]


def reviewer_environment(parent: Mapping[str, str] | None = None) -> dict[str, str]:
    environment = dict(os.environ if parent is None else parent)
    for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
        environment.pop(name, None)
    environment["GIT_CONFIG_NOSYSTEM"] = "1"
    environment["GIT_CONFIG_GLOBAL"] = os.devnull
    environment["GIT_TERMINAL_PROMPT"] = "0"
    return environment


def _reviewer_failure_tail(text: str) -> str | None:
    """Return a bounded redacted tail without reviewer prompt or diff content."""
    text = text or ""
    markers = [text.find(marker) for marker in REVIEWER_PROMPT_MARKERS if text.find(marker) >= 0]
    if markers:
        text = text[:min(markers)]
    for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
        secret = os.environ.get(name)
        if secret:
            text = text.replace(secret, "[REDACTED]")
    text = green_worker.AUTHORIZATION_VALUE_RE.sub("[REDACTED]", text)
    text = QUOTED_AUTHORIZATION_VALUE_RE.sub("[REDACTED]", text)
    text = green_worker.TOKEN_ASSIGNMENT_RE.sub("[REDACTED]", text)
    text = green_worker.OAUTH_TOKEN_ASSIGNMENT_RE.sub("[REDACTED]", text)
    text = APP_TOKEN_ASSIGNMENT_RE.sub("[REDACTED]", text)
    text = green_worker.JWT_LIKE_TOKEN_RE.sub("[REDACTED]", text)
    text = green_worker.COOKIE_VALUE_RE.sub("[REDACTED]", text)
    text = green_worker.COMMON_API_KEY_RE.sub("[REDACTED]", text)
    text = GITHUB_PAT_RE.sub("[REDACTED]", text)
    text = LOCAL_ABSOLUTE_PATH_RE.sub("[REDACTED_PATH]", text)
    text = re.sub(r"[\x00-\x09\x0b-\x1f\x7f]+", " ", text)

    lines = []
    for line in text.splitlines():
        normalized = " ".join(line.split())
        if (
            not normalized
            or green_worker.REDACTION_ONLY_RE.fullmatch(normalized)
            or normalized.startswith(("diff --git ", "--- ", "+++ ", "@@ ", "+", "-"))
            or "codex exec" in normalized.lower()
        ):
            continue
        lines.append(normalized)
    if not lines:
        return None
    return " | ".join(lines[-MAX_REVIEWER_FAILURE_DIAGNOSTIC_LINES:])


def reviewer_process_failure_diagnostic(returncode: int, stdout: str, stderr: str) -> str:
    """Format a stable, bounded diagnostic for a failed read-only reviewer."""
    if isinstance(returncode, bool) or not isinstance(returncode, int):
        raise ReviewError("reviewer process returned an invalid exit code")
    prefix = "reviewer-process exit %d" % returncode
    summary = _reviewer_failure_tail(stderr if stderr else stdout)
    if not summary:
        return prefix
    available = MAX_REVIEWER_FAILURE_DIAGNOSTIC_CHARS - len(prefix) - 2
    if available <= 0:
        return prefix[:MAX_REVIEWER_FAILURE_DIAGNOSTIC_CHARS]
    if len(summary) > available:
        summary = "..." + summary[-max(0, available - 3):].lstrip()
    return prefix + ": " + summary


def _read_final_reviewer_output(path: str) -> str:
    """Read only a bounded final response, never Codex's mixed progress stream."""
    try:
        size = os.path.getsize(path)
        if size < 1 or size > MAX_REVIEWER_FINAL_OUTPUT_BYTES:
            raise OSError("final reviewer output is missing or oversized")
        with open(path, "r", encoding="utf-8") as handle:
            return handle.read(MAX_REVIEWER_FINAL_OUTPUT_BYTES + 1)
    except (OSError, UnicodeError):
        raise ReviewError("reviewer final result is unavailable") from None


def review_snapshot(snapshot_data: Mapping[str, Any], cwd: str, executable: str | None = None) -> ReviewVerdict:
    """Invoke Codex read-only and return a validated verdict; never mutate GitHub."""
    snapshot = validate_snapshot(snapshot_data)
    prompt = build_prompt(snapshot)
    try:
        with tempfile.TemporaryDirectory(prefix="ml-amstress-a5-reviewer-") as temporary_directory:
            final_output_path = os.path.join(temporary_directory, "final-reviewer-message.json")
            command = reviewer_command(resolve_codex_executable(executable), final_output_path)
            try:
                result = subprocess.run(command, cwd=cwd, env=reviewer_environment(), input=prompt,
                                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False)
            except (OSError, ValueError):
                raise ReviewError("reviewer process could not start") from None
            if result.returncode != 0:
                raise ReviewError(reviewer_process_failure_diagnostic(
                    result.returncode, result.stdout or "", result.stderr or ""
                ))
            return parse_verdict(_read_final_reviewer_output(final_output_path), snapshot)
    except OSError:
        raise ReviewError("reviewer process could not start") from None
