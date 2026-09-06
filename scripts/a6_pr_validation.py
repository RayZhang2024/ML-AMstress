"""Trusted, bounded exact-PR Abaqus validation controller for A6.2.

This controller is always loaded from trusted ``main``.  It authorizes a
specific same-repository review PR, checks out that exact commit only in a
fresh temporary target workspace, and selects commands from this module's
fixed profile table.  Target files never provide commands, policy, or
evidence.
"""
from __future__ import annotations

import dataclasses
import json
import os
import re
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
from typing import Callable, Mapping

from scripts import a5_repair_worker as repair
from scripts import a6_abaqus_preflight as preflight
from scripts import codex_issue_worker as green_worker


REPOSITORY = "RayZhang2024/ML-AMstress"
BASE_BRANCH = "main"
EVIDENCE_PREFIX = "A6_PR_VALIDATION_EVIDENCE="
AUTHORIZATION_PREFIX = "A6_PR_AUTHORIZATION="
SCHEMA_VERSION = 1
RUNNER_ROLE = preflight.RUNNER_ROLE
RUNNER_LABELS = preflight.RUNNER_LABELS
ISOLATED_RUNNER_ROLE = "windows-x64-abaqus-validation"
ISOLATED_RUNNER_LABELS = ("self-hosted", "windows", "x64", "ml-amstress-abaqus-validation")
ISOLATION_READY_VALUE = "isolated"
TARGET_FIXTURE_PATH = "tests/fixtures/a7_1_target_cae_smoke.py"
TARGET_SENTINEL_ENVIRONMENT = "A7_TARGET_SENTINEL_FILE"
TARGET_SENTINEL_FILENAME = "a7-target-smoke.marker"
TARGET_SENTINEL = "A7.1_ISOLATED_TARGET_CAE_SMOKE_PASSED"
MAX_IDENTIFIER = 2_000_000_000
MAX_CHANGED_FILES = 1_000
FILES_PER_PAGE = 100
MAX_FILE_PAGES = MAX_CHANGED_FILES // FILES_PER_PAGE
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
RUN_ID_RE = re.compile(r"^[1-9][0-9]{0,19}$")
PR_ISSUE_REFERENCE_RE = re.compile(r"(?m)^Refs #([1-9][0-9]*)\s*$")
STATUS_LABELS = frozenset(("status:ready", "status:in-progress", "status:review", "status:blocked"))
RISK_LABELS = frozenset(("risk:green", "risk:yellow", "risk:red"))
CREDENTIAL_NAMES = frozenset((
    "GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN",
    "CODEX_API_KEY", "CODEX_AUTH_TOKEN", "CODEX_TOKEN", "API_TOKEN",
    "SSH_AUTH_SOCK",
))


class ValidationError(Exception):
    """A stable, bounded A6.2 authorization or runtime failure."""


@dataclasses.dataclass(frozen=True)
class ValidationInputs:
    pull_request_number: int
    issue_number: int
    expected_head_sha: str
    profile: str


@dataclasses.dataclass(frozen=True)
class ValidationProfile:
    identifier: str
    timeout_seconds: int
    executes_target_code: bool


@dataclasses.dataclass(frozen=True)
class ValidationResult:
    outcome: str
    release: str
    failure_category: str
    isolation_result: str = "not-applicable"


# This is intentionally the only enabled profile. It invokes controller-owned
# A6.1 code and never imports, executes, or configures target-branch content.
PROFILES = {
    "inert-cae-runtime-probe": ValidationProfile("inert-cae-runtime-probe", 120, False),
    "isolated-target-cae-smoke": ValidationProfile("isolated-target-cae-smoke", 120, True),
}
FAILURE_CATEGORIES = frozenset((
    "none", "controller-contract", "metadata-rejected", "metadata-race",
    "protected-path", "profile-unknown", "target-code-isolation", "target-checkout-failed",
    "target-head-stale", "timeout", "runtime-unavailable", "probe-failed", "internal-error",
    "target-identity", "target-fixture-missing", "target-sentinel-missing", "target-sentinel-stale",
    "target-execution-failed",
))


def _positive(value: object, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, str) or not value.isdecimal():
        raise ValidationError(name + " is malformed")
    number = int(value)
    if not 1 <= number <= MAX_IDENTIFIER:
        raise ValidationError(name + " is outside the approved bound")
    return number


def parse_inputs(environment: Mapping[str, str]) -> ValidationInputs:
    """Accept exactly four bounded dispatch inputs; no command-like input exists."""
    inputs = ValidationInputs(
        _positive(environment.get("A6_TARGET_PR_NUMBER", ""), "target PR number"),
        _positive(environment.get("A6_TARGET_ISSUE_NUMBER", ""), "target issue number"),
        environment.get("A6_EXPECTED_HEAD_SHA", "").strip(),
        environment.get("A6_VALIDATION_PROFILE", "").strip(),
    )
    if not SHA_RE.fullmatch(inputs.expected_head_sha):
        raise ValidationError("expected head SHA is malformed")
    if inputs.profile not in PROFILES:
        raise ValidationError("profile-unknown")
    return inputs


def validate_controller_environment(environment: Mapping[str, str]) -> tuple[str, str]:
    """Refuse a manual dispatch from any repository/ref other than trusted main."""
    if environment.get("GITHUB_REPOSITORY") != REPOSITORY:
        raise ValidationError("controller repository is not trusted")
    if environment.get("GITHUB_REF") != "refs/heads/main":
        raise ValidationError("controller ref is not trusted main")
    run_id = environment.get("GITHUB_RUN_ID", "").strip()
    sha = environment.get("GITHUB_SHA", "").strip()
    if not RUN_ID_RE.fullmatch(run_id) or not SHA_RE.fullmatch(sha):
        raise ValidationError("controller GitHub identity is unavailable")
    return run_id, sha


def _label_names(item: Mapping[str, object], prefix: str) -> tuple[str, ...]:
    values = item.get("labels", ())
    names = []
    if not isinstance(values, list):
        raise ValidationError("metadata labels are malformed")
    for value in values:
        name = value.get("name") if isinstance(value, dict) else value
        if isinstance(name, str) and name.startswith(prefix):
            names.append(name)
    return tuple(sorted(names))


def linked_issue_number(pr: Mapping[str, object]) -> int:
    """Require exactly one canonical, line-only ``Refs #N`` PR reference."""
    body = pr.get("body")
    if not isinstance(body, str):
        raise ValidationError("PR issue linkage is missing")
    matches = PR_ISSUE_REFERENCE_RE.findall(body)
    if len(matches) != 1:
        raise ValidationError("PR issue linkage is ambiguous")
    return int(matches[0])


def protected_path(path: str) -> bool:
    """Reuse the established A4/A5 protection surface, never target policy."""
    normalized = path.replace("\\", "/")
    return (
        repair.is_protected_path(normalized)
        or normalized.startswith(green_worker.PROTECTED_CONTROL_PLANE_ROOTS)
        or normalized in green_worker.PROTECTED_CONTROL_PLANE_FILES
    )


def authorization_snapshot(pr: Mapping[str, object], issue: Mapping[str, object]) -> tuple[object, ...]:
    """Return only authorization-relevant live state for an intra-gate race check."""
    if not isinstance(pr, Mapping) or not isinstance(issue, Mapping):
        raise ValidationError("metadata identity is malformed")
    base = pr.get("base") if isinstance(pr.get("base"), dict) else {}
    head = pr.get("head") if isinstance(pr.get("head"), dict) else {}
    repo = head.get("repo") if isinstance(head.get("repo"), dict) else {}
    return (
        pr.get("number"), pr.get("state"), base.get("ref"), head.get("sha"), repo.get("full_name"),
        pr.get("changed_files"), pr.get("body"), issue.get("number"), issue.get("state"),
        _label_names(issue, "status:"), _label_names(issue, "risk:"),
    )


def validate_authorization_metadata(pr: Mapping[str, object], issue: Mapping[str, object],
                                    inputs: ValidationInputs) -> str:
    """Validate all non-file authorization facts without inferring from target content."""
    if pr.get("number") != inputs.pull_request_number:
        raise ValidationError("target PR identity is malformed")
    if issue.get("number") != inputs.issue_number:
        raise ValidationError("target issue identity is malformed")
    changed_files = pr.get("changed_files")
    if isinstance(changed_files, bool) or not isinstance(changed_files, int) or not 0 <= changed_files <= MAX_CHANGED_FILES:
        raise ValidationError("target PR changed-file count is malformed")
    if pr.get("state", "").casefold() != "open":
        raise ValidationError("target PR is not open")
    base = pr.get("base") if isinstance(pr.get("base"), dict) else {}
    head = pr.get("head") if isinstance(pr.get("head"), dict) else {}
    repo = head.get("repo") if isinstance(head.get("repo"), dict) else {}
    if base.get("ref") != BASE_BRANCH:
        raise ValidationError("target PR base is not main")
    if repo.get("full_name") != REPOSITORY:
        raise ValidationError("target PR is not same-repository")
    if head.get("sha") != inputs.expected_head_sha:
        raise ValidationError("target PR head is stale")
    if linked_issue_number(pr) != inputs.issue_number:
        raise ValidationError("PR does not reference the requested issue")
    if issue.get("state", "").casefold() != "open":
        raise ValidationError("target issue is not open")
    statuses = _label_names(issue, "status:")
    risks = _label_names(issue, "risk:")
    if len(statuses) != 1 or statuses[0] not in STATUS_LABELS:
        raise ValidationError("target issue status is ambiguous")
    if len(risks) != 1 or risks[0] not in RISK_LABELS:
        raise ValidationError("target issue risk is ambiguous")
    if statuses[0] != "status:review":
        raise ValidationError("target issue is not in review")
    if risks[0] == "risk:red":
        raise ValidationError("red target work is not authorized")
    return risks[0]


def validate_metadata(pr: Mapping[str, object], issue: Mapping[str, object],
                      paths: tuple[str, ...], inputs: ValidationInputs) -> str:
    """Validate complete authorization metadata, including reconciled changed paths."""
    risk = validate_authorization_metadata(pr, issue, inputs)
    if len(paths) != pr.get("changed_files"):
        raise ValidationError("target PR file enumeration is incomplete")
    if any(protected_path(path) for path in paths):
        raise ValidationError("protected-path")
    return risk


def stripped_target_environment(parent: Mapping[str, str]) -> dict[str, str]:
    """Preserve only runtime/license context while removing reusable credentials."""
    environment = dict(parent)
    for name in list(environment):
        upper = name.upper()
        namespace_credential = (
            upper.startswith(("GITHUB_", "GH_", "ACTIONS_", "OPENAI_", "CODEX_", "AUTOMATION_", "REPOSITORY_"))
            and any(marker in upper for marker in ("TOKEN", "KEY", "SECRET", "CREDENTIAL", "AUTH"))
        )
        if name in CREDENTIAL_NAMES or upper in ("API_TOKEN", "API_KEY") or namespace_credential:
            environment.pop(name, None)
    environment["GIT_TERMINAL_PROMPT"] = "0"
    environment["GIT_CONFIG_NOSYSTEM"] = "1"
    environment["GIT_CONFIG_GLOBAL"] = os.devnull
    return environment


def _git(command: list[str], cwd: str, environment: Mapping[str, str], timeout: int,
         runner: Callable[..., subprocess.CompletedProcess[str]]) -> subprocess.CompletedProcess[str]:
    return runner(command, cwd=cwd, env=stripped_target_environment(environment), timeout=timeout,
                  stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False)


def checkout_exact_target(workspace: str, expected_sha: str, environment: Mapping[str, str],
                          runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run) -> None:
    """Fetch only the expected immutable commit into a new credential-free workspace."""
    for command in (
        ["git", "init", "-q"],
        ["git", "remote", "add", "origin", "https://github.com/" + REPOSITORY + ".git"],
        ["git", "fetch", "--depth=1", "origin", expected_sha],
        ["git", "checkout", "--detach", "--quiet", "FETCH_HEAD"],
        ["git", "rev-parse", "HEAD"],
    ):
        result = _git(command, workspace, environment, 60, runner)
        if result.returncode != 0:
            raise ValidationError("target-checkout-failed")
        if command[-1] == "HEAD" and result.stdout.strip() != expected_sha:
            raise ValidationError("target-head-stale")


def validate_isolated_target_identity(environment: Mapping[str, str], user: str | None = None) -> None:
    """Require the externally provisioned validation identity before target code."""
    import getpass
    actual_user = (user if user is not None else getpass.getuser()).strip()
    required = (
        environment.get("RUNNER_OS", "").casefold() == "windows",
        environment.get("RUNNER_ARCH", "").upper() == "X64",
        environment.get("A7_VALIDATION_RUNNER_LABEL", "") == ISOLATED_RUNNER_LABELS[-1],
        bool(environment.get("A7_EXPECTED_VALIDATION_RUNNER_NAME", "").strip()),
        bool(environment.get("A7_EXPECTED_VALIDATION_WINDOWS_USER", "").strip()),
        environment.get("RUNNER_NAME", "").casefold() == environment.get("A7_EXPECTED_VALIDATION_RUNNER_NAME", "").casefold(),
        actual_user.casefold() == environment.get("A7_EXPECTED_VALIDATION_WINDOWS_USER", "").casefold(),
        environment.get("A7_VALIDATION_ISOLATION_READY", "") == ISOLATION_READY_VALUE,
    )
    if not all(required):
        raise ValidationError("target-identity")
    # Explicitly reject an identity configured for the Codex worker even if a
    # maintainer accidentally adds the validation label to that runner.
    for actual, codex in ((environment.get("RUNNER_NAME", ""), environment.get("CODEX_EXPECTED_RUNNER_NAME", "")),
                          (actual_user, environment.get("CODEX_EXPECTED_WINDOWS_USER", ""))):
        if codex.strip() and actual.casefold() == codex.strip().casefold():
            raise ValidationError("target-identity")


def _exact_target_sentinel(path: str) -> bool:
    try:
        with open(path, "rb") as stream:
            value = stream.read(len(TARGET_SENTINEL.encode("ascii")) + 1)
    except OSError:
        return False
    return value == TARGET_SENTINEL.encode("ascii")


def _target_process(command: list[str], workspace: str, environment: Mapping[str, str], timeout: int,
                    runner: Callable[..., subprocess.CompletedProcess[str]]) -> subprocess.CompletedProcess[str]:
    return runner(command, cwd=workspace, env=stripped_target_environment(environment), timeout=timeout,
                  stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False)


def run_isolated_target_smoke(workspace: str, environment: Mapping[str, str],
                              runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
                              exists: Callable[[str], bool] = os.path.isfile,
                              marker_exists: Callable[[str], bool] = os.path.lexists) -> ValidationResult:
    """Run only the fixed future inert fixture from the exact target workspace."""
    fixture = os.path.join(workspace, *TARGET_FIXTURE_PATH.split("/"))
    if not exists(fixture):
        return ValidationResult("failed", "unavailable", "target-fixture-missing", "passed")
    try:
        launcher = preflight.resolve_approved_launcher(environment.get("A6_APPROVED_LAUNCHER"), exists)
        timeout = preflight.configured_timeout(environment.get("A6_TIMEOUT_SECONDS"))
        version = _target_process([launcher, "information=release"], workspace, environment, timeout, runner)
        release = preflight.parse_abaqus_release((version.stdout or "") + "\n" + (version.stderr or ""))
        if version.returncode != 0:
            return ValidationResult("unavailable", "unavailable", "runtime-unavailable", "passed")
        if release != preflight.EXPECTED_ABAQUS_RELEASE:
            return ValidationResult("failed", release, "probe-failed", "passed")
        marker = os.path.join(workspace, TARGET_SENTINEL_FILENAME)
        if marker_exists(marker):
            return ValidationResult("failed", release, "target-sentinel-stale", "passed")
        child = dict(environment)
        child[TARGET_SENTINEL_ENVIRONMENT] = marker
        result = _target_process([launcher, "cae", "noGUI=" + fixture], workspace, child, timeout, runner)
        if result.returncode != 0:
            return ValidationResult("failed", release, "target-execution-failed", "passed")
        if not _exact_target_sentinel(marker):
            return ValidationResult("failed", release, "target-sentinel-missing", "passed")
        return ValidationResult("passed", release, "none", "passed")
    except subprocess.TimeoutExpired:
        return ValidationResult("failed", "unavailable", "timeout", "passed")
    except (OSError, ValueError, preflight.PreflightError):
        return ValidationResult("unavailable", "unavailable", "runtime-unavailable", "passed")


def run_profile(profile: ValidationProfile, target_workspace: str, environment: Mapping[str, str]) -> ValidationResult:
    """Run an allowlisted controller-owned profile; the initial profile ignores target files."""
    if profile.identifier == "isolated-target-cae-smoke" and profile.executes_target_code:
        validate_isolated_target_identity(environment)
        return run_isolated_target_smoke(target_workspace, environment)
    if profile.executes_target_code:
        raise ValidationError("target-code-isolation")
    if profile.identifier != "inert-cae-runtime-probe" or not os.path.isdir(target_workspace):
        raise ValidationError("profile-unknown")
    profile_environment = stripped_target_environment(environment)
    profile_environment["A6_TIMEOUT_SECONDS"] = str(profile.timeout_seconds)
    result = preflight.run_preflight(environment=profile_environment)
    category = result.failure_category
    if category not in ("none", "runtime-unavailable", "timeout", "probe-failed"):
        category = "probe-failed"
    return ValidationResult(result.outcome, result.release, category)


def evidence(result: ValidationResult, inputs: ValidationInputs, effective_risk: str,
             run_id: str, controller_sha: str) -> dict[str, object]:
    """Construct the sole bounded A6.2 evidence record."""
    category = result.failure_category if result.failure_category in FAILURE_CATEGORIES else "internal-error"
    return {
        "schema_version": SCHEMA_VERSION,
        "github_run_id": run_id,
        "trusted_controller_sha": controller_sha,
        "target_pr_number": inputs.pull_request_number,
        "target_issue_number": inputs.issue_number,
        "target_head_sha": inputs.expected_head_sha,
        "effective_risk": effective_risk,
        "validation_profile": inputs.profile,
        "runner_role": ISOLATED_RUNNER_ROLE if inputs.profile == "isolated-target-cae-smoke" else RUNNER_ROLE,
        "runner_labels": list(ISOLATED_RUNNER_LABELS if inputs.profile == "isolated-target-cae-smoke" else RUNNER_LABELS),
        "isolation_result": result.isolation_result,
        "approved_abaqus_command": preflight.APPROVED_ABAQUS_COMMAND_ID,
        "abaqus_release": result.release,
        "outcome": result.outcome,
        "failure_category": category,
    }


class GitHubClient:
    """Read-only REST client; all exceptions are intentionally bounded."""
    def __init__(self, token: str | None):
        if not token:
            raise ValidationError("trusted GitHub read token is unavailable")
        self.token = token

    def _get(self, path: str) -> object:
        request = urllib.request.Request("https://api.github.com" + path, headers={
            "Accept": "application/vnd.github+json", "Authorization": "Bearer " + self.token,
            "User-Agent": "ml-amstress-a6-validation",
        })
        try:
            with urllib.request.urlopen(request, timeout=20) as response:
                return json.loads(response.read().decode("utf-8"))
        except (urllib.error.URLError, urllib.error.HTTPError, ValueError, OSError):
            raise ValidationError("trusted GitHub metadata is unavailable") from None

    def pr(self, number: int) -> Mapping[str, object]:
        value = self._get("/repos/" + REPOSITORY + "/pulls/" + str(number))
        if not isinstance(value, dict):
            raise ValidationError("trusted GitHub metadata is malformed")
        return value

    def issue(self, number: int) -> Mapping[str, object]:
        value = self._get("/repos/" + REPOSITORY + "/issues/" + str(number))
        if not isinstance(value, dict):
            raise ValidationError("trusted GitHub metadata is malformed")
        return value

    def files(self, number: int, expected_count: int) -> tuple[str, ...]:
        """Enumerate every PR file page, bounded and reconciled to PR metadata."""
        if isinstance(expected_count, bool) or not isinstance(expected_count, int) or not 0 <= expected_count <= MAX_CHANGED_FILES:
            raise ValidationError("trusted GitHub file metadata is malformed")
        paths = []
        for page in range(1, MAX_FILE_PAGES + 1):
            if len(paths) >= expected_count:
                break
            value = self._get("/repos/" + REPOSITORY + "/pulls/" + str(number)
                              + "/files?per_page=" + str(FILES_PER_PAGE) + "&page=" + str(page))
            if not isinstance(value, list) or len(value) > FILES_PER_PAGE:
                raise ValidationError("trusted GitHub file metadata is malformed")
            if not value:
                break
            for item in value:
                path = item.get("filename") if isinstance(item, dict) else None
                if not isinstance(path, str) or not path or len(path) > 240:
                    raise ValidationError("trusted GitHub file metadata is malformed")
                paths.append(path)
        if len(paths) != expected_count or len(set(paths)) != len(paths):
            raise ValidationError("trusted GitHub file metadata is incomplete")
        return tuple(sorted(paths))


def resolve_metadata(client: object, inputs: ValidationInputs) -> str:
    """Authorize only a complete changed-file set bound to one stable live snapshot."""
    initial_pr = client.pr(inputs.pull_request_number)
    initial_issue = client.issue(inputs.issue_number)
    initial_snapshot = authorization_snapshot(initial_pr, initial_issue)
    validate_authorization_metadata(initial_pr, initial_issue, inputs)
    paths = client.files(inputs.pull_request_number, initial_pr.get("changed_files"))

    # PR files are paginated separately from PR/issue metadata. Re-read those
    # facts after every page sequence and reject any mixed live observation.
    current_pr = client.pr(inputs.pull_request_number)
    current_issue = client.issue(inputs.issue_number)
    if authorization_snapshot(current_pr, current_issue) != initial_snapshot:
        raise ValidationError("metadata-race")
    return validate_metadata(current_pr, current_issue, paths, inputs)


def authorization_evidence(inputs: ValidationInputs, risk: str, run_id: str, controller_sha: str) -> dict[str, object]:
    """Emit the hosted gate's only bounded handoff record."""
    return {
        "schema_version": SCHEMA_VERSION,
        "github_run_id": run_id,
        "trusted_controller_sha": controller_sha,
        "target_pr_number": inputs.pull_request_number,
        "target_issue_number": inputs.issue_number,
        "target_head_sha": inputs.expected_head_sha,
        "effective_risk": risk,
        "validation_profile": inputs.profile,
        "authorization": "passed",
    }


def execute(client: object, inputs: ValidationInputs, environment: Mapping[str, str],
            git_runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run) -> dict[str, object]:
    """Re-check live metadata on self-hosted runner, then run one inert profile."""
    run_id, controller_sha = validate_controller_environment(environment)
    effective_risk = "unavailable"
    result = ValidationResult("failed", "unavailable", "metadata-rejected")
    try:
        # This is deliberately the second gate: the hosted gate has already
        # completed, but its observation may not authorize a later runtime.
        effective_risk = resolve_metadata(client, inputs)
        profile = PROFILES[inputs.profile]
        if profile.executes_target_code:
            validate_isolated_target_identity(environment)
        with tempfile.TemporaryDirectory(prefix="ml-amstress-a6-target-") as target_workspace:
            checkout_exact_target(target_workspace, inputs.expected_head_sha, environment, git_runner)
            result = run_profile(profile, target_workspace, environment)
    except ValidationError as error:
        category = str(error)
        if category not in FAILURE_CATEGORIES:
            category = "metadata-rejected"
        isolation = "failed" if inputs.profile == "isolated-target-cae-smoke" and category == "target-identity" else "not-applicable"
        result = ValidationResult("failed", "unavailable", category, isolation)
    except subprocess.TimeoutExpired:
        result = ValidationResult("failed", "unavailable", "timeout")
    except Exception:
        result = ValidationResult("failed", "unavailable", "internal-error")
    return evidence(result, inputs, effective_risk, run_id, controller_sha)


def main() -> int:
    environment = dict(os.environ)
    try:
        inputs = parse_inputs(environment)
        client = GitHubClient(environment.get("GITHUB_TOKEN"))
        if sys.argv[1:] == ["--metadata-gate"]:
            run_id, controller_sha = validate_controller_environment(environment)
            record = authorization_evidence(inputs, resolve_metadata(client, inputs), run_id, controller_sha)
            print(AUTHORIZATION_PREFIX + json.dumps(record, sort_keys=True, separators=(",", ":")))
            return 0
        if sys.argv[1:]:
            raise ValidationError("unsupported controller argument")
        record = execute(client, inputs, environment)
    except ValidationError:
        # No untrusted input or exception detail may reach Actions logs.
        return 1
    print(EVIDENCE_PREFIX + json.dumps(record, sort_keys=True, separators=(",", ":")))
    return 0 if record["outcome"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
