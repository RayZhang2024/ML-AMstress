"""Trusted, bounded Abaqus/CAE 2021 runner preflight for A6.1.

This module is intentionally separate from every production Abaqus helper.
It only invokes the approved launcher for a release query and the dedicated
no-model probe.  It never opens project files, creates a model, or submits a
job.
"""

from __future__ import annotations

import dataclasses
import getpass
import json
import os
import re
import subprocess
import sys
import tempfile
from typing import Callable, Mapping


APPROVED_ABAQUS_LAUNCHER = r"C:\SIMULIA\Commands\abq2021.bat"
APPROVED_ABAQUS_COMMAND_ID = "abq2021.bat"
EXPECTED_ABAQUS_RELEASE = "2021"
RUNNER_LABELS = ("self-hosted", "windows", "x64", "ml-amstress-abaqus")
RUNNER_ROLE = "windows-x64-abaqus"
PROBE_FILENAME = "a6_abaqus_probe.py"
PROBE_SUCCESS_MARKER = "A6.1_ABAQUS_CAE_PROBE_PASSED"
PROBE_MARKER_ENVIRONMENT = "A6_PROBE_MARKER_FILE"
PROBE_MARKER_FILENAME = "a6-probe-completion.marker"
DEFAULT_TIMEOUT_SECONDS = 60
MAX_TIMEOUT_SECONDS = 120
OUTCOMES = frozenset(("passed", "failed", "unavailable"))
FAILURE_CATEGORIES = frozenset((
    "none", "runner-contract", "launcher-not-approved", "launcher-missing",
    "launcher-unusable", "release-unexpected", "runtime-unavailable",
    "probe-failed", "probe-marker-missing", "probe-marker-stale", "timeout", "internal-error",
))
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
RUN_ID_RE = re.compile(r"^[1-9][0-9]{0,19}$")
RELEASE_RE = re.compile(r"(?i)\b(?:abaqus(?:/cae)?|release)\D{0,24}(20[0-9]{2})\b")
LICENSE_UNAVAILABLE_RE = re.compile(
    r"(?i)\b(?:licen[cs]e(?:s)?|checkout)\b.{0,120}\b(?:unavailable|failed|denied|cannot|not available)\b|"
    r"\b(?:unavailable|failed|denied|cannot|not available)\b.{0,120}\b(?:licen[cs]e(?:s)?|checkout)\b|"
    r"\bno\s+(?:licen[cs]e(?:s)?|checkout)\s+available\b"
)


class PreflightError(Exception):
    """Raised for a deterministic, safe A6.1 configuration failure."""


@dataclasses.dataclass(frozen=True)
class PreflightResult:
    outcome: str
    release: str
    failure_category: str


def _canonical_windows_path(value: str) -> str:
    return value.strip().replace("/", "\\").casefold()


def _runtime_environment(parent: Mapping[str, str] | None = None) -> dict[str, str]:
    """Keep required Abaqus licensing context but never pass GitHub credentials."""
    environment = dict(os.environ if parent is None else parent)
    for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
        environment.pop(name, None)
    return environment


def configured_timeout(value: str | None) -> int:
    """Validate the bounded A6.1 timeout without accepting unbounded input."""
    try:
        timeout = int(value or DEFAULT_TIMEOUT_SECONDS)
    except (TypeError, ValueError):
        raise PreflightError("A6 timeout is not a valid integer") from None
    if timeout < 1 or timeout > MAX_TIMEOUT_SECONDS:
        raise PreflightError("A6 timeout is outside the approved bound")
    return timeout


def resolve_approved_launcher(configured: str | None,
                              exists: Callable[[str], bool] = os.path.isfile) -> str:
    """Resolve only the single approved CAE 2021 launcher; never fall back."""
    if not isinstance(configured, str) or not configured.strip():
        raise PreflightError("launcher-not-approved")
    if _canonical_windows_path(configured) != _canonical_windows_path(APPROVED_ABAQUS_LAUNCHER):
        raise PreflightError("launcher-not-approved")
    if not exists(configured):
        raise PreflightError("launcher-missing")
    return configured


def parse_abaqus_release(output: str) -> str:
    """Extract a release from bounded captured launcher output."""
    match = RELEASE_RE.search(output or "")
    if not match:
        raise PreflightError("release-unexpected")
    return match.group(1)


def validate_runner_environment(environment: Mapping[str, str] | None = None,
                                user: str | None = None) -> tuple[str, ...]:
    """Return safe contract failures without reporting runner names or users."""
    environment = os.environ if environment is None else environment
    errors = []
    if environment.get("RUNNER_OS", "").strip().casefold() != "windows":
        errors.append("runner-os")
    if environment.get("RUNNER_ARCH", "").strip().upper() != "X64":
        errors.append("runner-arch")
    expected_runner = environment.get("A6_EXPECTED_RUNNER_NAME", "").strip()
    actual_runner = environment.get("RUNNER_NAME", "").strip()
    if not expected_runner or not actual_runner or actual_runner.casefold() != expected_runner.casefold():
        errors.append("runner-identity")
    expected_user = environment.get("A6_EXPECTED_WINDOWS_USER", "").strip()
    actual_user = (getpass.getuser() if user is None else user).strip()
    if not expected_user or not actual_user or actual_user.casefold() != expected_user.casefold():
        errors.append("runner-user")
    return tuple(errors)


def _combined_output(result: subprocess.CompletedProcess[str]) -> str:
    return (result.stdout or "") + "\n" + (result.stderr or "")


def _run(command: list[str], cwd: str, timeout: int,
         runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
         environment: Mapping[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    return runner(
        command, cwd=cwd, env=_runtime_environment(environment), timeout=timeout,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False,
    )


def probe_command(launcher: str, probe_path: str) -> list[str]:
    """Build the only permitted CAE command for this infrastructure probe."""
    if os.path.basename(probe_path) != PROBE_FILENAME:
        raise PreflightError("probe-failed")
    return [launcher, "cae", "noGUI=" + os.path.abspath(probe_path)]


def _exact_probe_marker(path: str) -> bool:
    """Read only the bounded fixed marker; never publish its contents."""
    try:
        with open(path, "rb") as marker_file:
            value = marker_file.read(len(PROBE_SUCCESS_MARKER.encode("ascii")) + 1)
    except OSError:
        return False
    return value == PROBE_SUCCESS_MARKER.encode("ascii")


def _result(outcome: str, release: str = "unavailable",
            failure_category: str = "none") -> PreflightResult:
    if outcome not in OUTCOMES or failure_category not in FAILURE_CATEGORIES:
        raise PreflightError("invalid preflight result")
    return PreflightResult(outcome, release, failure_category)


def run_preflight(environment: Mapping[str, str] | None = None,
                  runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
                  exists: Callable[[str], bool] = os.path.isfile,
                  user: str | None = None,
                  probe_path: str | None = None,
                  marker_exists: Callable[[str], bool] = os.path.lexists) -> PreflightResult:
    """Execute only release and inert noGUI probes, returning a bounded outcome."""
    environment = dict(os.environ if environment is None else environment)
    if validate_runner_environment(environment, user):
        return _result("failed", failure_category="runner-contract")
    try:
        timeout = configured_timeout(environment.get("A6_TIMEOUT_SECONDS"))
        launcher = resolve_approved_launcher(environment.get("A6_APPROVED_LAUNCHER"), exists)
    except PreflightError as error:
        category = str(error)
        return _result("unavailable" if category == "launcher-missing" else "failed",
                       failure_category=category)

    workspace_probe = probe_path or os.path.join(os.path.dirname(__file__), PROBE_FILENAME)
    try:
        with tempfile.TemporaryDirectory(prefix="ml-amstress-a6-") as temporary_directory:
            version_result = _run([launcher, "information=release"], temporary_directory, timeout, runner, environment)
            version_output = _combined_output(version_result)
            if version_result.returncode != 0:
                return _result("unavailable" if LICENSE_UNAVAILABLE_RE.search(version_output) else "failed",
                               failure_category="runtime-unavailable" if LICENSE_UNAVAILABLE_RE.search(version_output)
                               else "launcher-unusable")
            release = parse_abaqus_release(version_output)
            if release != EXPECTED_ABAQUS_RELEASE:
                return _result("failed", release, "release-unexpected")

            marker_path = os.path.join(temporary_directory, PROBE_MARKER_FILENAME)
            if marker_exists(marker_path):
                return _result("failed", release, "probe-marker-stale")
            probe_environment = dict(environment)
            probe_environment[PROBE_MARKER_ENVIRONMENT] = marker_path
            cae_result = _run(probe_command(launcher, workspace_probe), temporary_directory, timeout, runner,
                              probe_environment)
            cae_output = _combined_output(cae_result)
            if cae_result.returncode != 0:
                return _result("unavailable" if LICENSE_UNAVAILABLE_RE.search(cae_output) else "failed",
                               release, "runtime-unavailable" if LICENSE_UNAVAILABLE_RE.search(cae_output)
                               else "probe-failed")
            if not _exact_probe_marker(marker_path):
                return _result("failed", release, "probe-marker-missing")
            return _result("passed", release)
    except subprocess.TimeoutExpired:
        return _result("failed", failure_category="timeout")
    except (OSError, ValueError):
        return _result("unavailable", failure_category="launcher-unusable")
    except PreflightError as error:
        return _result("failed", failure_category=str(error))
    except Exception:
        return _result("failed", failure_category="internal-error")


def build_evidence(result: PreflightResult, environment: Mapping[str, str] | None = None) -> dict[str, object]:
    """Build the only bounded, non-secret A6.1 evidence representation."""
    environment = os.environ if environment is None else environment
    run_id = environment.get("GITHUB_RUN_ID", "").strip()
    sha = environment.get("GITHUB_SHA", "").strip()
    if not RUN_ID_RE.fullmatch(run_id) or not SHA_RE.fullmatch(sha):
        raise PreflightError("trusted GitHub run identity is unavailable")
    return {
        "schema_version": 1,
        "github_run_id": run_id,
        "trusted_repository_sha": sha,
        "runner_role": RUNNER_ROLE,
        "runner_labels": list(RUNNER_LABELS),
        "approved_abaqus_command": APPROVED_ABAQUS_COMMAND_ID,
        "abaqus_release": result.release,
        "outcome": result.outcome,
        "failure_category": result.failure_category,
    }


def main() -> int:
    result = run_preflight()
    try:
        evidence = build_evidence(result)
    except PreflightError:
        result = _result("failed", failure_category="runner-contract")
        evidence = {
            "schema_version": 1,
            "github_run_id": "unavailable",
            "trusted_repository_sha": "unavailable",
            "runner_role": RUNNER_ROLE,
            "runner_labels": list(RUNNER_LABELS),
            "approved_abaqus_command": APPROVED_ABAQUS_COMMAND_ID,
            "abaqus_release": result.release,
            "outcome": result.outcome,
            "failure_category": result.failure_category,
        }
    print("A6_PREFLIGHT_EVIDENCE=" + json.dumps(evidence, sort_keys=True, separators=(",", ":")))
    return 0 if result.outcome == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
