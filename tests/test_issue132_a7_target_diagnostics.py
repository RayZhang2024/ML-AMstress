import json
import subprocess
import tempfile
import unittest
from pathlib import Path

from scripts import a5_repair_worker as repair
from scripts import a6_pr_validation as validation
from scripts import codex_issue_worker as green_worker


HEAD = "a" * 40


def environment(**overrides):
    values = {
        "RUNNER_OS": "Windows", "RUNNER_ARCH": "X64", "RUNNER_NAME": "validation-runner",
        "A7_EXPECTED_VALIDATION_RUNNER_NAME": "validation-runner",
        "A7_EXPECTED_VALIDATION_WINDOWS_USER": "validation-user",
        "A7_VALIDATION_ISOLATION_READY": "isolated",
        "A7_VALIDATION_RUNNER_LABEL": "ml-amstress-abaqus-validation",
        "CODEX_EXPECTED_RUNNER_NAME": "codex-runner", "CODEX_EXPECTED_WINDOWS_USER": "codex-user",
        "A6_APPROVED_LAUNCHER": "C:/SIMULIA/Commands/abq2021.bat", "A6_TIMEOUT_SECONDS": "60",
    }
    values.update(overrides)
    return values


def target_runner(returncode=0, success=False, diagnostic=None):
    def run(command, **kwargs):
        if command[1] == "information=release":
            return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "raw output must not escape")
        if success:
            Path(kwargs["env"][validation.TARGET_SENTINEL_ENVIRONMENT]).write_bytes(
                validation.PARTITION_SENTINEL.encode("ascii"))
        if diagnostic is not None:
            Path(kwargs["env"][validation.TARGET_DIAGNOSTIC_ENVIRONMENT]).write_bytes(diagnostic)
        return subprocess.CompletedProcess(command, returncode, "raw target stdout", "raw target stderr")
    return run


class A72TargetDiagnosticTests(unittest.TestCase):
    def setUp(self):
        self.profile = validation.PROFILES[validation.PARTITION_REGRESSION_PROFILE]

    def run_target(self, runner, **kwargs):
        with tempfile.TemporaryDirectory() as workspace:
            return validation.run_isolated_target_profile(self.profile, workspace, environment(), runner,
                                                          exists=lambda _: True, **kwargs)

    def test_valid_diagnostic_never_replaces_missing_success_sentinel(self):
        result = self.run_target(target_runner(diagnostic=b"model-state-invariant"))
        self.assertEqual(result.failure_category, "target-sentinel-missing")
        self.assertEqual(result.target_diagnostic_stage, "model-state-invariant")
        self.assertEqual(result.outcome, "failed")

    def test_valid_diagnostic_preserves_target_execution_failure(self):
        result = self.run_target(target_runner(returncode=1, diagnostic=b"production-execution"))
        self.assertEqual(result.failure_category, "target-execution-failed")
        self.assertEqual(result.target_diagnostic_stage, "production-execution")
        self.assertEqual(result.outcome, "failed")

    def test_success_marker_without_diagnostic_still_passes(self):
        result = self.run_target(target_runner(success=True))
        self.assertEqual(result, validation.ValidationResult("passed", "2021", "none", "passed", "none"))

    def test_success_marker_and_diagnostic_are_conflicting_evidence(self):
        result = self.run_target(target_runner(success=True, diagnostic=b"fixture-internal"))
        self.assertEqual(result.failure_category, "target-diagnostic-conflict")
        self.assertEqual(result.target_diagnostic_stage, "fixture-internal")
        self.assertEqual(result.outcome, "failed")

    def test_absent_diagnostic_does_not_invent_failure_information(self):
        result = self.run_target(target_runner())
        self.assertEqual(result.failure_category, "target-sentinel-missing")
        self.assertEqual(result.target_diagnostic_stage, "none")

    def test_stale_and_invalid_diagnostics_fail_closed_without_raw_content(self):
        stale = self.run_target(target_runner(), marker_exists=lambda path: path.endswith(validation.TARGET_DIAGNOSTIC_FILENAME))
        self.assertEqual((stale.failure_category, stale.target_diagnostic_stage), ("target-diagnostic-stale", "invalid"))
        for raw in (b"unlisted-stage", b"fixture-setup\n", b"C:/Users/runner/secret", b"::set-output name=x::y",
                    b"GITHUB_TOKEN=secret", b"x" * (validation.MAX_TARGET_DIAGNOSTIC_BYTES + 1)):
            result = self.run_target(target_runner(diagnostic=raw))
            self.assertEqual((result.failure_category, result.target_diagnostic_stage),
                             ("target-sentinel-missing", "invalid"))
            record = validation.evidence(result,
                                         validation.ValidationInputs(1320, 132, HEAD,
                                                                    validation.PARTITION_REGRESSION_PROFILE),
                                         "risk:yellow", "34130000001", "b" * 40)
            rendered = json.dumps(record, sort_keys=True)
            self.assertNotIn(raw.decode("ascii", "ignore"), rendered)
            self.assertNotIn("raw target", rendered)

        conflict = self.run_target(target_runner(success=True, diagnostic=b"unlisted-stage"))
        self.assertEqual((conflict.failure_category, conflict.target_diagnostic_stage),
                         ("target-diagnostic-conflict", "invalid"))

    def test_diagnostic_path_is_allowlisted_only_for_a72_target_child(self):
        child = validation.target_child_environment(environment(), "success", "diagnostic")
        self.assertEqual(child[validation.TARGET_DIAGNOSTIC_ENVIRONMENT], "diagnostic")
        a71_child = validation.target_child_environment(environment(), "success")
        self.assertNotIn(validation.TARGET_DIAGNOSTIC_ENVIRONMENT, a71_child)
        for forbidden in ("GITHUB_TOKEN", "OPENAI_API_KEY", "CODEX_TOKEN", "ACTIONS_RUNTIME_TOKEN", "SSH_AUTH_SOCK"):
            self.assertNotIn(forbidden, child)

    def test_evidence_keeps_diagnostic_non_authoritative_and_bounded(self):
        inputs = validation.ValidationInputs(1320, 132, HEAD, validation.PARTITION_REGRESSION_PROFILE)
        result = validation.ValidationResult("failed", "2021", "target-sentinel-missing", "passed",
                                             "model-state-read")
        record = validation.evidence(result, inputs, "risk:yellow", "34130000001", "b" * 40)
        self.assertEqual(record["failure_category"], "target-sentinel-missing")
        self.assertEqual(record["target_diagnostic_stage"], "model-state-read")
        self.assertEqual(record["outcome"], "failed")
        self.assertEqual(record["isolation_result"], "passed")
        unsafe = validation.evidence(
            validation.ValidationResult("failed", "2021", "target-sentinel-missing", "passed", "secret-path"),
            inputs, "risk:yellow", "34130000001", "b" * 40)
        self.assertEqual(unsafe["target_diagnostic_stage"], "invalid")
        self.assertNotIn("secret-path", json.dumps(unsafe, sort_keys=True))

    def test_a4_and_a5_cannot_change_diagnostic_controller_paths(self):
        _, denied = green_worker.green_changed_paths(("scripts/a6_pr_validation.py",))
        self.assertEqual(denied, ("scripts/a6_pr_validation.py",))
        self.assertTrue(repair.is_protected_path("scripts/a6_pr_validation.py"))


if __name__ == "__main__":
    unittest.main()
