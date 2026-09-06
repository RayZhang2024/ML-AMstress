import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a6_pr_validation as validation
from scripts import codex_issue_worker as green_worker


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = (ROOT / ".github" / "workflows" / "a6-pr-validation.yml").read_text(encoding="utf-8")
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


class A71IsolatedTargetValidationTests(unittest.TestCase):
    def test_dedicated_target_job_requires_hosted_gate_and_never_generic_runner(self):
        self.assertIn("validate-isolated-target-code:", WORKFLOW)
        self.assertIn("needs: metadata-gate", WORKFLOW)
        self.assertIn("runs-on: [self-hosted, windows, x64, ml-amstress-abaqus-validation]", WORKFLOW)
        block = WORKFLOW.split("validate-isolated-target-code:", 1)[1]
        self.assertNotIn("ml-amstress-abaqus]", block)
        self.assertIn("persist-credentials: false", block)

    def test_identity_requires_dedicated_windows_x64_name_user_and_external_setup(self):
        validation.validate_isolated_target_identity(environment(), "validation-user")
        for changed in (
            {"RUNNER_OS": "Linux"}, {"RUNNER_ARCH": "ARM64"},
            {"A7_VALIDATION_RUNNER_LABEL": "ml-amstress-abaqus"},
            {"RUNNER_NAME": "other"}, {"A7_EXPECTED_VALIDATION_WINDOWS_USER": "other"},
            {"A7_VALIDATION_ISOLATION_READY": ""},
        ):
            with self.assertRaisesRegex(validation.ValidationError, "target-identity"):
                validation.validate_isolated_target_identity(environment(**changed), "validation-user")

    def test_codex_maintainer_identity_is_explicitly_rejected(self):
        env = environment(RUNNER_NAME="codex-runner", A7_EXPECTED_VALIDATION_RUNNER_NAME="codex-runner")
        with self.assertRaisesRegex(validation.ValidationError, "target-identity"):
            validation.validate_isolated_target_identity(env, "validation-user")
        env = environment(A7_EXPECTED_VALIDATION_WINDOWS_USER="codex-user")
        with self.assertRaisesRegex(validation.ValidationError, "target-identity"):
            validation.validate_isolated_target_identity(env, "codex-user")

    def test_fixed_profile_and_fixture_path_are_controller_owned(self):
        profile = validation.PROFILES["isolated-target-cae-smoke"]
        self.assertTrue(profile.executes_target_code)
        self.assertEqual(validation.TARGET_FIXTURE_PATH, "tests/fixtures/a7_1_target_cae_smoke.py")
        self.assertNotIn("target_path", WORKFLOW)
        self.assertNotIn("target_command", WORKFLOW)
        with self.assertRaises(validation.ValidationError):
            validation.parse_inputs({"A6_TARGET_PR_NUMBER": "1", "A6_TARGET_ISSUE_NUMBER": "1",
                                     "A6_EXPECTED_HEAD_SHA": HEAD, "A6_VALIDATION_PROFILE": "anything"})

    def test_valid_fresh_target_sentinel_is_required_for_pass(self):
        def runner(command, **kwargs):
            if command[1] == "information=release":
                return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")
            Path(kwargs["env"][validation.TARGET_SENTINEL_ENVIRONMENT]).write_bytes(validation.TARGET_SENTINEL.encode("ascii"))
            return subprocess.CompletedProcess(command, 0, "", "")
        with tempfile.TemporaryDirectory() as workspace:
            fixture = Path(workspace, *validation.TARGET_FIXTURE_PATH.split("/"))
            fixture.parent.mkdir(parents=True)
            fixture.write_text("# future inert fixture", encoding="utf-8")
            result = validation.run_isolated_target_smoke(workspace, environment(), runner, exists=lambda _: True)
        self.assertEqual(result, validation.ValidationResult("passed", "2021", "none", "passed"))

    def test_zero_exit_missing_wrong_or_stale_sentinel_cannot_pass(self):
        def runner(command, **kwargs):
            if command[1] == "information=release":
                return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")
            return subprocess.CompletedProcess(command, 0, "", "")
        with tempfile.TemporaryDirectory() as workspace:
            fixture = Path(workspace, *validation.TARGET_FIXTURE_PATH.split("/"))
            fixture.parent.mkdir(parents=True)
            fixture.write_text("# inert", encoding="utf-8")
            self.assertEqual(validation.run_isolated_target_smoke(workspace, environment(), runner, exists=lambda _: True).failure_category,
                             "target-sentinel-missing")
            self.assertEqual(validation.run_isolated_target_smoke(workspace, environment(), runner, exists=lambda _: True,
                             marker_exists=lambda _: True).failure_category, "target-sentinel-stale")
            def wrong(command, **kwargs):
                if command[1] == "information=release":
                    return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")
                Path(kwargs["env"][validation.TARGET_SENTINEL_ENVIRONMENT]).write_text("wrong", encoding="ascii")
                return subprocess.CompletedProcess(command, 0, "", "")
            self.assertEqual(validation.run_isolated_target_smoke(workspace, environment(), wrong, exists=lambda _: True).failure_category,
                             "target-sentinel-missing")

    def test_timeout_and_credentials_are_bounded_and_stripped(self):
        def timeout(*args, **kwargs):
            raise subprocess.TimeoutExpired(args[0], kwargs["timeout"])
        with tempfile.TemporaryDirectory() as workspace:
            fixture = Path(workspace, *validation.TARGET_FIXTURE_PATH.split("/"))
            fixture.parent.mkdir(parents=True)
            fixture.write_text("# inert", encoding="utf-8")
            self.assertEqual(validation.run_isolated_target_smoke(workspace, environment(), timeout, exists=lambda _: True).failure_category,
                             "timeout")
        child = validation.stripped_target_environment(environment(GITHUB_TOKEN="secret", OPENAI_API_KEY="secret",
                                                                     ACTIONS_RUNTIME_TOKEN="secret", SSH_AUTH_SOCK="secret",
                                                                     DSLS_LICENSE_FILE="license"))
        for name in ("GITHUB_TOKEN", "OPENAI_API_KEY", "ACTIONS_RUNTIME_TOKEN", "SSH_AUTH_SOCK"):
            self.assertNotIn(name, child)
        self.assertEqual(child["DSLS_LICENSE_FILE"], "license")

    def test_target_evidence_is_bounded_and_sensitive_fields_absent(self):
        inputs = validation.ValidationInputs(1, 2, HEAD, "isolated-target-cae-smoke")
        rendered = json.dumps(validation.evidence(validation.ValidationResult("passed", "2021", "none", "passed"),
                                                   inputs, "risk:yellow", "1", "b" * 40), sort_keys=True)
        self.assertIn(validation.ISOLATED_RUNNER_ROLE, rendered)
        self.assertIn("isolation_result", rendered)
        for forbidden in ("validation-user", "C:/", "license", "secret"):
            self.assertNotIn(forbidden, rendered)

    def test_a4_a5_protect_a7_controller_and_workflow_surfaces(self):
        _, denied = green_worker.green_changed_paths(("scripts/a6_pr_validation.py", ".github/workflows/a6-pr-validation.yml"))
        self.assertEqual(len(denied), 2)
        self.assertTrue(repair.is_protected_path("scripts/a6_pr_validation.py"))
        self.assertTrue(repair.is_protected_path(".github/workflows/a6-pr-validation.yml"))

    def test_target_smoke_does_not_invoke_production_scientific_helpers(self):
        source = __import__("inspect").getsource(validation.run_isolated_target_smoke)
        for helper in ("import_and_partition", "apply_materials", "apply_meshing", "apply_boundary",
                       "build_cae", "create_input", "data_extract", "AM_gui_v7"):
            self.assertNotIn(helper, source)


if __name__ == "__main__":
    unittest.main()
