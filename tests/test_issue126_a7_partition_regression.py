import inspect
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

from scripts import a5_repair_worker as repair
from scripts import a6_pr_validation as validation
from scripts import codex_issue_worker as green_worker


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = (ROOT / ".github" / "workflows" / "a6-pr-validation.yml").read_text(encoding="utf-8")


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


class A72PartitionRegressionTests(unittest.TestCase):
    def test_fixed_contract_has_exact_partial_layer_reference_case(self):
        profile = validation.PROFILES[validation.PARTITION_REGRESSION_PROFILE]
        contract = validation.TARGET_CODE_CONTRACTS[profile.target_contract]
        self.assertTrue(profile.executes_target_code)
        self.assertEqual(contract.fixture_path, validation.PARTITION_FIXTURE_PATH)
        self.assertEqual(contract.required_target_paths,
                         (validation.PARTITION_FIXTURE_PATH, validation.PARTITION_PRODUCTION_PATH))
        self.assertEqual((contract.build_extent, contract.layer_thickness, contract.expected_layer_count),
                         (10.3, 0.5, 21))
        self.assertEqual(contract.expected_part_sets, ("BASE", "BUILD_ALL"))
        self.assertEqual(contract.expected_assembly_sets, tuple("set-%d" % item for item in range(23)))
        self.assertEqual((contract.expected_partial_layer_set, contract.expected_whole_build_set), ("set-21", "set-22"))

    def test_workflow_exposes_only_fixed_profile_on_isolated_runner_after_metadata_gate(self):
        self.assertIn("- partition-layer-sets-regression", WORKFLOW)
        block = WORKFLOW.split("validate-partition-layer-sets-regression:", 1)[1]
        self.assertIn("needs: metadata-gate", block)
        self.assertIn("inputs.validation_profile == 'partition-layer-sets-regression'", block)
        self.assertIn("runs-on: [self-hosted, windows, x64, ml-amstress-abaqus-validation]", block)
        self.assertIn("persist-credentials: false", block)
        self.assertNotIn("target_path", WORKFLOW)
        self.assertNotIn("target_command", WORKFLOW)

    def test_exact_target_fixture_and_production_script_are_required_before_launch(self):
        profile = validation.PROFILES[validation.PARTITION_REGRESSION_PROFILE]
        with tempfile.TemporaryDirectory() as workspace:
            fixture_suffix = os.path.join(*validation.PARTITION_FIXTURE_PATH.split("/"))
            missing_production = lambda path: path.endswith(fixture_suffix)
            result = validation.run_isolated_target_profile(profile, workspace, environment(), exists=missing_production)
        self.assertEqual(result.failure_category, "target-fixture-missing")

    def test_valid_fresh_model_state_sentinel_is_required_for_partition_pass(self):
        profile = validation.PROFILES[validation.PARTITION_REGRESSION_PROFILE]
        commands = []

        def runner(command, **kwargs):
            commands.append(command)
            if command[1] == "information=release":
                return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")
            Path(kwargs["env"][validation.TARGET_SENTINEL_ENVIRONMENT]).write_bytes(
                validation.PARTITION_SENTINEL.encode("ascii"))
            return subprocess.CompletedProcess(command, 0, "", "")

        with tempfile.TemporaryDirectory() as workspace:
            result = validation.run_isolated_target_profile(profile, workspace, environment(), runner,
                                                            exists=lambda _: True)
        self.assertEqual(result, validation.ValidationResult("passed", "2021", "none", "passed"))
        self.assertEqual(commands[-1][1], "cae")
        self.assertTrue(commands[-1][2].endswith(os.path.join(*validation.PARTITION_FIXTURE_PATH.split("/"))))
        self.assertNotIn(validation.PARTITION_PRODUCTION_PATH, commands[-1])

    def test_wrong_missing_or_stale_partition_sentinel_cannot_pass(self):
        profile = validation.PROFILES[validation.PARTITION_REGRESSION_PROFILE]

        def release_only(command, **_):
            return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")

        with tempfile.TemporaryDirectory() as workspace:
            self.assertEqual(validation.run_isolated_target_profile(profile, workspace, environment(), release_only,
                                                                     exists=lambda _: True).failure_category,
                             "target-sentinel-missing")
            self.assertEqual(validation.run_isolated_target_profile(profile, workspace, environment(), release_only,
                                                                     exists=lambda _: True,
                                                                     marker_exists=lambda _: True).failure_category,
                             "target-sentinel-stale")

            def wrong_marker(command, **kwargs):
                if command[1] == "information=release":
                    return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021", "")
                Path(kwargs["env"][validation.TARGET_SENTINEL_ENVIRONMENT]).write_text("wrong", encoding="ascii")
                return subprocess.CompletedProcess(command, 0, "", "")

            self.assertEqual(validation.run_isolated_target_profile(profile, workspace, environment(), wrong_marker,
                                                                     exists=lambda _: True).failure_category,
                             "target-sentinel-missing")

    def test_partition_evidence_is_bounded_and_uses_isolated_role(self):
        inputs = validation.ValidationInputs(1260, 126, "a" * 40, validation.PARTITION_REGRESSION_PROFILE)
        record = validation.evidence(validation.ValidationResult("passed", "2021", "none", "passed"), inputs,
                                     "risk:yellow", "34060000001", "b" * 40)
        rendered = json.dumps(record, sort_keys=True)
        self.assertEqual(record["regression_result"], "passed")
        self.assertEqual(record["runner_role"], validation.ISOLATED_RUNNER_ROLE)
        self.assertLess(len(rendered), 900)
        for forbidden in ("validation-user", "C:/", "license", "secret", "output"):
            self.assertNotIn(forbidden, rendered)

    def test_partition_profile_has_no_reimplemented_partition_algorithm(self):
        source = inspect.getsource(validation.run_isolated_target_profile)
        for implementation_detail in ("PartitionCell", "DatumPlane", "getByBoundingBox", "math.ceil"):
            self.assertNotIn(implementation_detail, source)

    def test_a4_a5_protect_the_a72_controller_and_workflow(self):
        _, denied = green_worker.green_changed_paths(("scripts/a6_pr_validation.py", ".github/workflows/a6-pr-validation.yml"))
        self.assertEqual(len(denied), 2)
        self.assertTrue(repair.is_protected_path("scripts/a6_pr_validation.py"))
        self.assertTrue(repair.is_protected_path(".github/workflows/a6-pr-validation.yml"))


if __name__ == "__main__":
    unittest.main()
