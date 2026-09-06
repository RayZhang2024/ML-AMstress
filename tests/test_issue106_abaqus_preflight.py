import json
import subprocess
import unittest
from pathlib import Path

from scripts import a6_abaqus_preflight as preflight


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = (ROOT / ".github" / "workflows" / "a6-abaqus-preflight.yml").read_text(encoding="utf-8")


def environment(**overrides):
    values = {
        "RUNNER_OS": "Windows",
        "RUNNER_ARCH": "X64",
        "RUNNER_NAME": "abaqus-runner",
        "A6_EXPECTED_RUNNER_NAME": "abaqus-runner",
        "A6_EXPECTED_WINDOWS_USER": "abaqus-user",
        "A6_APPROVED_LAUNCHER": "C:/SIMULIA/Commands/abq2021.bat",
        "A6_TIMEOUT_SECONDS": "60",
        "GITHUB_RUN_ID": "33999999999",
        "GITHUB_SHA": "a" * 40,
    }
    values.update(overrides)
    return values


def successful_runner(captured=None):
    def run(command, **kwargs):
        if captured is not None:
            captured.append((command, kwargs))
        if command[1] == "information=release":
            return subprocess.CompletedProcess(command, 0, "Abaqus/CAE 2021\n", "")
        Path(kwargs["env"][preflight.PROBE_MARKER_ENVIRONMENT]).write_bytes(
            preflight.PROBE_SUCCESS_MARKER.encode("ascii")
        )
        return subprocess.CompletedProcess(command, 0, "", "")

    return run


class A61AbaqusPreflightTests(unittest.TestCase):
    def test_trusted_manual_workflow_restricts_execution_to_main(self):
        self.assertIn("workflow_dispatch:", WORKFLOW)
        self.assertNotIn("pull_request:", WORKFLOW)
        self.assertNotIn("workflow_run:", WORKFLOW)
        self.assertIn("github.ref == 'refs/heads/main'", WORKFLOW)
        self.assertIn("github.repository == 'RayZhang2024/ML-AMstress'", WORKFLOW)
        self.assertIn("ref: " + "$" + "{{ github.sha }}", WORKFLOW)
        self.assertIn("persist-credentials: false", WORKFLOW)
        self.assertIn("contents: read", WORKFLOW)
        self.assertNotIn("issues: write", WORKFLOW)
        self.assertNotIn("pull-requests: write", WORKFLOW)

    def test_powershell_wrapper_normalizes_scalar_evidence_output_before_indexing(self):
        command = "[string[]]$output = @(& python -m scripts.a6_abaqus_preflight)"
        self.assertIn(command, WORKFLOW)
        self.assertNotIn("$output = & python -m scripts.a6_abaqus_preflight", WORKFLOW)
        self.assertLess(WORKFLOW.index(command), WORKFLOW.index("$exitCode = $LASTEXITCODE"))
        self.assertIn("$output.Count -ne 1", WORKFLOW)
        self.assertIn("$output[0].StartsWith('A6_PREFLIGHT_EVIDENCE=')", WORKFLOW)
        self.assertIn("exit $exitCode", WORKFLOW)

    def test_runner_label_contract_is_dedicated_and_distinct_from_codex(self):
        self.assertEqual(preflight.RUNNER_LABELS, (
            "self-hosted", "windows", "x64", "ml-amstress-abaqus",
        ))
        self.assertIn("runs-on: [self-hosted, windows, x64, ml-amstress-abaqus]", WORKFLOW)
        self.assertNotIn("ml-amstress-codex", WORKFLOW)
        self.assertEqual(preflight.validate_runner_environment(environment(), "abaqus-user"), ())
        self.assertIn("runner-identity", preflight.validate_runner_environment(
            environment(RUNNER_NAME="other-runner"), "abaqus-user"
        ))

    def test_approved_launcher_resolution_never_falls_back(self):
        launcher = preflight.resolve_approved_launcher(
            "C:/SIMULIA/Commands/abq2021.bat", exists=lambda value: True
        )
        self.assertEqual(launcher, "C:/SIMULIA/Commands/abq2021.bat")
        with self.assertRaisesRegex(preflight.PreflightError, "launcher-not-approved"):
            preflight.resolve_approved_launcher("C:/SIMULIA/Commands/abq2022.bat", exists=lambda value: True)
        with self.assertRaisesRegex(preflight.PreflightError, "launcher-missing"):
            preflight.resolve_approved_launcher("C:/SIMULIA/Commands/abq2021.bat", exists=lambda value: False)

    def test_version_parser_accepts_only_explicit_release_evidence(self):
        self.assertEqual(preflight.parse_abaqus_release("Abaqus/CAE 2021"), "2021")
        with self.assertRaisesRegex(preflight.PreflightError, "release-unexpected"):
            preflight.parse_abaqus_release("launcher ready")

    def test_successful_release_and_inert_probe_pass(self):
        calls = []
        result = preflight.run_preflight(
            environment(), successful_runner(calls), exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(result, preflight.PreflightResult("passed", "2021", "none"))
        self.assertEqual(calls[0][0][1:], ["information=release"])
        self.assertEqual(calls[1][0][1], "cae")
        self.assertTrue(calls[1][0][2].endswith(preflight.PROBE_FILENAME))
        self.assertEqual(Path(calls[1][1]["env"][preflight.PROBE_MARKER_ENVIRONMENT]).name,
                         preflight.PROBE_MARKER_FILENAME)
        self.assertEqual(calls[1][1]["env"][preflight.PROBE_MARKER_ENVIRONMENT],
                         str(Path(calls[1][1]["cwd"]) / preflight.PROBE_MARKER_FILENAME))
        self.assertFalse(Path(calls[1][1]["env"][preflight.PROBE_MARKER_ENVIRONMENT]).exists())

    def test_missing_launcher_is_unavailable_and_unexpected_launcher_fails(self):
        missing = preflight.run_preflight(
            environment(), exists=lambda value: False, user="abaqus-user"
        )
        self.assertEqual(missing, preflight.PreflightResult("unavailable", "unavailable", "launcher-missing"))
        unexpected = preflight.run_preflight(
            environment(A6_APPROVED_LAUNCHER="C:/SIMULIA/Commands/abq2022.bat"),
            exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(unexpected.failure_category, "launcher-not-approved")
        self.assertEqual(unexpected.outcome, "failed")

    def test_license_unavailable_is_not_a_pass(self):
        def unavailable_runner(command, **kwargs):
            return subprocess.CompletedProcess(command, 1, "", "No licenses available")

        result = preflight.run_preflight(
            environment(), unavailable_runner, exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(result, preflight.PreflightResult("unavailable", "unavailable", "runtime-unavailable"))

    def test_unexpected_release_and_missing_probe_marker_fail_closed(self):
        wrong_release = preflight.run_preflight(
            environment(),
            lambda command, **kwargs: subprocess.CompletedProcess(command, 0, "Abaqus 2022", ""),
            exists=lambda value: True, user="abaqus-user",
        )
        self.assertEqual(wrong_release.failure_category, "release-unexpected")

        results = iter((subprocess.CompletedProcess([], 0, "Abaqus 2021", ""),
                        subprocess.CompletedProcess([], 0, "", "")))
        marker_missing = preflight.run_preflight(
            environment(), lambda command, **kwargs: next(results), exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(marker_missing.failure_category, "probe-marker-missing")

    def test_wrong_or_stale_probe_marker_cannot_pass(self):
        def wrong_marker_runner(command, **kwargs):
            if command[1] == "information=release":
                return subprocess.CompletedProcess(command, 0, "Abaqus 2021", "")
            Path(kwargs["env"][preflight.PROBE_MARKER_ENVIRONMENT]).write_text("wrong marker", encoding="ascii")
            return subprocess.CompletedProcess(command, 0, "", "")

        wrong = preflight.run_preflight(
            environment(), wrong_marker_runner, exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(wrong.failure_category, "probe-marker-missing")

        calls = []
        stale = preflight.run_preflight(
            environment(), successful_runner(calls), exists=lambda value: True, user="abaqus-user",
            marker_exists=lambda value: True,
        )
        self.assertEqual(stale.failure_category, "probe-marker-stale")
        self.assertEqual(len(calls), 1)

    def test_nonzero_cae_exit_fails_even_with_a_marker(self):
        def failing_cae_runner(command, **kwargs):
            if command[1] == "information=release":
                return subprocess.CompletedProcess(command, 0, "Abaqus 2021", "")
            Path(kwargs["env"][preflight.PROBE_MARKER_ENVIRONMENT]).write_bytes(
                preflight.PROBE_SUCCESS_MARKER.encode("ascii")
            )
            return subprocess.CompletedProcess(command, 1, "", "")

        result = preflight.run_preflight(
            environment(), failing_cae_runner, exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(result.failure_category, "probe-failed")

    def test_timeout_is_bounded_and_fails_closed(self):
        def timeout_runner(command, **kwargs):
            raise subprocess.TimeoutExpired(command, kwargs["timeout"])

        result = preflight.run_preflight(
            environment(A6_TIMEOUT_SECONDS="1"), timeout_runner, exists=lambda value: True, user="abaqus-user"
        )
        self.assertEqual(result, preflight.PreflightResult("failed", "unavailable", "timeout"))
        with self.assertRaisesRegex(preflight.PreflightError, "approved bound"):
            preflight.configured_timeout("121")

    def test_evidence_is_bounded_and_never_contains_runtime_secrets_or_paths(self):
        env = environment(
            DSLS_LICENSE_FILE="27000@license.internal.example",
            GITHUB_TOKEN="ghp_not_for_evidence",
            USERPROFILE=r"C:\Users\runner",
        )
        evidence = preflight.build_evidence(preflight.PreflightResult("passed", "2021", "none"), env)
        rendered = json.dumps(evidence, sort_keys=True)
        self.assertLess(len(rendered), 500)
        self.assertNotIn("license.internal", rendered)
        self.assertNotIn("ghp_not_for_evidence", rendered)
        self.assertNotIn(r"C:\Users", rendered)
        self.assertEqual(evidence["runner_labels"], list(preflight.RUNNER_LABELS))

    def test_runtime_child_environment_strips_github_credentials(self):
        calls = []
        preflight.run_preflight(
            environment(GITHUB_TOKEN="secret", GH_TOKEN="secret", OPENAI_API_KEY="secret",
                        AUTOMATION_APP_TOKEN="secret"),
            successful_runner(calls), exists=lambda value: True, user="abaqus-user",
        )
        for _, kwargs in calls:
            for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
                self.assertNotIn(name, kwargs["env"])

    def test_probe_scope_cannot_invoke_production_abaqus_helpers(self):
        command = preflight.probe_command(
            preflight.APPROVED_ABAQUS_LAUNCHER, str(ROOT / "scripts" / preflight.PROBE_FILENAME)
        )
        rendered = " ".join(command)
        for helper in (
            "import_and_partition.py", "apply_materials.py", "apply_meshing.py",
            "apply_boundary.py", "build_cae.py", "create_input.py", "data_extract.py",
        ):
            self.assertNotIn(helper, rendered)
        probe_source = (ROOT / "scripts" / preflight.PROBE_FILENAME).read_text(encoding="utf-8")
        self.assertIn(preflight.PROBE_SUCCESS_MARKER, probe_source)
        self.assertIn(preflight.PROBE_MARKER_ENVIRONMENT, probe_source)
        self.assertNotIn("print(", probe_source)
        self.assertNotIn("mdb.", probe_source)
        self.assertNotIn("Job(", probe_source)


if __name__ == "__main__":
    unittest.main()
