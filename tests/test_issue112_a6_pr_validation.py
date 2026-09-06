import inspect
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a6_abaqus_preflight as preflight
from scripts import a6_pr_validation as validation
from scripts import codex_issue_worker as green_worker


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = (ROOT / ".github" / "workflows" / "a6-pr-validation.yml").read_text(encoding="utf-8")
HEAD = "a" * 40


def environment(**overrides):
    values = {
        "GITHUB_REPOSITORY": validation.REPOSITORY,
        "GITHUB_REF": "refs/heads/main",
        "GITHUB_RUN_ID": "34050000001",
        "GITHUB_SHA": "b" * 40,
        "A6_TARGET_PR_NUMBER": "1120",
        "A6_TARGET_ISSUE_NUMBER": "112",
        "A6_EXPECTED_HEAD_SHA": HEAD,
        "A6_VALIDATION_PROFILE": "inert-cae-runtime-probe",
        "RUNNER_OS": "Windows", "RUNNER_ARCH": "X64", "RUNNER_NAME": "abaqus-runner",
        "A6_EXPECTED_RUNNER_NAME": "abaqus-runner", "A6_EXPECTED_WINDOWS_USER": "abaqus-user",
        "A6_APPROVED_LAUNCHER": "C:/SIMULIA/Commands/abq2021.bat", "A6_TIMEOUT_SECONDS": "60",
    }
    values.update(overrides)
    return values


def issue(labels=("status:review", "risk:yellow"), state="open"):
    return {"state": state, "labels": [{"name": name} for name in labels]}


def pr(head=HEAD, state="open", base="main", repo=validation.REPOSITORY, body="Refs #112\n"):
    return {"state": state, "base": {"ref": base}, "head": {"sha": head, "repo": {"full_name": repo}}, "body": body}


class FakeClient:
    def __init__(self, pr_data=None, issue_data=None, paths=("docs/example.md",)):
        self.pr_data = pr_data or pr()
        self.issue_data = issue_data or issue()
        self.paths = paths
        self.calls = 0

    def pr(self, _):
        self.calls += 1
        return self.pr_data

    def issue(self, _):
        return self.issue_data

    def files(self, _):
        return tuple(self.paths)


class A62ExactPrValidationTests(unittest.TestCase):
    def test_workflow_is_manual_trusted_main_read_only_and_no_pr_trigger(self):
        self.assertIn("workflow_dispatch:", WORKFLOW)
        self.assertNotIn("pull_request:", WORKFLOW)
        self.assertNotIn("workflow_run:", WORKFLOW)
        self.assertIn("github.ref == 'refs/heads/main'", WORKFLOW)
        self.assertIn("github.repository == 'RayZhang2024/ML-AMstress'", WORKFLOW)
        self.assertIn("contents: read", WORKFLOW)
        self.assertIn("pull-requests: read", WORKFLOW)
        self.assertIn("issues: read", WORKFLOW)
        self.assertNotIn("write", WORKFLOW)
        self.assertIn("persist-credentials: false", WORKFLOW)
        self.assertIn("ref: " + "$" + "{{ github.sha }}", WORKFLOW)

    def test_inputs_are_bounded_and_command_injection_is_not_an_input_surface(self):
        values = validation.parse_inputs(environment())
        self.assertEqual(values.expected_head_sha, HEAD)
        for changed in ({"A6_TARGET_PR_NUMBER": "1;cmd"}, {"A6_EXPECTED_HEAD_SHA": "bad"},
                        {"A6_VALIDATION_PROFILE": "cmd /c anything"}):
            with self.assertRaises(validation.ValidationError):
                validation.parse_inputs(environment(**changed))
        for forbidden in ("shell_command:", "script_body:", "arguments:", "path_list:"):
            self.assertNotIn(forbidden, WORKFLOW.casefold())

    def test_metadata_gate_accepts_exact_open_same_repo_review_pr(self):
        inputs = validation.parse_inputs(environment())
        self.assertEqual(validation.validate_metadata(pr(), issue(), ("docs/example.md",), inputs), "risk:yellow")

    def test_metadata_rejects_stale_closed_wrong_base_fork_and_linkage(self):
        inputs = validation.parse_inputs(environment())
        for changed in (
            pr(head="c" * 40), pr(state="closed"), pr(base="release"), pr(repo="other/repo"),
            pr(body="Refs #113\n"), pr(body="Refs #112\nRefs #112\n"),
        ):
            with self.assertRaises(validation.ValidationError):
                validation.validate_metadata(changed, issue(), (), inputs)

    def test_metadata_rejects_missing_issue_bad_labels_status_and_red(self):
        inputs = validation.parse_inputs(environment())
        for target in (
            issue(state="closed"), issue(("status:review", "status:ready", "risk:yellow")),
            issue(("status:review", "risk:yellow", "risk:green")), issue(("status:ready", "risk:yellow")),
            issue(("status:review", "risk:red")),
        ):
            with self.assertRaises(validation.ValidationError):
                validation.validate_metadata(pr(), target, (), inputs)

    def test_controller_identity_rejects_arbitrary_ref_or_repository(self):
        self.assertEqual(validation.validate_controller_environment(environment()), ("34050000001", "b" * 40))
        for changed in ({"GITHUB_REF": "refs/heads/feature"}, {"GITHUB_REPOSITORY": "fork/repo"}):
            with self.assertRaises(validation.ValidationError):
                validation.validate_controller_environment(environment(**changed))

    def test_protected_control_plane_target_paths_are_rejected(self):
        inputs = validation.parse_inputs(environment())
        for path in (".github/workflows/a6-pr-validation.yml", "scripts/a6_pr_validation.py",
                     "scripts/a5_reviewer.py", "scripts/codex_issue_worker.py"):
            self.assertTrue(validation.protected_path(path))
            with self.assertRaises(validation.ValidationError):
                validation.validate_metadata(pr(), issue(), (path,), inputs)

    def test_gate_is_rechecked_for_force_push_and_metadata_races(self):
        client = FakeClient()
        def current_pr(_):
            client.calls += 1
            if client.calls > 1:
                return pr(head="c" * 40)
            return pr()
        client.pr = current_pr
        with mock.patch.object(validation, "checkout_exact_target") as checkout:
            record = validation.execute(client, validation.parse_inputs(environment()), environment())
        self.assertEqual(record["failure_category"], "metadata-rejected")
        checkout.assert_not_called()

    def test_exact_target_workspace_is_separate_credential_free_and_head_bound(self):
        calls = []
        def runner(command, **kwargs):
            calls.append((command, kwargs))
            output = HEAD + "\n" if command[-1] == "HEAD" else ""
            return subprocess.CompletedProcess(command, 0, output, "")
        with tempfile.TemporaryDirectory() as workspace:
            validation.checkout_exact_target(workspace, HEAD, environment(GITHUB_TOKEN="secret", OPENAI_API_KEY="secret"), runner)
        self.assertIn(["git", "fetch", "--depth=1", "origin", HEAD], [call[0] for call in calls])
        for _, kwargs in calls:
            self.assertNotIn("GITHUB_TOKEN", kwargs["env"])
            self.assertNotIn("OPENAI_API_KEY", kwargs["env"])
            self.assertEqual(kwargs["env"]["GIT_TERMINAL_PROMPT"], "0")

    def test_stale_target_checkout_cannot_pass(self):
        def runner(command, **_):
            output = ("c" * 40 + "\n") if command[-1] == "HEAD" else ""
            return subprocess.CompletedProcess(command, 0, output, "")
        with tempfile.TemporaryDirectory() as workspace:
            with self.assertRaisesRegex(validation.ValidationError, "target-head-stale"):
                validation.checkout_exact_target(workspace, HEAD, environment(), runner)

    def test_target_code_profile_is_blocked_without_isolated_identity(self):
        profile = validation.ValidationProfile("target-test", 1, True)
        with tempfile.TemporaryDirectory() as workspace:
            with self.assertRaisesRegex(validation.ValidationError, "target-code-isolation"):
                validation.run_profile(profile, workspace, environment())

    def test_inert_profile_strips_credentials_and_uses_trusted_timeout(self):
        captured = {}
        def inert(environment):
            captured.update(environment)
            return preflight.PreflightResult("passed", "2021", "none")
        with tempfile.TemporaryDirectory() as workspace:
            with mock.patch.object(validation.preflight, "run_preflight", side_effect=inert):
                result = validation.run_profile(validation.PROFILES["inert-cae-runtime-probe"], workspace,
                                                environment(GITHUB_TOKEN="secret", CODEX_TOKEN="secret"))
        self.assertEqual(result, validation.ValidationResult("passed", "2021", "none"))
        self.assertNotIn("GITHUB_TOKEN", captured)
        self.assertNotIn("CODEX_TOKEN", captured)
        self.assertEqual(captured["A6_TIMEOUT_SECONDS"], "120")

    def test_evidence_is_exact_bounded_and_safe(self):
        inputs = validation.parse_inputs(environment())
        record = validation.evidence(validation.ValidationResult("passed", "2021", "none"), inputs, "risk:yellow", "34050000001", "b" * 40)
        rendered = json.dumps(record, sort_keys=True)
        self.assertLess(len(rendered), 800)
        self.assertEqual(record["target_head_sha"], HEAD)
        self.assertEqual(record["trusted_controller_sha"], "b" * 40)
        self.assertEqual(record["runner_labels"], list(validation.RUNNER_LABELS))
        for forbidden in ("license", "secret", "C:/", "Users"):
            self.assertNotIn(forbidden, rendered)

    def test_inert_profile_uses_controller_probe_not_target_or_production_helpers(self):
        source = inspect.getsource(validation.run_profile)
        self.assertIn("preflight.run_preflight", source)
        for helper in ("import_and_partition", "apply_materials", "apply_meshing", "apply_boundary", "create_input", "data_extract"):
            self.assertNotIn(helper, source)

    def test_pass_failed_unavailable_and_timeout_results_remain_bounded(self):
        inputs = validation.parse_inputs(environment())
        for result in (validation.ValidationResult("passed", "2021", "none"),
                       validation.ValidationResult("failed", "unavailable", "timeout"),
                       validation.ValidationResult("unavailable", "unavailable", "runtime-unavailable")):
            record = validation.evidence(result, inputs, "risk:yellow", "34050000001", "b" * 40)
            self.assertIn(record["outcome"], ("passed", "failed", "unavailable"))
        self.assertEqual(validation.PROFILES["inert-cae-runtime-probe"].timeout_seconds, 120)

    def test_a4_and_a5_already_protect_new_a62_executable(self):
        _, denied = green_worker.green_changed_paths(("scripts/a6_pr_validation.py",))
        self.assertEqual(denied, ("scripts/a6_pr_validation.py",))
        self.assertTrue(repair.is_protected_path("scripts/a6_pr_validation.py"))


if __name__ == "__main__":
    unittest.main()
