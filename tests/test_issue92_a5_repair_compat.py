import json
import os
import unittest
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a5_review_orchestrator as orchestrator


HEAD = "a" * 40


def request():
    return repair.RepairRequest(
        schema_version=1,
        repository="RayZhang2024/ML-AMstress",
        pull_request_number=83,
        issue_number=82,
        branch="codex/issue-82-a5-4b-fresh-fixture-live-docs-only-review-repair-t",
        expected_head_sha=HEAD,
        review_decision_key="a5.2:" + ("b" * 64),
        current_issue_status="status:in-progress",
        current_pr_review_state="review:blocker",
        review_state_head_sha=HEAD,
        effective_risk="green",
        accepted_findings=(
            repair.BlockerFinding(
                "F-1",
                "tests",
                "fixture value violates the issue contract",
                "restore the required fixture value",
                "trusted validation passes",
            ),
        ),
        allowed_paths=("docs/A5_4B_LIVE_FIXTURE.md",),
        attempt_number=1,
    )


class RepairCodexCompatibilityTests(unittest.TestCase):
    def test_repair_codex_command_pins_runner_compatible_model_and_stdin(self):
        completed = mock.Mock(returncode=0, stdout="progress must not be trusted", stderr="")
        with mock.patch.dict(
            os.environ,
            {
                "GITHUB_TOKEN": "github-secret",
                "GH_TOKEN": "gh-secret",
                "OPENAI_API_KEY": "openai-secret",
                "AUTOMATION_APP_TOKEN": "app-secret",
            },
            clear=False,
        ), mock.patch.object(repair, "resolve_codex_executable", return_value="codex.exe"), mock.patch.object(
            repair, "_run", return_value=completed
        ) as run:
            repair.run_codex(request(), ".")

        command, cwd, environment, prompt = run.call_args.args
        self.assertEqual(
            command,
            [
                "codex.exe",
                "exec",
                "--model",
                "gpt-5.5",
                "--sandbox",
                "workspace-write",
                "-c",
                'approval_policy="never"',
                "-",
            ],
        )
        self.assertEqual(cwd, ".")
        self.assertEqual(repair.CODEX_REPAIR_MODEL, "gpt-5.5")
        self.assertNotIn(prompt, command)
        self.assertTrue(prompt.startswith("Trusted repair request (A5.3)."))
        for secret_name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
            self.assertNotIn(secret_name, environment)
        self.assertEqual(environment["GIT_TERMINAL_PROMPT"], "0")
        self.assertEqual(environment["GIT_CONFIG_GLOBAL"], os.devnull)

    def test_repair_codex_nonzero_remains_bounded_and_stream_free(self):
        completed = mock.Mock(returncode=1, stdout="secret stdout", stderr="secret stderr")
        with mock.patch.object(repair, "resolve_codex_executable", return_value="codex.exe"), mock.patch.object(
            repair, "_run", return_value=completed
        ):
            with self.assertRaisesRegex(repair.RepairError, "^Codex execution failed$") as caught:
                repair.run_codex(request(), ".")
        self.assertNotIn("stdout", str(caught.exception))
        self.assertNotIn("stderr", str(caught.exception))


class RepairFailureAuditTests(unittest.TestCase):
    @staticmethod
    def payload(marker):
        prefix = "<!-- a5.4a-repair-failed:"
        suffix = " -->"
        assert marker.startswith(prefix) and marker.endswith(suffix)
        return json.loads(marker[len(prefix):-len(suffix)])

    def test_reviewed_repair_error_gets_bounded_static_detail(self):
        marker = orchestrator._repair_failure_marker(1, repair.RepairError("Codex execution failed"))
        self.assertLess(len(marker), orchestrator.MAX_AUDIT)
        self.assertEqual(
            self.payload(marker),
            {
                "attempt": 1,
                "category": "trusted-repair-failed",
                "detail": "Codex execution failed",
                "schema_version": 1,
            },
        )

    def test_unreviewed_or_unexpected_error_never_reaches_audit_detail(self):
        secrets = (
            repair.RepairError(r"token=C:/Users/private/sentinel"),
            RuntimeError(r"token=C:/Users/private/sentinel"),
        )
        for error in secrets:
            marker = orchestrator._repair_failure_marker(2, error)
            payload = self.payload(marker)
            self.assertEqual(payload["category"], "trusted-repair-failed")
            self.assertNotIn("detail", payload)
            self.assertNotIn("sentinel", marker)
            self.assertNotIn("C:/Users", marker)

    def test_safe_detail_allowlist_is_exact_not_pattern_based(self):
        self.assertEqual(
            repair.audit_safe_error_detail(repair.RepairError("local validation failed")),
            "local validation failed",
        )
        self.assertIsNone(repair.audit_safe_error_detail(repair.RepairError("local validation failed: secret")))
        self.assertIsNone(repair.audit_safe_error_detail(ValueError("Codex execution failed")))


if __name__ == "__main__":
    unittest.main()
