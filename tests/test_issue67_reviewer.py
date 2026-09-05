import json
import os
from pathlib import Path
import subprocess
import unittest
from unittest import mock

from scripts import a5_reviewer as reviewer


HEAD = "a" * 40


def snapshot(**overrides):
    value = {
        "schema_version": 1, "repository": "RayZhang2024/ML-AMstress", "pull_request_number": 67,
        "issue_number": 67, "base_sha": "b" * 40, "head_sha": HEAD, "pr_title": "A5.1 reviewer",
        "pr_body": "Adds a bounded reviewer.", "issue_title": "Reviewer foundation", "issue_body": "Contract.",
        "issue_labels": ["status:review", "risk:green"], "declared_risk": "green", "trusted_risk_floor": "green",
        "changed_files": [{"path": "scripts/a5_reviewer.py", "patch": "+reviewer"}],
        "ci_checks": [{"name": "Normal Python", "status": "success"}],
        "worker_metadata": {"worker_run_id": "123", "branch": "codex/issue-67-reviewer"},
    }
    value.update(overrides)
    return value


def verdict(**overrides):
    value = {
        "schema_version": 1, "verdict": "clean", "reviewed_head_sha": HEAD, "effective_risk": "green",
        "summary": "Bounded implementation matches the contract.", "findings": [], "escalation_reason": "",
    }
    value.update(overrides)
    return json.dumps(value, separators=(",", ":"))


class ReviewerContractTests(unittest.TestCase):
    def setUp(self):
        self.valid_snapshot = reviewer.validate_snapshot(snapshot())

    def test_valid_clean_blocker_and_escalate(self):
        self.assertEqual(reviewer.parse_verdict(verdict(), self.valid_snapshot).verdict, "clean")
        finding = {"id": "F-1", "category": "tests", "message": "Missing test.",
                   "required_action": "Add test.", "required_evidence": "Focused test passes."}
        self.assertEqual(reviewer.parse_verdict(verdict(verdict="blocker", findings=[finding]), self.valid_snapshot).verdict, "blocker")
        self.assertEqual(reviewer.parse_verdict(verdict(verdict="escalate", effective_risk="red", escalation_reason="Scientific ambiguity."), self.valid_snapshot).verdict, "escalate")

    def test_snapshot_rejects_unknown_schema_risk_and_unsafe_input(self):
        for changes in ({"schema_version": 2}, {"trusted_risk_floor": "blue"}, {"head_sha": "short"},
                        {"changed_files": [{"path": "../unsafe", "patch": "+x"}]}, {"extra": True}):
            with self.assertRaises(reviewer.ReviewError):
                reviewer.validate_snapshot(snapshot(**changes))

    def test_stale_and_malformed_outputs_fail_closed(self):
        outputs = ("not JSON", "```json\n{}\n```", verdict(reviewed_head_sha="c" * 40),
                   verdict(extra="no"), '{"schema_version":1,"schema_version":1}')
        for output in outputs:
            with self.assertRaises(reviewer.ReviewError):
                reviewer.parse_verdict(output, self.valid_snapshot)

    def test_verdict_consistency_and_findings_are_strict(self):
        finding = {"id": "F-1", "category": "tests", "message": "Missing test.",
                   "required_action": "Add test.", "required_evidence": "Focused test passes."}
        bad = (
            verdict(verdict="clean", findings=[finding]), verdict(verdict="blocker"),
            verdict(effective_risk="red"), verdict(effective_risk="blue"), verdict(verdict="unknown"),
            verdict(schema_version=2),
            verdict(verdict="blocker", findings=[finding, finding]),
            verdict(verdict="blocker", findings=[dict(finding, id="not-stable")]),
        )
        for output in bad:
            with self.assertRaises(reviewer.ReviewError):
                reviewer.parse_verdict(output, self.valid_snapshot)
        yellow_floor = reviewer.validate_snapshot(snapshot(trusted_risk_floor="yellow"))
        with self.assertRaises(reviewer.ReviewError):
            reviewer.parse_verdict(verdict(effective_risk="green"), yellow_floor)

    def test_read_only_command_and_credential_free_stdin_invocation(self):
        raw_secret = "ghp_abcdefghijklmnopqrstuvwxyz"
        prompt = reviewer.build_prompt(self.valid_snapshot)
        command = reviewer.reviewer_command("C:/tools/codex.exe", "C:/temporary/final.json")
        self.assertEqual(command, [
            "C:/tools/codex.exe", "exec", "--model", "gpt-5.5", "--sandbox", "read-only", "-c", 'approval_policy="never"',
            "--output-last-message", "C:/temporary/final.json", "-",
        ])
        self.assertNotIn("--approve-for-me", command)
        self.assertNotIn("--dangerously-bypass-approvals-and-sandbox", command)
        self.assertNotIn("workspace-write", command)
        self.assertNotIn(raw_secret, command)
        self.assertNotIn(raw_secret, prompt)
        environment = reviewer.reviewer_environment({"GITHUB_TOKEN": raw_secret, "GH_TOKEN": raw_secret,
                                                     "OPENAI_API_KEY": raw_secret, "AUTOMATION_APP_TOKEN": raw_secret,
                                                     "SAFE": "yes"})
        self.assertEqual(environment["SAFE"], "yes")
        for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
            self.assertNotIn(name, environment)

        captured = {}
        def fake_run(command, **kwargs):
            captured.update(command=command, **kwargs)
            Path(command[command.index("--output-last-message") + 1]).write_text(verdict(), encoding="utf-8")
            return subprocess.CompletedProcess(command, 0, "session progress is not a verdict", "")
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=fake_run):
            result = reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(result.verdict, "clean")
        self.assertEqual(captured["input"], prompt)
        self.assertNotIn(prompt, " ".join(captured["command"]))
        self.assertNotIn(raw_secret, " ".join(captured["command"]))
        self.assertFalse(captured.get("shell", False))
        for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
            self.assertNotIn(name, captured["env"])
        final_path = Path(captured["command"][captured["command"].index("--output-last-message") + 1])
        self.assertFalse(final_path.exists())

    def test_nonzero_reviewer_exit_reports_bounded_stderr_diagnostic(self):
        completed = subprocess.CompletedProcess(
            ["codex"], 17, "stdout detail that must not win", "stderr safe diagnostic"
        )
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", return_value=completed):
            with self.assertRaises(reviewer.ReviewError) as raised:
                reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(str(raised.exception), "reviewer-process exit 17: stderr safe diagnostic")
        self.assertNotIn("stdout detail", str(raised.exception))

    def test_nonzero_reviewer_exit_uses_stdout_only_when_stderr_is_empty(self):
        completed = subprocess.CompletedProcess(["codex"], 9, "safe stdout fallback", "")
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", return_value=completed):
            with self.assertRaises(reviewer.ReviewError) as raised:
                reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(str(raised.exception), "reviewer-process exit 9: safe stdout fallback")

    def test_reviewer_failure_diagnostic_redacts_sensitive_content_and_suppresses_prompt(self):
        github = "ghp_abcdefghijklmnop"
        app = "app-token-secret-value"
        jwt = "eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue"
        text = (
            "safe failure line\n"
            "Authorization: Bearer bearer-secret-value\n"
            'Basic "basic-secret-value"\n'
            '"id_token": "id-token-secret-value"\n'
            "refresh_token=refresh-token-secret-value\n"
            "AUTOMATION_APP_TOKEN=%s\n" % app
        ) + (
            "github_pat_abcdefghijklmnop %s C:/Users/alice/private.txt /home/alice/private.txt %s\n"
            "You are a read-only PR reviewer.\nSNAPSHOT_JSON:\nsecret issue body and diff"
        ) % (github, jwt)
        with mock.patch.dict(os.environ, {
            "GITHUB_TOKEN": github,
            "AUTOMATION_APP_TOKEN": app,
            "OPENAI_API_KEY": "sk-abcdefghijklmnop",
        }, clear=False):
            diagnostic = reviewer.reviewer_process_failure_diagnostic(23, "", text)
        self.assertIn("reviewer-process exit 23", diagnostic)
        self.assertIn("safe failure line", diagnostic)
        for forbidden in (
            github, app, jwt, "bearer-secret-value", "basic-secret-value", "id-token-secret-value",
            "refresh-token-secret-value", "C:/Users", "/home/alice", "You are a read-only PR reviewer.",
            "secret issue body", "diff",
        ):
            self.assertNotIn(forbidden, diagnostic)

    def test_reviewer_failure_diagnostic_is_deterministically_bounded(self):
        diagnostic = reviewer.reviewer_process_failure_diagnostic(
            1, "", "\n".join("line-%d %s" % (index, "x" * 300) for index in range(8))
        )
        self.assertLessEqual(len(diagnostic), reviewer.MAX_REVIEWER_FAILURE_DIAGNOSTIC_CHARS)
        self.assertNotIn("line-0", diagnostic)
        self.assertIn("line-7", diagnostic)
        self.assertLessEqual(diagnostic.count(" | "), reviewer.MAX_REVIEWER_FAILURE_DIAGNOSTIC_LINES - 1)

    def test_reviewer_failure_diagnostic_falls_back_to_category_and_exit_code(self):
        self.assertEqual(
            reviewer.reviewer_process_failure_diagnostic(2, "", "Bearer bearer-secret-value"),
            "reviewer-process exit 2",
        )

    def test_reviewer_start_failure_does_not_chain_local_process_detail(self):
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=OSError("C:/Users/alice/codex.exe")):
            with self.assertRaises(reviewer.ReviewError) as raised:
                reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(str(raised.exception), "reviewer process could not start")
        self.assertNotIn("C:/Users", str(raised.exception))

    def test_credential_material_is_rejected_before_prompt_and_large_prompt_is_intact(self):
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "synthetic-secret-value"}, clear=False):
            with self.assertRaises(reviewer.ReviewError):
                reviewer.validate_snapshot(snapshot(pr_body="synthetic-secret-value"))
        large_patch = "x" * 100_000
        large = reviewer.validate_snapshot(snapshot(changed_files=[{"path": "tests/large.py", "patch": large_patch}]))
        self.assertIn(large_patch, reviewer.build_prompt(large))
        captured = {}
        def fake_run(command, **kwargs):
            captured.update(command=command, **kwargs)
            Path(command[command.index("--output-last-message") + 1]).write_text(verdict(), encoding="utf-8")
            return subprocess.CompletedProcess(command, 0, "", "")
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=fake_run):
            reviewer.review_snapshot(snapshot(changed_files=[{"path": "tests/large.py", "patch": large_patch}]), "C:/repo")
        self.assertIn(large_patch, captured["input"])

    def test_success_reads_only_bounded_final_message_and_preserves_strict_parsing(self):
        captured = {}

        def fake_run(command, **kwargs):
            captured.update(command=command, **kwargs)
            Path(command[command.index("--output-last-message") + 1]).write_text(
                verdict(reviewed_head_sha="c" * 40), encoding="utf-8"
            )
            return subprocess.CompletedProcess(command, 0, "session id: not-a-verdict", "progress")

        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=fake_run):
            with self.assertRaises(reviewer.ReviewError) as raised:
                reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(str(raised.exception), "verdict reviewed_head_sha does not match snapshot")
        self.assertNotIn("session id", str(raised.exception))

    def test_missing_or_oversized_final_message_fails_closed(self):
        def missing_output(command, **kwargs):
            return subprocess.CompletedProcess(command, 0, "", "")

        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=missing_output):
            with self.assertRaisesRegex(reviewer.ReviewError, "reviewer final result is unavailable"):
                reviewer.review_snapshot(snapshot(), "C:/repo")

        def oversized_output(command, **kwargs):
            Path(command[command.index("--output-last-message") + 1]).write_bytes(
                b"x" * (reviewer.MAX_REVIEWER_FINAL_OUTPUT_BYTES + 1)
            )
            return subprocess.CompletedProcess(command, 0, "", "")

        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=oversized_output):
            with self.assertRaisesRegex(reviewer.ReviewError, "reviewer final result is unavailable"):
                reviewer.review_snapshot(snapshot(), "C:/repo")
