import json
import os
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
        command = reviewer.reviewer_command("C:/tools/codex.exe")
        self.assertEqual(command, ["C:/tools/codex.exe", "exec", "--sandbox", "read-only", "-c", 'approval_policy="never"', "-"])
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
            return subprocess.CompletedProcess(command, 0, verdict(), "")
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=fake_run):
            result = reviewer.review_snapshot(snapshot(), "C:/repo")
        self.assertEqual(result.verdict, "clean")
        self.assertEqual(captured["input"], prompt)
        self.assertNotIn(prompt, " ".join(captured["command"]))
        self.assertNotIn(raw_secret, " ".join(captured["command"]))

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
            return subprocess.CompletedProcess(command, 0, verdict(), "")
        with mock.patch.object(reviewer, "resolve_codex_executable", return_value="C:/tools/codex.exe"), \
             mock.patch.object(reviewer.subprocess, "run", side_effect=fake_run):
            reviewer.review_snapshot(snapshot(changed_files=[{"path": "tests/large.py", "patch": large_patch}]), "C:/repo")
        self.assertIn(large_patch, captured["input"])
