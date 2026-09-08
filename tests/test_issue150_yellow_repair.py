import dataclasses
import inspect
import os
import subprocess
import unittest
from unittest import mock

from scripts import a5_repair_worker as green
from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_yellow_repair_worker as yellow
from tests.test_issue75_review_orchestrator import (
    FakeClient, automated_evidence, automated_yellow_issue, automated_yellow_pull_request,
    protected_blocker_verdict,
)


HEAD = "a" * 40
BRANCH = "codex-yellow/issue-150-yellow-repair"


def request(**changes):
    finding = green.BlockerFinding("F-1", "tests", "bounded defect", "repair it", "[AC-1] tests pass")
    value = yellow.YellowRepairRequest(
        1, "RayZhang2024/ML-AMstress", 153, 150, BRANCH, HEAD,
        "a5.2:" + "b" * 64, "a5.yellow-blocker:" + "c" * 64,
        "a5.yellow-authorization:" + "d" * 64, "status:in-progress",
        "review:blocker", HEAD, "yellow", (finding,), ("docs/change.md",), 1,
    )
    return dataclasses.replace(value, **changes)


class YellowRequestTests(unittest.TestCase):
    def test_valid_request_and_decision_identity_are_deterministic(self):
        item = request()
        self.assertIs(yellow.validate_request(item), item)
        self.assertEqual(yellow.repair_decision_key(item), yellow.repair_decision_key(item))
        self.assertRegex(yellow.repair_decision_key(item), yellow.DECISION_KEY_RE)
        changed = dataclasses.replace(item, attempt_number=2)
        self.assertNotEqual(yellow.repair_decision_key(item), yellow.repair_decision_key(changed))

    def test_request_binds_all_trusted_identities(self):
        cases = {
            "repository": {"repository": "other/repo"},
            "pr": {"pull_request_number": 0},
            "issue": {"issue_number": 151},
            "branch": {"branch": "codex-yellow/issue-151-wrong"},
            "head": {"expected_head_sha": "x" * 40},
            "decision": {"blocker_decision_key": "bad"},
            "blocker evidence": {"blocker_evidence_key": "a5.yellow-blocker:bad"},
            "authorization": {"authorization_key": "a5.yellow-authorization:bad"},
            "status": {"current_issue_status": "status:review"},
            "review": {"current_pr_review_state": "review:pending"},
            "review head": {"review_state_head_sha": "e" * 40},
            "risk": {"effective_risk": "green"},
            "attempt": {"attempt_number": 3},
        }
        for name, change in cases.items():
            with self.subTest(name=name), self.assertRaises((yellow.YellowRepairError, green.RepairError)):
                yellow.validate_request(request(**change))

    def test_paths_are_exact_bounded_and_never_scientific(self):
        rejected = (
            (), ("docs/change.md", "docs/change.md"), ("../escape",),
            ("tests/test_change.py", "docs/change.md"),
            tuple("docs/%d.md" % index for index in range(33)),
            ("import_and_partition.py",), ("AM_gui_v7.py",),
        )
        for paths in rejected:
            with self.subTest(paths=paths), self.assertRaises((yellow.YellowRepairError, green.RepairError)):
                yellow.validate_request(request(authorized_paths=paths))
        yellow.validate_request(request(authorized_paths=("docs/change.md", "tests/test_change.py")))

    def test_finding_categories_ids_and_bounds_fail_closed(self):
        for findings in (
            (),
            (green.BlockerFinding("bad", "tests", "m", "a", "[AC-1] e"),),
            (green.BlockerFinding("F-1", "scientific", "m", "a", "[AC-1] e"),),
            (green.BlockerFinding("F-1", "tests", "m", "a", "[AC-1] e"),) * 2,
        ):
            with self.subTest(findings=findings), self.assertRaises((yellow.YellowRepairError, green.RepairError)):
                yellow.validate_request(request(accepted_findings=findings))

    def test_evidence_keys_are_canonical_and_kind_bound(self):
        first = yellow.evidence_key("authorization", '{"a":1}')
        self.assertEqual(first, yellow.evidence_key("authorization", '{"a":1}'))
        self.assertNotEqual(first, yellow.evidence_key("blocker", '{"a":1}'))
        with self.assertRaises(yellow.YellowRepairError):
            yellow.evidence_key("unknown", "value")


class YellowExecutionTests(unittest.TestCase):
    def test_codex_uses_workspace_write_stdin_and_stripped_credentials(self):
        completed = subprocess.CompletedProcess([], 0, "", "")
        with mock.patch.object(green, "resolve_codex_executable", return_value="codex.exe"), \
                mock.patch.object(green, "_run", return_value=completed) as run, \
                mock.patch.dict(os.environ, {
                    "GITHUB_TOKEN": "github", "GH_TOKEN": "gh", "OPENAI_API_KEY": "openai",
                    "AUTOMATION_APP_TOKEN": "app",
                }, clear=False):
            yellow.run_codex(request(), ".")
        command, _, environment, prompt = run.call_args.args
        self.assertEqual(command[-1], "-")
        self.assertIn("workspace-write", command)
        self.assertIn('approval_policy="never"', command)
        self.assertNotIn(prompt, command)
        for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
            self.assertNotIn(name, environment)
        self.assertNotIn("dangerously", " ".join(command))

    def test_exact_scope_rejects_added_path_and_scientific_path(self):
        yellow.enforce_change_scope(request(), ("docs/change.md",))
        for paths in (("docs/extra.md",), ("import_and_partition.py",)):
            with self.subTest(paths=paths), self.assertRaises(yellow.YellowRepairError):
                yellow.enforce_change_scope(request(), paths)

    def test_push_uses_ephemeral_app_token_and_exact_lease(self):
        completed = subprocess.CompletedProcess([], 0, "", "")
        captured = {}
        def run(command, cwd, environment=None, input_text=None):
            captured.update(command=command, environment=dict(environment or {}))
            return completed
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "secret-app-token"}, clear=False), \
                mock.patch.object(green, "_run", side_effect=run):
            yellow.push_repair(request(), ".", "e" * 40)
        self.assertIn("--force-with-lease=refs/heads/%s:%s" % (BRANCH, HEAD), captured["command"])
        self.assertNotIn("secret-app-token", " ".join(captured["command"]))
        self.assertNotIn("GITHUB_TOKEN", captured["environment"])
        self.assertNotIn("AUTOMATION_APP_TOKEN", captured["environment"])

    def test_source_has_no_github_merge_or_shell_authority(self):
        source = inspect.getsource(yellow).lower()
        self.assertNotIn("shell=true", source)
        self.assertNotIn("auto-merge", source)
        self.assertNotIn("/merges", source)


class SharedAttemptHistoryTests(unittest.TestCase):
    def test_attempts_one_and_two_use_shared_history_and_third_is_exhausted(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        verdict = protected_blocker_verdict()
        args = (
            client, client.pr_data, client.issue_data, client.comment_data, current, verdict,
            "a5.2:" + "b" * 64, "a5.yellow-authorization:" + "d" * 64,
            "a5.yellow-blocker:" + "c" * 64, ("docs/change.md",), ".",
        )
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(yellow, "execute_repair", side_effect=yellow.YellowRepairError("failed")) as execute:
            self.assertEqual(orchestrator._yellow_repair(*args), "repair-failed")
            self.assertEqual(orchestrator._yellow_repair(*args), "repair-failed")
            self.assertEqual(orchestrator._yellow_repair(*args), "repair-exhausted")
        self.assertEqual(execute.call_count, 2)
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 2)
        self.assertEqual(orchestrator.MAX_REPAIR_ATTEMPTS, 2)
        attempt_markers = [item["body"] for item in client.comment_data if "a5.4a-repair:" in item["body"]]
        self.assertTrue(all('"lane":"automated-yellow"' in marker for marker in attempt_markers))
        self.assertTrue(all('"authorization_key":"a5.yellow-authorization:' in marker
                            and '"blocker_evidence_key":"a5.yellow-blocker:' in marker
                            for marker in attempt_markers))

    def test_invalid_request_consumes_no_attempt(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        with self.assertRaises((yellow.YellowRepairError, green.RepairError)):
            orchestrator._yellow_repair(
                client, client.pr_data, client.issue_data, client.comment_data, current,
                protected_blocker_verdict(), "a5.2:" + "b" * 64,
                "a5.yellow-authorization:" + "d" * 64, "a5.yellow-blocker:" + "c" * 64,
                ("import_and_partition.py",), ".",
            )
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)


if __name__ == "__main__":
    unittest.main()
