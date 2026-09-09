import copy
import unittest
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer
from scripts import yellow_lane_policy as yellow_policy


OLD_HEAD = "a" * 40
REPAIR_HEAD = "b" * 40
THIRD_HEAD = "c" * 40
BRANCH = "codex/issue-210-a5-repair-push-verification"


def pr(head=OLD_HEAD, branch=BRANCH):
    return {
        "number": 210,
        "state": "open",
        "title": "Issue #210",
        "body": "Refs #210",
        "base": {"ref": "main", "sha": "d" * 40, "repo": {"full_name": orchestrator.REPOSITORY}},
        "head": {"sha": head, "ref": branch, "repo": {"full_name": orchestrator.REPOSITORY}},
    }


def issue(risk="green"):
    return {
        "number": 210,
        "state": "open",
        "title": "A5 repair push verification",
        "body": """## Goal
Test.
## Necessity Gate
Test.
## Required behavior
Test.
## Do not change
Test.
## Acceptance criteria
- [ ] Existing GREEN A5 repair regressions pass.
- [ ] Existing automatic-YELLOW A5 repair regressions pass.
- [ ] Full normal-Python suite passes.
- [ ] Python syntax compilation passes.
- [ ] git diff --check origin/main...HEAD passes.
- [ ] trusted-current-main A5 evidence is present.
## Tests/validation
Test.
## Risk classification
Declared risk label: `risk:%s`
## Dependencies
- none
""" % risk,
        "labels": [{"name": "status:review"}, {"name": "risk:" + risk}, {"name": "agent:codex"}],
    }


def changed_files(path="tests/test_issue210_repair_push_verification.py"):
    return [{"filename": path, "patch": "+# exact-head evidence regression"}]


def snapshot_check_names(lane="green"):
    branch = BRANCH if lane == "green" else yellow_policy.yellow_branch(210, "A5 repair push verification")
    pull_request = pr(branch=branch)
    if lane != "green":
        pull_request["base"]["sha"] = "e" * 40
    snapshot, _ = orchestrator.build_snapshot(
        pull_request,
        issue("green" if lane == "green" else "yellow"),
        orchestrator.WorkflowRun(100, OLD_HEAD, "success"),
        changed_files(),
        lane,
        ("tests/test_issue210_repair_push_verification.py",),
    )
    return [item["name"] for item in snapshot["ci_checks"]], snapshot


class SequencedClient:
    def __init__(self, reads):
        self.reads = list(reads)
        self.read_count = 0
        self.comments_data = []
        self.issue_data = {"number": 210, "labels": [{"name": "status:in-progress"}]}

    def pr(self, number):
        self.read_count += 1
        value = self.reads.pop(0)
        if isinstance(value, BaseException):
            raise value
        return copy.deepcopy(value)

    def issue(self, number):
        return self.issue_data

    def comments(self, number):
        return self.comments_data

    def comment(self, number, body):
        self.comments_data.append({"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})


def repair_result():
    return repair.RepairResult(
        1, orchestrator.REPOSITORY, 210, 210, BRANCH, 1, OLD_HEAD, REPAIR_HEAD,
        ("F-1",), ("docs/change.md",), "passed", "a5.3:" + "d" * 64,
    )


def blocker_verdict():
    return reviewer.ReviewVerdict(
        1, "blocker", OLD_HEAD, "green", "blocker",
        (reviewer.Finding("F-1", "tests", "blocker", "repair", "evidence"),), "",
    )


class RepairPushVerificationTests(unittest.TestCase):
    def test_green_snapshot_exposes_each_required_exact_head_validation_gate(self):
        names, snapshot = snapshot_check_names("green")
        self.assertEqual(names, list(orchestrator.EXACT_HEAD_VALIDATION_CHECKS))
        self.assertEqual({item["status"] for item in snapshot["ci_checks"]}, {"success"})
        self.assertIn("Existing GREEN A5 repair regressions", names)
        self.assertIn("Existing automatic-YELLOW A5 repair regressions", names)
        self.assertIn("Full normal-Python suite", names)
        self.assertIn("Python syntax compilation", names)
        self.assertIn("git diff --check origin/main...HEAD", names)
        self.assertIn("trusted-current-main A5", names)
        self.assertEqual(snapshot["head_sha"], OLD_HEAD)

    def test_automated_yellow_snapshot_exposes_same_exact_head_validation_gates(self):
        names, snapshot = snapshot_check_names(yellow_policy.AUTOMATED_YELLOW_LANE)
        self.assertEqual(names, list(orchestrator.EXACT_HEAD_VALIDATION_CHECKS))
        self.assertEqual({item["status"] for item in snapshot["ci_checks"]}, {"success"})
        self.assertEqual(snapshot["trusted_risk_floor"], "yellow")
        self.assertEqual(snapshot["head_sha"], OLD_HEAD)

    def test_old_head_then_returned_head_completes_without_another_repair_attempt(self):
        client = SequencedClient([pr(OLD_HEAD), pr(REPAIR_HEAD)])
        transition = mock.Mock()
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", OLD_HEAD)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(orchestrator.repair, "execute_repair", return_value=repair_result()) as execute, \
                mock.patch.object(orchestrator, "current_review_state", return_value=current), \
                mock.patch.object(orchestrator, "apply_transition", transition):
            self.assertEqual(orchestrator._repair(
                client, pr(), client.issue_data, [], current, blocker_verdict(),
                "a5.2:" + "e" * 64, ("docs/change.md",), ".",
            ), "repair-pushed")
        execute.assert_called_once()
        transition.assert_called_once()
        self.assertEqual(client.read_count, 2)
        self.assertEqual(orchestrator.repair_attempt_count(client.comments_data, 210), 1)

    def test_persistent_old_head_fails_after_fixed_read_budget_without_transition(self):
        client = SequencedClient([pr(OLD_HEAD), pr(OLD_HEAD)])
        transition = mock.Mock()
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", OLD_HEAD)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(orchestrator.repair, "execute_repair", return_value=repair_result()) as execute, \
                mock.patch.object(orchestrator, "apply_transition", transition), \
                self.assertRaisesRegex(orchestrator.OrchestrationError, "repair push head could not be verified"):
            orchestrator._repair(client, pr(), client.issue_data, [], current, blocker_verdict(),
                                 "a5.2:" + "e" * 64, ("docs/change.md",), ".")
        execute.assert_called_once()
        transition.assert_not_called()
        self.assertEqual(client.read_count, orchestrator.MAX_REPAIR_PUSH_HEAD_VERIFICATION_READS)
        self.assertEqual(orchestrator.repair_attempt_count(client.comments_data, 210), 1)

    def test_unexpected_third_head_fails_immediately(self):
        client = SequencedClient([pr(THIRD_HEAD), pr(REPAIR_HEAD)])
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair push head could not be verified"):
            orchestrator._verify_repair_push_head(client, 210, BRANCH, OLD_HEAD, REPAIR_HEAD)
        self.assertEqual(client.read_count, 1)

    def test_missing_or_ambiguous_branch_identity_fails_closed(self):
        missing_head = pr(REPAIR_HEAD)
        missing_head["head"] = None
        client = SequencedClient([missing_head])
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "PR head"):
            orchestrator._verify_repair_push_head(client, 210, BRANCH, OLD_HEAD, REPAIR_HEAD)
        self.assertEqual(client.read_count, 1)

        conflicting_pr = pr(REPAIR_HEAD)
        conflicting_pr["number"] = 211
        client = SequencedClient([conflicting_pr])
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair push head could not be verified"):
            orchestrator._verify_repair_push_head(client, 210, BRANCH, OLD_HEAD, REPAIR_HEAD)
        self.assertEqual(client.read_count, 1)

    def test_transient_read_error_is_retried_but_non_transient_error_is_not(self):
        transient = orchestrator.OrchestrationError("GitHub get-pr: transport timeout")
        client = SequencedClient([transient, pr(REPAIR_HEAD)])
        self.assertEqual(orchestrator._verify_repair_push_head(client, 210, BRANCH, OLD_HEAD, REPAIR_HEAD)["head"]["sha"], REPAIR_HEAD)
        self.assertEqual(client.read_count, 2)

        client = SequencedClient([orchestrator.OrchestrationError("GitHub get-pr: HTTP 403"), pr(REPAIR_HEAD)])
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair push head could not be verified"):
            orchestrator._verify_repair_push_head(client, 210, BRANCH, OLD_HEAD, REPAIR_HEAD)
        self.assertEqual(client.read_count, 1)


if __name__ == "__main__":
    unittest.main()
