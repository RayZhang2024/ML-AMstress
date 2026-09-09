import copy
import unittest
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer


OLD_HEAD = "a" * 40
REPAIR_HEAD = "b" * 40
THIRD_HEAD = "c" * 40
BRANCH = "codex/issue-210-a5-repair-push-verification"


def pr(head=OLD_HEAD, branch=BRANCH):
    return {
        "number": 210,
        "state": "open",
        "base": {"ref": "main", "repo": {"full_name": orchestrator.REPOSITORY}},
        "head": {"sha": head, "ref": branch, "repo": {"full_name": orchestrator.REPOSITORY}},
    }


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
