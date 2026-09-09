import copy
import unittest
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer
from scripts import a5_yellow_repair_worker as yellow_repair
from scripts import yellow_lane_policy


OLD_HEAD = "a" * 40
NEW_HEAD = "b" * 40
THIRD_HEAD = "c" * 40
GREEN_BRANCH = "codex/issue-214-repair-push-verification"
YELLOW_BRANCH = yellow_lane_policy.yellow_branch(214, "Repair push verification")


def pull_request(head=OLD_HEAD, branch=GREEN_BRANCH, number=214):
    return {"number": number, "head": {"sha": head, "ref": branch,
            "repo": {"full_name": orchestrator.REPOSITORY}}}


class SequencedClient:
    def __init__(self, observations, branch=GREEN_BRANCH):
        self.observations = list(observations)
        self.branch = branch
        self.pr_calls = 0
        self.comments_written = []
        self.issue_data = {"number": 314, "labels": [{"name": "status:in-progress"}]}

    def pr(self, number):
        self.pr_calls += 1
        observation = self.observations.pop(0)
        if isinstance(observation, Exception):
            raise observation
        return copy.deepcopy(observation)

    def issue(self, number):
        return self.issue_data

    def comments(self, number):
        return self.comments_written

    def comment(self, number, body):
        self.comments_written.append({"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})


class RepairPushHeadVerificationTests(unittest.TestCase):
    def verify(self, observations, branch=GREEN_BRANCH):
        client = SequencedClient(observations, branch)
        delays = []
        result = orchestrator._verify_repair_push_head(
            client, 214, branch, OLD_HEAD, NEW_HEAD, delays.append,
        )
        return client, delays, result

    def test_old_head_then_returned_head_succeeds_after_one_fixed_delay(self):
        client, delays, result = self.verify((pull_request(), pull_request(NEW_HEAD)))
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.pr_calls, 2)
        self.assertEqual(delays, [orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS])
        self.assertEqual(client.comments_written, [])

    def test_persistent_old_head_fails_closed_after_bounded_reads(self):
        client = SequencedClient((pull_request(), pull_request(), pull_request()))
        delays = []
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "could not be verified"):
            orchestrator._verify_repair_push_head(client, 214, GREEN_BRANCH, OLD_HEAD, NEW_HEAD, delays.append)
        self.assertEqual(client.pr_calls, orchestrator.MAX_REPAIR_PUSH_HEAD_VERIFICATION_READS)
        self.assertEqual(delays, [1, 1])
        self.assertEqual(client.comments_written, [])

    def test_unexpected_third_head_fails_without_waiting(self):
        client = SequencedClient((pull_request(THIRD_HEAD),))
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "unexpected concurrent head"):
            orchestrator._verify_repair_push_head(client, 214, GREEN_BRANCH, OLD_HEAD, NEW_HEAD, mock.Mock())
        self.assertEqual(client.pr_calls, 1)

    def test_mismatched_identity_fails_closed(self):
        wrong = pull_request(NEW_HEAD)
        wrong["head"]["ref"] = "codex/issue-214-other"
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "branch identity"):
            self.verify((wrong,))

    def test_explicit_transient_read_failure_retries_then_succeeds(self):
        transient = orchestrator.OrchestrationError("GitHub get-pr: transport timeout")
        client, delays, result = self.verify((transient, pull_request(NEW_HEAD)))
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.pr_calls, 2)
        self.assertEqual(delays, [1])

    def test_non_transient_read_failure_fails_closed_without_retry(self):
        client = SequencedClient((orchestrator.OrchestrationError("GitHub get-pr: HTTP 403"),))
        delay = mock.Mock()
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "HTTP 403"):
            orchestrator._verify_repair_push_head(client, 214, GREEN_BRANCH, OLD_HEAD, NEW_HEAD, delay)
        self.assertEqual(client.pr_calls, 1)
        delay.assert_not_called()

    def test_green_and_yellow_repair_use_shared_verifier_before_transition(self):
        verdict = reviewer.ReviewVerdict(1, "blocker", OLD_HEAD, "green", "blocked", (
            reviewer.Finding("F-1", "scope", "repair", "repair", "[AC-1] repair"),), "")
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", OLD_HEAD)
        green_client = SequencedClient((), GREEN_BRANCH)
        green_pr = pull_request()
        green_issue = {"number": 214}
        green_result = repair.RepairResult(1, orchestrator.REPOSITORY, 214, 214, GREEN_BRANCH, 1,
                                           OLD_HEAD, NEW_HEAD, ("F-1",), ("docs/change.md",), "passed", "a5.3:" + "d" * 64)
        yellow_client = SequencedClient((), YELLOW_BRANCH)
        yellow_pr = pull_request(branch=YELLOW_BRANCH)
        yellow_result = yellow_repair.YellowRepairResult(
            1, orchestrator.REPOSITORY, 214, 214, YELLOW_BRANCH, 1, OLD_HEAD, NEW_HEAD,
            ("F-1",), ("docs/change.md",), "passed", "a5.yellow-repair:" + "d" * 64,
        )
        verifier = mock.Mock(side_effect=[pull_request(NEW_HEAD), pull_request(NEW_HEAD, YELLOW_BRANCH)])
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(repair, "execute_repair", return_value=green_result), \
                mock.patch.object(yellow_repair, "execute_repair", return_value=yellow_result), \
                mock.patch.object(orchestrator, "_verify_repair_push_head", verifier), \
                mock.patch.object(orchestrator, "current_review_state", return_value=current), \
                mock.patch.object(orchestrator, "apply_transition") as transition:
            self.assertEqual(orchestrator._repair(
                green_client, green_pr, green_issue, (), current, verdict, "a5.2:" + "d" * 64,
                ("docs/change.md",), "."), "repair-pushed")
            self.assertEqual(orchestrator._yellow_repair(
                yellow_client, yellow_pr, green_issue, (), current, verdict, "a5.2:" + "d" * 64,
                "a5.yellow-authorization:" + "d" * 64, "a5.yellow-blocker:" + "d" * 64,
                ("docs/change.md",), "."), "repair-pushed")
        self.assertEqual(verifier.call_count, 2)
        self.assertEqual(transition.call_count, 2)
        self.assertEqual(len(green_client.comments_written), 1)
        self.assertEqual(len(yellow_client.comments_written), 1)


if __name__ == "__main__":
    unittest.main()
