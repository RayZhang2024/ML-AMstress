"""Deterministic bounded remote-head checks for Issue #228."""
import copy
import unittest
import urllib.error
from unittest import mock

from scripts import a5_repair_worker as repair
from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_yellow_repair_worker as yellow_repair
from scripts import a5_reviewer as reviewer
from scripts import yellow_lane_policy
from tests.test_issue75_review_orchestrator import HEAD, NEW_HEAD, pull_request


class SequencedClient:
    def __init__(self, observations):
        self.observations = list(observations)
        self.reads = []

    def pr(self, number):
        self.reads.append(number)
        item = self.observations.pop(0)
        if isinstance(item, Exception):
            raise item
        return copy.deepcopy(item)

    def __getattr__(self, name):
        raise AssertionError("verification must not call " + name)


def refreshed(**changes):
    value = pull_request()
    value["head"]["sha"] = NEW_HEAD
    value.update(changes)
    return value


class RepairPushHeadVerificationTests(unittest.TestCase):
    def verify(self, observations):
        client = SequencedClient(observations)
        with mock.patch.object(orchestrator.time, "sleep") as sleep:
            result = orchestrator._verify_repair_push_head(client, pull_request(), NEW_HEAD)
        return client, sleep, result

    def test_old_head_then_returned_head_succeeds_with_one_fixed_delay(self):
        client, sleep, result = self.verify([pull_request(), refreshed()])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.reads, [175, 175])
        sleep.assert_called_once_with(orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS)

    def test_immediate_returned_head_succeeds_without_delay(self):
        client, sleep, _ = self.verify([refreshed()])
        self.assertEqual(client.reads, [175])
        sleep.assert_not_called()

    def test_persistent_old_head_fails_after_exact_read_and_delay_budgets(self):
        client = SequencedClient([pull_request(), pull_request(), pull_request()])
        with mock.patch.object(orchestrator.time, "sleep") as sleep:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "remained at expected head"):
                orchestrator._verify_repair_push_head(client, pull_request(), NEW_HEAD)
        self.assertEqual(len(client.reads), orchestrator.MAX_REPAIR_PUSH_HEAD_VERIFICATION_READS)
        self.assertEqual(sleep.call_count, 2)
        self.assertLessEqual(sleep.call_count * orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS, 2)

    def test_unexpected_third_head_fails_closed_without_waiting(self):
        other = refreshed()
        other["head"]["sha"] = "c" * 40
        client = SequencedClient([other])
        with mock.patch.object(orchestrator.time, "sleep") as sleep:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "unexpected concurrent"):
                orchestrator._verify_repair_push_head(client, pull_request(), NEW_HEAD)
        self.assertEqual(len(client.reads), 1)
        sleep.assert_not_called()

    def test_repository_and_branch_mismatches_fail_closed(self):
        for change in (
                {"head": {"sha": NEW_HEAD, "ref": pull_request()["head"]["ref"], "repo": {"full_name": "other/repo"}}},
                {"head": {"sha": NEW_HEAD, "ref": "codex/issue-99-wrong", "repo": {"full_name": orchestrator.REPOSITORY}}},
        ):
            with self.subTest(change=change), self.assertRaises(orchestrator.OrchestrationError):
                self.verify([refreshed(**change)])

    def test_base_repository_mismatch_fails_closed(self):
        with self.assertRaises(orchestrator.OrchestrationError):
            self.verify([refreshed(base={"ref": "main", "sha": "c" * 40,
                                        "repo": {"full_name": "other/repo"}})])

    def test_malformed_or_mismatched_pr_number_fails_before_head_acceptance(self):
        for number in (None, True, False, 0, -1, "175", 175.0, 176):
            with self.subTest(number=number), self.assertRaises(orchestrator.OrchestrationError):
                self.verify([refreshed(number=number)])

    def test_transient_read_failure_retries_inside_same_budget_then_succeeds(self):
        client, sleep, result = self.verify([urllib.error.URLError(TimeoutError()), refreshed()])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(len(client.reads), 2)
        sleep.assert_called_once_with(orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS)

    def test_non_transient_read_failure_fails_immediately(self):
        client = SequencedClient([orchestrator.OrchestrationError("GitHub get-pr: HTTP 403")])
        with mock.patch.object(orchestrator.time, "sleep") as sleep:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "failed closed"):
                orchestrator._verify_repair_push_head(client, pull_request(), NEW_HEAD)
        self.assertEqual(len(client.reads), 1)
        sleep.assert_not_called()

    def test_helper_has_no_repair_or_state_side_effect_surface(self):
        client, _, _ = self.verify([refreshed()])
        self.assertEqual(client.reads, [175])
        self.assertEqual(orchestrator.MAX_REPAIR_ATTEMPTS, 2)

    def test_both_repair_lanes_transition_only_after_shared_verifier_confirms_head(self):
        class IntegrationClient(SequencedClient):
            def __init__(self, observations):
                super().__init__(observations)
                self.issue_data = {"number": 75, "labels": [{"name": "status:in-progress"}]}
                self.comment_data = []

            def issue(self, number):
                if number != 75:
                    raise AssertionError("wrong issue read")
                return self.issue_data

            def comments(self, number):
                if number != 175:
                    raise AssertionError("wrong comments read")
                return self.comment_data

            def comment(self, number, body):
                if number != 175:
                    raise AssertionError("wrong comment write")
                self.comment_data.append({"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})

        finding = reviewer.Finding("F-1", "tests", "test blocker", "repair", "[AC-1] pass")
        verdict = reviewer.ReviewVerdict(1, "blocker", HEAD, "green", "blocker", (finding,), "")
        result = repair.RepairResult(1, orchestrator.REPOSITORY, 175, 75, pull_request()["head"]["ref"],
                                     1, HEAD, NEW_HEAD, ("F-1",), ("docs/change.md",), "passed", "key")
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        for lane in ("green", "yellow"):
            with self.subTest(lane=lane):
                transitioned = mock.Mock()
                client = IntegrationClient([pull_request(), refreshed()])
                original_pr = pull_request()
                original_pr["head"]["ref"] = (pull_request()["head"]["ref"] if lane == "green"
                                                 else yellow_lane_policy.yellow_branch(75, "A5 automated yellow task"))
                for observation in client.observations:
                    if not isinstance(observation, Exception):
                        observation["head"]["ref"] = original_pr["head"]["ref"]
                original_pr["head"]["repo"] = {"full_name": orchestrator.REPOSITORY}
                with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                        mock.patch.object(orchestrator, "current_review_state", return_value=current), \
                        mock.patch.object(orchestrator, "apply_transition", transitioned), \
                        mock.patch.object(orchestrator.time, "sleep"):
                    if lane == "green":
                        with mock.patch.object(repair, "execute_repair", return_value=result) as execute:
                            self.assertEqual(orchestrator._repair(
                                client, original_pr, client.issue_data, (), current, verdict,
                                "a5.2:" + "d" * 64, ("docs/change.md",), "."), "repair-pushed")
                    else:
                        with mock.patch.object(yellow_repair, "validate_request"), \
                                mock.patch.object(yellow_repair, "execute_repair", return_value=result) as execute:
                            self.assertEqual(orchestrator._yellow_repair(
                                client, original_pr, client.issue_data, (), current, verdict,
                                "a5.2:" + "d" * 64, "a5.yellow-authorization:" + "e" * 64,
                                "a5.yellow-blocker:" + "f" * 64, ("docs/change.md",), "."), "repair-pushed")
                self.assertEqual(execute.call_count, 1)
                self.assertEqual(client.reads, [175, 175])
                transitioned.assert_called_once()


if __name__ == "__main__":
    unittest.main()
