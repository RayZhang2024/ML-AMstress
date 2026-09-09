import copy
from pathlib import Path
import unittest
import urllib.error
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator


OLD_HEAD = "a" * 40
NEW_HEAD = "b" * 40
THIRD_HEAD = "c" * 40
BRANCH = "codex/issue-219-repair-push-verification"


def pr(**changes):
    value = {
        "number": 219, "state": "open",
        "base": {"ref": "main", "repo": {"full_name": orchestrator.REPOSITORY}},
        "head": {"sha": OLD_HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}},
    }
    value.update(changes)
    return value


class QueuedClient:
    def __init__(self, values):
        self.values = list(values)
        self.reads = 0
        self.mutations = 0

    def pr(self, number):
        self.reads += 1
        if number != 219:
            raise AssertionError("unexpected PR")
        value = self.values.pop(0)
        if isinstance(value, BaseException):
            raise value
        return copy.deepcopy(value)


class RepairPushVerificationTests(unittest.TestCase):
    def verify(self, values):
        client, delays = QueuedClient(values), []
        with mock.patch.object(orchestrator.time, "sleep", side_effect=delays.append):
            result = orchestrator._verify_repair_push_head(client, pr(), OLD_HEAD, NEW_HEAD)
        return result, client, delays

    def test_stale_old_head_then_exact_new_head_succeeds_with_one_fixed_delay(self):
        result, client, delays = self.verify([pr(), pr(head={"sha": NEW_HEAD, "ref": BRANCH,
                                                              "repo": {"full_name": orchestrator.REPOSITORY}})])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.reads, 2)
        self.assertEqual(delays, [orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS])

    def test_immediate_exact_new_head_needs_one_read_and_no_delay(self):
        result, client, delays = self.verify([pr(head={"sha": NEW_HEAD, "ref": BRANCH,
                                                            "repo": {"full_name": orchestrator.REPOSITORY}})])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.reads, 1)
        self.assertEqual(delays, [])

    def test_persistent_old_head_is_bounded_and_does_not_mutate(self):
        client = QueuedClient([pr(), pr(), pr()])
        delays = []
        with mock.patch.object(orchestrator.time, "sleep", side_effect=delays.append), \
                self.assertRaisesRegex(orchestrator.OrchestrationError, "bounded re-read"):
            orchestrator._verify_repair_push_head(client, pr(), OLD_HEAD, NEW_HEAD)
        self.assertEqual(client.reads, orchestrator.MAX_REPAIR_PUSH_HEAD_VERIFICATION_READS)
        self.assertEqual(delays, [orchestrator.REPAIR_PUSH_HEAD_VERIFICATION_DELAY_SECONDS] * 2)
        self.assertLessEqual(sum(delays), 2)
        self.assertEqual(client.mutations, 0)

    def test_unexpected_third_head_fails_immediately(self):
        client = QueuedClient([pr(head={"sha": THIRD_HEAD, "ref": BRANCH,
                                        "repo": {"full_name": orchestrator.REPOSITORY}})])
        with mock.patch.object(orchestrator.time, "sleep") as sleep, \
                self.assertRaisesRegex(orchestrator.OrchestrationError, "unexpected head"):
            orchestrator._verify_repair_push_head(client, pr(), OLD_HEAD, NEW_HEAD)
        self.assertEqual(client.reads, 1)
        sleep.assert_not_called()

    def test_refreshed_identity_rejects_bad_numbers_and_repository_or_branch(self):
        bad_numbers = (True, False, 0, -1, 1.0, "219", None)
        for number in bad_numbers:
            with self.subTest(number=number):
                bad = pr(number=number, head={"sha": NEW_HEAD, "ref": BRANCH,
                                               "repo": {"full_name": orchestrator.REPOSITORY}})
                with self.assertRaises(orchestrator.OrchestrationError):
                    self.verify([bad])
        missing_number = pr(head={"sha": NEW_HEAD, "ref": BRANCH,
                                  "repo": {"full_name": orchestrator.REPOSITORY}})
        del missing_number["number"]
        with self.assertRaises(orchestrator.OrchestrationError):
            self.verify([missing_number])
        for head in (
            {"sha": NEW_HEAD, "ref": BRANCH, "repo": {"full_name": "other/repository"}},
            {"sha": NEW_HEAD, "ref": "codex/issue-219-other", "repo": {"full_name": orchestrator.REPOSITORY}},
            {"sha": NEW_HEAD, "ref": BRANCH, "repo": None},
        ):
            with self.subTest(head=head):
                with self.assertRaises(orchestrator.OrchestrationError):
                    self.verify([pr(head=head)])
        with self.assertRaises(orchestrator.OrchestrationError):
            self.verify([pr(base={"ref": "main", "repo": {"full_name": "other/repository"}},
                            head={"sha": NEW_HEAD, "ref": BRANCH,
                                  "repo": {"full_name": orchestrator.REPOSITORY}})])

    def test_transient_failure_retries_but_non_transient_failure_fails_closed(self):
        result, client, delays = self.verify([
            urllib.error.URLError(TimeoutError()),
            pr(head={"sha": NEW_HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}}),
        ])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.reads, 2)
        self.assertEqual(len(delays), 1)
        result, client, delays = self.verify([
            orchestrator.OrchestrationError("GitHub get-pr: transport timeout"),
            pr(head={"sha": NEW_HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}}),
        ])
        self.assertEqual(result["head"]["sha"], NEW_HEAD)
        self.assertEqual(client.reads, 2)
        self.assertEqual(len(delays), 1)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "read failed"):
            self.verify([ValueError("malformed")])

    def test_both_repair_lanes_use_the_shared_verifier(self):
        source = Path(orchestrator.__file__).read_text(encoding="utf-8")
        self.assertEqual(source.count("_verify_repair_push_head(client, pr, request.expected_head_sha, result.new_head_sha)"), 2)


if __name__ == "__main__":
    unittest.main()
