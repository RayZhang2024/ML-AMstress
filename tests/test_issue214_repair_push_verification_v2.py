import copy
import json
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
PR_ONE_BRANCH = "codex/issue-1-repair-push-verification"
YELLOW_BRANCH = yellow_lane_policy.yellow_branch(214, "Repair push verification")


def pull_request(head=OLD_HEAD, branch=GREEN_BRANCH, number=214):
    return {"number": number, "head": {"sha": head, "ref": branch,
            "repo": {"full_name": orchestrator.REPOSITORY}}}


def validation_payload(**changes):
    value = {
        "schema_version": 1,
        "repository": orchestrator.REPOSITORY,
        "issue_number": 314,
        "pr_number": 214,
        "reviewed_head_sha": OLD_HEAD,
        "source": "trusted-check-artifact",
        "results": [
            {
                "name": "full-normal-python-tests",
                "command": "python -m unittest discover -s tests -p test_*.py",
                "status": "passed",
            },
            {
                "name": "focused-normal-python-tests",
                "command": "python -m unittest tests.test_issue214_repair_push_verification_v2",
                "status": "passed",
            },
            {
                "name": "diff-check-origin-main-head",
                "command": "git diff --check origin/main...HEAD",
                "status": "passed",
            },
            {
                "name": "python-syntax-compilation",
                "command": "python -m compileall -q .",
                "status": "passed",
            },
        ],
    }
    value.update(changes)
    return value


def trusted_comment(body):
    return {"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}}


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


class ExactHeadEvidenceMarkerTests(unittest.TestCase):
    def test_ac13_marker_requires_all_exact_head_validation_results(self):
        marker = orchestrator._ac13_validation_evidence_marker(validation_payload())
        payload = json.loads(marker.removeprefix("<!-- a5.ac13-validation-evidence:").removesuffix(" -->"))
        self.assertEqual(
            [item["name"] for item in payload["results"]],
            sorted(orchestrator.REQUIRED_AC13_VALIDATION_NAMES),
        )
        self.assertIn("git diff --check origin/main...HEAD", marker)
        accepted = orchestrator.accepted_ac13_validation_evidence(
            [trusted_comment(marker)], pull_request(), {"number": 314}, OLD_HEAD,
        )
        self.assertEqual(accepted, marker)

        incomplete = validation_payload(results=validation_payload()["results"][:-1])
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "incomplete"):
            orchestrator._ac13_validation_evidence_marker(incomplete)

        failed = validation_payload()
        failed["results"][0] = dict(failed["results"][0], status="failed")
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "did not pass"):
            orchestrator._ac13_validation_evidence_marker(failed)

    def test_ac13_evidence_is_exact_head_trusted_and_deterministic(self):
        marker = orchestrator._ac13_validation_evidence_marker(validation_payload())
        comments = [trusted_comment(marker)]
        self.assertEqual(orchestrator.accepted_ac13_validation_evidence(
            comments, pull_request(), {"number": 314}, OLD_HEAD,
        ), marker)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "missing or ambiguous"):
            orchestrator.accepted_ac13_validation_evidence(
                comments, pull_request(), {"number": 314}, NEW_HEAD,
            )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "missing or ambiguous"):
            orchestrator.accepted_ac13_validation_evidence(
                [{"body": marker, "user": {"login": "untrusted"}}],
                pull_request(), {"number": 314}, OLD_HEAD,
            )
        noncanonical = "<!-- a5.ac13-validation-evidence:" + json.dumps(
            validation_payload(), sort_keys=False
        ) + " -->"
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "not deterministic"):
            orchestrator.accepted_ac13_validation_evidence(
                [trusted_comment(noncanonical)], pull_request(), {"number": 314}, OLD_HEAD,
            )

    def test_ac14_result_binds_current_main_success_to_ac13_evidence(self):
        validation_marker = orchestrator._ac13_validation_evidence_marker(validation_payload())
        validation_key = orchestrator._validation_evidence_key(validation_marker)
        result_payload = {
            "schema_version": 1,
            "repository": orchestrator.REPOSITORY,
            "issue_number": 314,
            "pr_number": 214,
            "reviewed_head_sha": OLD_HEAD,
            "context": "trusted-current-main",
            "status": "success",
            "validation_evidence_key": validation_key,
            "unauthorized_repair": False,
            "synthesized_evidence": False,
        }
        result_marker = orchestrator._trusted_current_main_a5_result_marker(result_payload)
        comments = [trusted_comment(validation_marker), trusted_comment(result_marker)]
        self.assertEqual(orchestrator.accepted_trusted_current_main_a5_result(
            comments, pull_request(), {"number": 314}, OLD_HEAD,
        ), result_marker)

        conflicting = dict(result_payload, validation_evidence_key="a5.ac13:" + "0" * 64)
        conflict_marker = orchestrator._trusted_current_main_a5_result_marker(conflicting)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "conflicts"):
            orchestrator.accepted_trusted_current_main_a5_result(
                [trusted_comment(validation_marker), trusted_comment(conflict_marker)],
                pull_request(), {"number": 314}, OLD_HEAD,
            )

    def test_ac14_rejects_unauthorized_or_synthesized_success_claims(self):
        validation_marker = orchestrator._ac13_validation_evidence_marker(validation_payload())
        base = {
            "schema_version": 1,
            "repository": orchestrator.REPOSITORY,
            "issue_number": 314,
            "pr_number": 214,
            "reviewed_head_sha": OLD_HEAD,
            "context": "trusted-current-main",
            "status": "success",
            "validation_evidence_key": orchestrator._validation_evidence_key(validation_marker),
            "unauthorized_repair": False,
            "synthesized_evidence": False,
        }
        for field in ("unauthorized_repair", "synthesized_evidence"):
            with self.subTest(field=field):
                invalid = dict(base, **{field: True})
                with self.assertRaisesRegex(orchestrator.OrchestrationError, "malformed"):
                    orchestrator._trusted_current_main_a5_result_marker(invalid)

    def test_clean_a5_result_is_persisted_only_after_ac13_evidence_exists(self):
        client = SequencedClient(())
        pr = pull_request()
        issue = {"number": 314}
        orchestrator.maybe_persist_trusted_current_main_a5_result(
            client, [], pr, issue, OLD_HEAD,
        )
        self.assertEqual(client.comments_written, [])

        validation_marker = orchestrator._ac13_validation_evidence_marker(validation_payload())
        comments = [trusted_comment(validation_marker)]
        orchestrator.maybe_persist_trusted_current_main_a5_result(
            client, comments, pr, issue, OLD_HEAD,
        )
        self.assertEqual(len(client.comments_written), 1)
        result_marker = client.comments_written[0]["body"]
        self.assertEqual(orchestrator.accepted_trusted_current_main_a5_result(
            comments + client.comments_written, pr, issue, OLD_HEAD,
        ), result_marker)

        orchestrator.maybe_persist_trusted_current_main_a5_result(
            client, comments + client.comments_written, pr, issue, OLD_HEAD,
        )
        self.assertEqual(len(client.comments_written), 1)


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

    def test_pr_one_refreshed_boolean_number_fails_before_transition(self):
        verdict = reviewer.ReviewVerdict(1, "blocker", OLD_HEAD, "green", "blocked", (
            reviewer.Finding("F-1", "policy", "repair", "repair", "[AC-7] repair"),), "")
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", OLD_HEAD)
        client = SequencedClient((pull_request(NEW_HEAD, PR_ONE_BRANCH, True),), PR_ONE_BRANCH)
        result = repair.RepairResult(1, orchestrator.REPOSITORY, 1, 214, PR_ONE_BRANCH, 1,
                                     OLD_HEAD, NEW_HEAD, ("F-1",), ("scripts/a5_review_orchestrator.py",),
                                     "passed", "a5.3:" + "d" * 64)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(repair, "execute_repair", return_value=result), \
                mock.patch.object(orchestrator, "apply_transition") as transition:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "PR identity is malformed"):
                orchestrator._repair(
                    client, pull_request(branch=PR_ONE_BRANCH, number=1), {"number": 214}, (),
                    current, verdict, "a5.2:" + "d" * 64,
                    ("scripts/a5_review_orchestrator.py",), ".")
        self.assertEqual(client.pr_calls, 1)
        transition.assert_not_called()

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
