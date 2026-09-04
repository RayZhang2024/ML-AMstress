import inspect
import json
import unittest

from scripts import a5_review_state as contract

HEAD = "a" * 40
NEW_HEAD = "b" * 40


def state(**changes):
    value = dict(schema_version=1, repository="RayZhang2024/ML-AMstress", pull_request_number=71, issue_number=71,
                 current_head_sha=HEAD, current_issue_status="status:review", current_pr_review_state="review:pending",
                 review_state_head_sha=HEAD, validated_verdict=None, event_kind="verdict")
    value.update(changes)
    return contract.ReviewStateInput(**value)


def verdict(kind, sha=HEAD, ids=()):
    return contract.ValidatedVerdict(kind, sha, "green", ids)


class ReviewStateContractTests(unittest.TestCase):
    def test_initialize_and_canonical_verdicts(self):
        plan = contract.transition(state(current_pr_review_state=None, review_state_head_sha=None, validated_verdict=None, event_kind="initialize"))
        self.assertEqual((plan.next_issue_status, plan.next_pr_review_state, plan.review_state_head_sha), ("status:review", "review:pending", HEAD))
        for kind, issue, review in (("clean", "status:review", "review:clean"), ("blocker", "status:in-progress", "review:blocker"), ("escalate", "status:blocked", "review:escalated")):
            plan = contract.transition(state(validated_verdict=verdict(kind)))
            self.assertEqual((plan.next_issue_status, plan.next_pr_review_state), (issue, review))

    def test_stale_conflicting_and_duplicate_verdicts(self):
        with self.assertRaises(contract.ReviewStateError):
            contract.transition(state(validated_verdict=verdict("clean", NEW_HEAD)))
        with self.assertRaises(contract.ReviewStateError):
            contract.transition(state(current_pr_review_state="review:clean", current_issue_status="status:review", validated_verdict=verdict("blocker")))
        duplicate = contract.transition(state(current_pr_review_state="review:clean", current_issue_status="status:review", validated_verdict=verdict("clean")))
        self.assertTrue(duplicate.idempotent_no_op)

    def test_clean_and_blocker_new_heads_return_to_pending(self):
        clean = state(current_head_sha=NEW_HEAD, current_pr_review_state="review:clean", review_state_head_sha=HEAD, current_issue_status="status:review", validated_verdict=None, event_kind="new_head")
        blocker = state(current_head_sha=NEW_HEAD, current_pr_review_state="review:blocker", review_state_head_sha=HEAD, current_issue_status="status:in-progress", validated_verdict=None, event_kind="new_head")
        for item in (clean, blocker):
            plan = contract.transition(item)
            self.assertEqual((plan.next_issue_status, plan.next_pr_review_state, plan.review_state_head_sha), ("status:review", "review:pending", NEW_HEAD))

    def test_escalation_persists_and_human_release_resets(self):
        blocked = state(current_head_sha=NEW_HEAD, current_pr_review_state="review:escalated", review_state_head_sha=HEAD, current_issue_status="status:blocked", validated_verdict=None, event_kind="new_head")
        plan = contract.transition(blocked)
        self.assertTrue(plan.idempotent_no_op)
        self.assertEqual(plan.review_state_head_sha, HEAD)
        released = contract.transition(state(current_head_sha=NEW_HEAD, current_pr_review_state="review:escalated", review_state_head_sha=HEAD, current_issue_status="status:blocked", validated_verdict=None, event_kind="human_release"))
        self.assertEqual((released.next_issue_status, released.next_pr_review_state, released.review_state_head_sha), ("status:review", "review:pending", NEW_HEAD))

    def test_invalid_multiple_and_impossible_states_fail_closed(self):
        invalid = (state(current_issue_status=["status:review", "status:blocked"]), state(current_pr_review_state=["review:pending", "review:clean"]), state(current_pr_review_state="review:blocker", current_issue_status="status:review"), state(current_pr_review_state="review:clean", review_state_head_sha=NEW_HEAD, validated_verdict=verdict("clean")))
        for item in invalid:
            with self.assertRaises(contract.ReviewStateError):
                contract.transition(item)

    def test_decision_key_audit_and_pure_boundary(self):
        item = state(validated_verdict=verdict("blocker", ids=("F-1",)))
        self.assertEqual(contract.decision_key(item), contract.decision_key(item))
        initialize = state(current_pr_review_state=None, review_state_head_sha=None, validated_verdict=None, event_kind="initialize")
        clean = state(validated_verdict=verdict("clean"))
        self.assertNotEqual(contract.decision_key(initialize), contract.decision_key(clean))
        self.assertNotEqual(contract.decision_key(clean), contract.decision_key(item))
        changed = state(current_head_sha=NEW_HEAD, review_state_head_sha=NEW_HEAD, validated_verdict=verdict("blocker", NEW_HEAD, ("F-1",)))
        self.assertNotEqual(contract.decision_key(item), contract.decision_key(changed))
        serialized = contract.serialize_audit(item, contract.transition(item))
        self.assertEqual(serialized, contract.serialize_audit(item, contract.transition(item)))
        audit = json.loads(serialized)
        self.assertEqual(audit["finding_ids"], ["F-1"])
        self.assertNotIn("summary", audit)
        with self.assertRaises(contract.ReviewStateError):
            contract.transition(state(validated_verdict=verdict("blocker", ids=("unsafe",))))
        source = inspect.getsource(contract).lower()
        for forbidden in ("subprocess", "requests", "urllib", "github", "socket", "http.client"):
            self.assertNotIn(forbidden, source)
