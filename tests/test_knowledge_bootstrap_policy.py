import json
import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import knowledge_bootstrap_policy as policy


SHA = "a" * 40


def payload(**updates):
    value = {
        "schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 283,
        "trusted_base_sha": SHA, "authorized_paths": ["README.md", "docs/KNOWLEDGE.json"],
        "purpose": "repository-knowledge-reconstruction", "a5_mode": "skip-review-and-repair",
        "scientific_runtime_prohibited": True,
    }
    value.update(updates)
    return value


def marker(value=None, author=policy.TRUSTED_AUTHOR):
    value = payload() if value is None else value
    return {"body": policy.MARKER_PREFIX + json.dumps(value, sort_keys=True, separators=(",", ":")) + policy.MARKER_SUFFIX,
            "user": {"login": author}}


class BootstrapPolicyTests(unittest.TestCase):
    def test_no_marker_zero_touch(self):
        class Trap:
            def __getattr__(self, name):
                raise AssertionError(name)
        self.assertIsNone(orchestrator._bootstrap_skip_if_authorized([{"body": "ordinary"}], client=Trap(), pr=Trap(), issue=Trap()))

    def test_marker_owner_and_canonical_standalone(self):
        self.assertEqual(policy.parse_marker(marker()["body"], expected_issue=283, expected_base=SHA).authorized_paths,
                         ("README.md", "docs/KNOWLEDGE.json"))

    def test_marker_malformed_or_nonstandalone_rejected(self):
        for body in (policy.MARKER_PREFIX + "{}" + policy.MARKER_SUFFIX, "x" + marker()["body"], marker()["body"] + "x"):
            with self.assertRaises(policy.PolicyError): policy.parse_marker(body)

    def test_marker_untrusted_rejected(self):
        self.assertIsNone(orchestrator._bootstrap_skip_if_authorized([marker(author="other")]))

    def test_marker_duplicate_or_conflicting_rejected(self):
        self.assertIsNone(orchestrator._bootstrap_skip_if_authorized([marker(), marker(payload(issue_number=284))]))

    def test_schema_and_required_values_rejected(self):
        for change in ({"schema_version": 2}, {"repository": "other/repo"}, {"issue_number": 0}, {"a5_mode": "review"}, {"scientific_runtime_prohibited": False}):
            with self.assertRaises(policy.PolicyError): policy.validate(payload(**change))

    def test_authorized_paths_sorted_unique_safe_and_bounded(self):
        for paths in (["docs/KNOWLEDGE.json", "README.md"], ["README.md", "README.md"], ["../README.md"], []):
            with self.assertRaises(policy.PolicyError): policy.validate(payload(authorized_paths=paths))

    def test_path_gate_allows_descriptive_knowledge(self):
        for path in ("README.md", "AGENTS.md", "docs/guide.md", "docs/data.json"):
            self.assertTrue(policy.is_eligible_path(path))

    def test_path_gate_rejects_protected_runtime_scientific_and_unauthorized(self):
        for path in ("scripts/x.py", ".github/workflows/x.yml", "docs/A5_4A_REVIEW_LOOP.md", "docs/ABAQUS_MODEL_CONTRACT.md", "docs/model.md"):
            self.assertFalse(policy.is_eligible_path(path))

    def test_d0_yellow_cross_evidence_must_match(self):
        comments, client, pr, issue, run = self._valid_context()
        with mock.patch.object(orchestrator, "validate_pr_identity", return_value=(1, "codex-yellow/issue-283-d0")), \
             mock.patch.object(orchestrator, "review_lane", return_value="automated-yellow"), \
             mock.patch.object(orchestrator, "canonical_linked_issue", return_value=283), \
             mock.patch.object(orchestrator, "_automated_yellow_authorized_paths", return_value=("README.md",)):
            self.assertIsNone(orchestrator._bootstrap_skip_if_authorized(
                comments, audit_comments=comments, client=client, pr=pr, issue=issue, run=run,
                branch="codex-yellow/issue-283-d0"))

    def _valid_context(self, ci="success"):
        comments = [marker()]
        class Client:
            def changed_files(self, number): return [{"filename": "README.md"}, {"filename": "docs/KNOWLEDGE.json"}]
            def comment(self, number, body): comments.append({"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})
        pr = {"number": 1, "base": {"sha": SHA}, "head": {"sha": SHA}}
        return comments, Client(), pr, {"number": 283}, orchestrator.WorkflowRun(1, SHA, ci)

    def _invoke_valid(self, ci="success", files=True):
        comments, client, pr, issue, run = self._valid_context(ci)
        if not files: client.changed_files = lambda number: [{"filename": "README.md"}]
        with mock.patch.object(orchestrator, "validate_pr_identity", return_value=(1, "codex-yellow/issue-283-d0")), \
             mock.patch.object(orchestrator, "review_lane", return_value="automated-yellow"), \
             mock.patch.object(orchestrator, "canonical_linked_issue", return_value=283), \
             mock.patch.object(orchestrator, "_automated_yellow_authorized_paths", return_value=("README.md", "docs/KNOWLEDGE.json")):
            return orchestrator._bootstrap_skip_if_authorized(comments, audit_comments=comments, client=client, pr=pr, issue=issue, run=run,
                                                               branch="codex-yellow/issue-283-d0", authorization_comments=[]), comments

    def test_valid_exact_head_successful_ci_skips_without_review_or_repair(self):
        result, comments = self._invoke_valid()
        self.assertEqual(result, "bootstrap-skip")
        self.assertEqual(len(comments), 2)

    def test_failed_ci_cannot_skip(self):
        self.assertIsNone(self._invoke_valid(ci="failure")[0])

    def test_stale_head_base_or_path_mismatch_cannot_skip(self):
        self.assertIsNone(self._invoke_valid(files=False)[0])

    def test_wrong_lane_or_issue_identity_cannot_skip(self):
        comments, client, pr, issue, run = self._valid_context()
        self.assertIsNone(orchestrator._bootstrap_skip_if_authorized(comments, client=client, pr=pr, issue=issue, run=run, branch="wrong"))

    def test_exact_replay_emits_one_audit_only(self):
        result, comments = self._invoke_valid()
        self.assertEqual(result, "bootstrap-skip")
        # Replay against the same persisted audit has no second side effect.
        with mock.patch.object(orchestrator, "validate_pr_identity", return_value=(1, "codex-yellow/issue-283-d0")), mock.patch.object(orchestrator, "review_lane", return_value="automated-yellow"), mock.patch.object(orchestrator, "canonical_linked_issue", return_value=283), mock.patch.object(orchestrator, "_automated_yellow_authorized_paths", return_value=("README.md", "docs/KNOWLEDGE.json")):
            self.assertEqual(orchestrator._bootstrap_skip_if_authorized(comments, audit_comments=comments, client=self._valid_context()[1], pr={"number": 1, "base": {"sha": SHA}, "head": {"sha": SHA}}, issue={"number": 283}, run=orchestrator.WorkflowRun(1, SHA, "success"), branch="codex-yellow/issue-283-d0"), "bootstrap-skip")
        self.assertEqual(len(comments), 2)

    def test_invalid_marker_falls_through_to_ordinary_a5(self):
        self.assertIsNone(orchestrator._bootstrap_skip_if_authorized([marker(payload(issue_number=1))]))


if __name__ == "__main__":
    unittest.main()
