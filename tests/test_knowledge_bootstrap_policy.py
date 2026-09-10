import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import knowledge_bootstrap_policy as policy


SHA = "b299d8daab1696c7880986ea04bfac76327595b3"


class Sentinel:
    def __getattribute__(self, name):
        raise AssertionError("no-marker probe touched " + name)


class KnowledgeBootstrapPolicyTests(unittest.TestCase):
    def test_no_marker_never_touches_unusable_inputs_or_helpers(self):
        with mock.patch.object(orchestrator, "validate_pr_identity", side_effect=AssertionError), \
             mock.patch.object(orchestrator, "_automated_yellow_authorized_paths", side_effect=AssertionError), \
             mock.patch.object(policy, "authorized_marker", side_effect=AssertionError):
            self.assertEqual("not-applicable", orchestrator._bootstrap_skip_if_authorized(
                Sentinel(), Sentinel(), Sentinel(), Sentinel(), [{"body": "ordinary comment"}]))

    def test_marker_is_canonical_owner_standalone_and_eligible(self):
        value = {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 280,
                 "trusted_base_sha": SHA, "authorized_paths": ["README.md", "docs/guide.json"],
                 "purpose": "repository-knowledge-reconstruction", "a5_mode": "skip-review-and-repair",
                 "scientific_runtime_prohibited": True}
        body = policy.marker(value)
        evidence = policy.authorized_marker([{"body": body, "user": {"login": "RayZhang2024"}}])
        self.assertEqual(("README.md", "docs/guide.json"), evidence.authorized_paths)
        self.assertFalse(policy.eligible_path("scripts/a.py"))
        self.assertFalse(policy.eligible_path("docs/A5_4A_REVIEW_LOOP.md"))
        with self.assertRaises(policy.PolicyError):
            policy.authorized_marker([{"body": body + " extra", "user": {"login": "RayZhang2024"}}])
