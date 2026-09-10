import json
import unittest

from scripts import knowledge_bootstrap_policy as policy
from scripts import a5_review_orchestrator as orchestrator
from scripts import yellow_lane_policy


SHA = "a" * 40


def value(**changes):
    result = {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 276,
              "trusted_base_sha": SHA, "authorized_paths": ["docs/authority-index.json", "docs/knowledge.md"],
              "purpose": "repository-knowledge-reconstruction", "a5_mode": "skip-review-and-repair",
              "scientific_runtime_prohibited": True}
    result.update(changes)
    return result


def comment(raw=None, author=policy.TRUSTED_AUTHOR):
    raw = policy.serialize_evidence(value()) if raw is None else raw
    return {"body": "<!-- knowledge-bootstrap-prestart:" + raw + " -->", "user": {"login": author}}


class BootstrapEvidenceTests(unittest.TestCase):
    def test_canonical_owner_standalone_marker(self):
        raw = policy.serialize_evidence(value())
        self.assertEqual(policy.extract_evidence([comment(raw)], expected_issue=276, expected_base=SHA).authorized_paths,
                         ("docs/authority-index.json", "docs/knowledge.md"))

    def test_invalid_schema_and_values_fail_closed(self):
        for changes in ({"schema_version": 2}, {"repository": "other/repo"}, {"issue_number": 0},
                        {"trusted_base_sha": "bad"}, {"authorized_paths": []},
                        {"authorized_paths": ["docs/z.md", "docs/a.md"]},
                        {"authorized_paths": ["docs/a.md", "docs/a.md"]},
                        {"purpose": "other"}, {"a5_mode": "review"},
                        {"scientific_runtime_prohibited": False}):
            with self.subTest(changes=changes), self.assertRaises(policy.PolicyError):
                policy.serialize_evidence(value(**changes))
        raw = policy.serialize_evidence(value())
        for bad in ("{bad", raw.replace("\"purpose\"", "\"extra\":1,\"purpose\""),
                    raw.replace("\"schema_version\":1", "\"schema_version\":1,\"schema_version\":1")):
            with self.subTest(bad=bad), self.assertRaises(policy.PolicyError): policy.parse_evidence(bad)

    def test_marker_requires_one_exact_trusted_standalone_body(self):
        raw = policy.serialize_evidence(value())
        for comments in ([comment(raw, "other")], [comment(raw), comment(raw)],
                         [{"body": "prefix " + comment(raw)["body"], "user": {"login": policy.TRUSTED_AUTHOR}}], []):
            with self.subTest(comments=comments), self.assertRaises(policy.PolicyError): policy.extract_evidence(comments)

    def test_cross_evidence_and_changed_paths_must_match(self):
        evidence = policy.extract_evidence([comment()])
        yellow = type("Y", (), {"authorized_paths": evidence.authorized_paths})()
        self.assertEqual(policy.validate_activation([comment()], yellow, evidence.authorized_paths,
                                                    expected_issue=276, expected_base=SHA), evidence)
        for paths, number, base in ((("docs/other.md",), 276, SHA), (evidence.authorized_paths, 277, SHA),
                                    (evidence.authorized_paths, 276, "b" * 40)):
            with self.subTest(paths=paths, number=number, base=base), self.assertRaises(policy.PolicyError):
                policy.validate_activation([comment()], yellow, paths, expected_issue=number, expected_base=base)


class BootstrapPathTests(unittest.TestCase):
    def test_only_descriptive_knowledge_identities_are_eligible(self):
        for path in ("README.md", "AGENTS.md", "docs/plain.md", "docs/map.json"):
            self.assertTrue(policy.bootstrap_path_eligible(path))
        for path in (".github/workflows/test.yml", "scripts/x.py", "tests/x.py", "AM_gui_v7.py",
                     "docs/AUTONOMOUS_DEVELOPMENT.md", "docs/AUTONOMOUS_ORCHESTRATION.md",
                     "docs/AUTONOMOUS_KNOWLEDGE_BOOTSTRAP.md", "docs/A5_4A_REVIEW_LOOP.md",
                     "docs/A4_x.md", "docs/A6_x.md", "docs/A7_x.md", "docs/ABAQUS_MODEL_CONTRACT.md",
                     ".github/ISSUE_TEMPLATE/autonomous-work.md"):
            with self.subTest(path=path): self.assertFalse(policy.bootstrap_path_eligible(path))


class BootstrapOrchestratorTests(unittest.TestCase):
    def test_valid_activation_skips_once_without_reviewer_or_repair(self):
        title = "Bootstrap knowledge"
        branch = yellow_lane_policy.yellow_branch(276, title)
        yellow = {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 276,
                  "trusted_base_sha": SHA, "declared_risk": "yellow", "effective_risk": "yellow",
                  "authorized_paths": value()["authorized_paths"], "scientific_runtime_prohibited": True}
        issue_comments = [
            {"body": "<!-- a5.yellow-prestart:" + yellow_lane_policy.serialize_prestart(yellow) + " -->",
             "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}},
            {"body": "<!-- a5.yellow-claim:" + yellow_lane_policy.serialize_claim(
                {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 276,
                 "trusted_base_sha": SHA, "branch": branch, "lane": yellow_lane_policy.AUTOMATED_YELLOW_LANE}) + " -->",
             "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}}, comment()]
        pr = {"number": 376, "base": {"sha": SHA}, "head": {"sha": "b" * 40, "ref": branch,
              "repo": {"full_name": policy.REPOSITORY}}}
        class Client:
            def __init__(self): self.created = []
            def changed_files(self, number): return [{"filename": path} for path in value()["authorized_paths"]]
            def comment(self, number, body): self.created.append((number, body))
        client = Client()
        self.assertTrue(orchestrator._bootstrap_skip_if_authorized(client, pr, issue_comments, [], 276))
        self.assertEqual(len(client.created), 1)
        self.assertIn("skipped-under-d0", client.created[0][1])
        self.assertIn("required", client.created[0][1])
        self.assertFalse(orchestrator._bootstrap_skip_if_authorized(
            client, pr, issue_comments, [{"body": client.created[0][1], "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}}], 276) is False)
        self.assertEqual(len(client.created), 1)
