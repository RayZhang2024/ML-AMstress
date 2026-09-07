import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class GovernanceRiskGateDocumentationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.development = (ROOT / "docs" / "AUTONOMOUS_DEVELOPMENT.md").read_text(
            encoding="utf-8"
        )
        cls.orchestration = (
            ROOT / "docs" / "AUTONOMOUS_ORCHESTRATION.md"
        ).read_text(encoding="utf-8")
        cls.runbook = (ROOT / "docs" / "AUTONOMOUS_WORKER_RUNBOOK.md").read_text(
            encoding="utf-8"
        )
        cls.all_docs = " ".join(
            (cls.development + "\n" + cls.orchestration + "\n" + cls.runbook).split()
        )

    def test_pre_start_effective_risk_assessment_fails_closed(self):
        for phrase in (
            "pre-start effective-risk assessment",
            "expected changed paths",
            "control-plane surface",
            "generated artifacts",
            "required evidence",
            "ordinary GREEN triggering is forbidden",
            "fail closed to `status:blocked`",
        ):
            self.assertIn(" ".join(phrase.split()), " ".join(self.development.split()))
        self.assertIn(
            "uncertain expected scope make ordinary GREEN triggering ineligible",
            " ".join(self.orchestration.split()),
        )

    def test_trigger_review_and_authorizations_are_separate(self):
        for phrase in (
            "fresh addition of this label",
            "Re-adding or leaving an existing label is not a retry",
            "Controlled runtime or scientific acceptance may still be pending",
            "Implementation authorization, controlled-runtime authorization, and merge",
            "reviewed current PR head SHA",
            "not issue completion",
        ):
            self.assertIn(" ".join(phrase.split()), self.all_docs)

    def test_dependency_release_and_canonical_pr_linkage_are_deterministic(self):
        self.assertIn("<!-- dependency-release:RayZhang2024/ML-AMstress#123 -->", self.orchestration)
        self.assertIn("Free-form prose cannot release a", self.orchestration)
        self.assertIn("Refs #123", self.orchestration)
        self.assertIn("exactly one standalone", self.orchestration)


if __name__ == "__main__":
    unittest.main()
