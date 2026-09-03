import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class AutonomousOrchestrationDocumentationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = (ROOT / "docs" / "AUTONOMOUS_DEVELOPMENT.md").read_text(
            encoding="utf-8"
        )
        cls.orchestration = (
            ROOT / "docs" / "AUTONOMOUS_ORCHESTRATION.md"
        ).read_text(encoding="utf-8")
        cls.template = (
            ROOT / ".github" / "ISSUE_TEMPLATE" / "autonomous-work.md"
        ).read_text(encoding="utf-8")

    def test_label_vocabulary_and_lifecycle_are_explicit(self):
        for label in (
            "status:ready",
            "status:in-progress",
            "status:review",
            "status:blocked",
            "risk:green",
            "risk:yellow",
            "risk:red",
            "agent:codex",
            "agent:gpt-review",
        ):
            self.assertIn(label, self.orchestration)
        self.assertIn("status:ready -> status:in-progress", self.orchestration)
        self.assertIn("status:review -> closed", self.orchestration)
        self.assertIn("status:blocked -> status:ready", self.orchestration)

    def test_dependency_grammar_and_eligibility_are_machine_readable(self):
        self.assertRegex(self.orchestration, r"blocked-by:\s+#22")
        self.assertRegex(
            self.orchestration,
            r"blocked-by:\s+RayZhang2024/ML-AMstress#123",
        )
        for phrase in (
            "The issue is open.",
            "exactly one `status:ready`",
            "Every declared dependency is satisfied.",
            "No active implementation already exists",
            "must stop and set/report `status:blocked`",
        ):
            self.assertIn(phrase, self.orchestration)
        self.assertRegex(
            self.orchestration,
            r"must reject\s+unknown keys",
        )
        self.assertIn("AUTONOMOUS_ORCHESTRATION.md", self.policy)

    def test_issue_contract_template_has_required_sections_without_labels(self):
        required = (
            "## Goal",
            "## Necessity Gate",
            "## Required behavior",
            "## Do not change",
            "## Acceptance criteria",
            "## Tests/validation",
            "## Risk classification",
            "## Dependencies",
        )
        for section in required:
            self.assertIn(section, self.template)
        self.assertIn('labels: ""', self.template)
        self.assertIn("- none", self.template)
        self.assertIn(".github/workflows/codex-green-worker.yml", self.orchestration)
        self.assertIn("does not activate Codex workers", self.orchestration)


if __name__ == "__main__":
    unittest.main()
