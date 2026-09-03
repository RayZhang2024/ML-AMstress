import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class AutonomousDevelopmentPolicyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = (ROOT / "docs" / "AUTONOMOUS_DEVELOPMENT.md").read_text(
            encoding="utf-8"
        )
        cls.agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")

    def test_policy_contains_required_governance_sections(self):
        required_sections = (
            "## Work-unit rules",
            "## Repository risk model",
            "### GREEN",
            "### YELLOW",
            "### RED",
            "## Effective-risk escalation",
            "## Merge authority",
            "## Credential and safety boundaries",
            "## Mandatory stop and escalation conditions",
        )
        for section in required_sections:
            self.assertIn(section, self.policy)

    def test_policy_preserves_safety_and_no_automation_boundary(self):
        for phrase in (
            "One issue per branch and pull request",
            "Necessity Gate",
            "duplicate concurrent implementations",
            "required validation cannot be run",
            "least-privilege GitHub permissions",
            "Never place unrestricted secrets",
            "Destructive operations",
            "this document does not",
            "does not enable merge",
            "never auto-merged",
            "issue requirements conflict",
            "scientific intent is ambiguous",
            "another implementation is already active",
        ):
            self.assertIn(phrase, self.policy)

    def test_agents_points_to_policy(self):
        self.assertIn("docs/AUTONOMOUS_DEVELOPMENT.md", self.agents)


if __name__ == "__main__":
    unittest.main()
