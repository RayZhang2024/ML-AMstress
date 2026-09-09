import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class NormalPythonCiWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = (ROOT / ".github" / "workflows" / "python-ci.yml").read_text(
            encoding="utf-8"
        )
        cls.development = (ROOT / "docs" / "DEVELOPMENT.md").read_text(
            encoding="utf-8"
        )

    def test_pull_request_trigger_and_read_only_permissions(self):
        self.assertIn("pull_request:", self.workflow)
        self.assertIn("      - main", self.workflow)
        self.assertIn("permissions:\n  contents: read\n\njobs:", self.workflow)
        for other_trigger in ("pull_request_target:", "push:", "schedule:", "workflow_call:"):
            self.assertNotIn(other_trigger, self.workflow)
        self.assertNotIn("contents: write", self.workflow)
        self.assertNotIn("pull-requests: write", self.workflow)
        self.assertNotIn("workflow_dispatch:", self.workflow)
        self.assertNotIn("inputs:", self.workflow)
        for write_permission in (
            "actions: write",
            "attestations: write",
            "checks: write",
            "deployments: write",
            "discussions: write",
            "id-token: write",
            "issues: write",
            "packages: write",
            "statuses: write",
            "workflows: write",
        ):
            self.assertNotIn(write_permission, self.workflow)

    def test_exact_pr_head_and_trusted_main_diff_are_checked(self):
        head_sha = "${{ github.event.pull_request.head.sha }}"
        self.assertIn("uses: actions/checkout@v4", self.workflow)
        self.assertIn("ref: " + head_sha, self.workflow)
        self.assertIn(
            'test "$(git rev-parse HEAD)" = "' + head_sha + '"',
            self.workflow,
        )
        self.assertIn(
            "git fetch --no-tags origin +refs/heads/main:refs/remotes/origin/main",
            self.workflow,
        )
        self.assertIn("git diff --check origin/main...HEAD", self.workflow)
        self.assertNotIn("continue-on-error", self.workflow)

    def test_safe_commands_and_headless_environment_are_defined(self):
        self.assertIn("python-version: \"3.11\"", self.workflow)
        self.assertIn("QT_QPA_PLATFORM: offscreen", self.workflow)
        self.assertIn("MPLBACKEND: Agg", self.workflow)
        self.assertIn("python -m py_compile AM_gui_v7.py data_extract.py", self.workflow)
        self.assertIn(
            'python -m unittest discover -s tests -p "test_*.py"',
            self.workflow,
        )
        for dependency in ("PyQt5", "numpy", "matplotlib", "joblib"):
            self.assertIn(dependency, self.workflow)
        for optional in ("PyVista", "PyVistaQt", "VTK", "Abaqus"):
            self.assertNotIn(
                "pip install " + optional,
                self.workflow,
            )

    def test_development_docs_match_ci_contract(self):
        self.assertIn(".github/workflows/python-ci.yml", self.development)
        self.assertIn("QT_QPA_PLATFORM=offscreen", self.development)
        self.assertIn('python -m unittest discover -s tests -p "test_*.py"', self.development)
        self.assertIn("PyVista, PyVistaQt, VTK, and Abaqus are not installed", self.development)


if __name__ == "__main__":
    unittest.main()
