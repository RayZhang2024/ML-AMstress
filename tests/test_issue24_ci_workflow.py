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
        self.assertIn("permissions:\n  contents: read", self.workflow)
        self.assertNotIn("contents: write", self.workflow)
        self.assertNotIn("pull-requests: write", self.workflow)

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
