import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

from scripts import codex_issue_worker as worker
from scripts import yellow_lane_policy


ROOT = Path(__file__).resolve().parents[1]


class DirectEntrypointImportTests(unittest.TestCase):
    def test_direct_script_context_loads_yellow_policy_without_side_effects(self):
        """Reproduce ``python scripts/codex_issue_worker.py`` import semantics."""
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            scripts = root / "scripts"
            scripts.mkdir()
            source = (ROOT / "scripts" / "codex_issue_worker.py").read_text(encoding="utf-8")
            source = source.rsplit('\nif __name__ == "__main__":', 1)[0]
            source += (
                '\nif __name__ == "__main__":\n'
                '    policy = _yellow_policy()\n'
                '    print(policy.AUTOMATED_YELLOW_LANE + ":" + '
                '("same" if policy.green is sys.modules[__name__] else "duplicate"))\n'
            )
            (scripts / "codex_issue_worker.py").write_text(source, encoding="utf-8")
            shutil.copyfile(
                ROOT / "scripts" / "yellow_lane_policy.py",
                scripts / "yellow_lane_policy.py",
            )
            environment = os.environ.copy()
            for name in ("GITHUB_TOKEN", "GH_TOKEN", "OPENAI_API_KEY", "AUTOMATION_APP_TOKEN"):
                environment.pop(name, None)
            result = subprocess.run(
                (sys.executable, "-I", "scripts/codex_issue_worker.py"),
                cwd=root, env=environment, text=True,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False,
            )
        self.assertEqual(result.returncode, 0, result.stderr[:500])
        self.assertEqual(result.stdout.strip(), "automated-yellow:same")
        self.assertEqual(result.stderr, "")

    def test_normal_package_import_remains_canonical(self):
        policy = worker._yellow_policy()
        self.assertIs(policy, yellow_lane_policy)
        self.assertIs(policy.green, worker)


if __name__ == "__main__":
    unittest.main()
