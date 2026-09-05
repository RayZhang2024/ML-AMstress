import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock

from scripts import a5_repair_worker as worker


class Issue94RepairGitIdentityTests(unittest.TestCase):
    @staticmethod
    def _git(cwd, *arguments, check=True):
        return subprocess.run(("git", *arguments), cwd=cwd, check=check, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    @staticmethod
    def _request(expected_head, branch, path="tracked.txt"):
        return worker.RepairRequest(
            schema_version=1,
            repository="RayZhang2024/ML-AMstress",
            pull_request_number=83,
            issue_number=82,
            branch=branch,
            expected_head_sha=expected_head,
            review_decision_key="a5.2:" + ("a" * 64),
            current_issue_status="status:in-progress",
            current_pr_review_state="review:blocker",
            review_state_head_sha=expected_head,
            effective_risk="green",
            accepted_findings=(worker.BlockerFinding(
                "F-1", "tests", "repair fixture value", "restore required value", "tests pass"),),
            allowed_paths=(path,),
            attempt_number=1,
        )

    def test_production_attempt_bound_is_unchanged(self):
        self.assertEqual(worker.MAX_REPAIR_ATTEMPTS, 2)

    def test_isolated_environment_removes_inherited_git_identity(self):
        inherited = {
            "GIT_AUTHOR_NAME": "runner-user",
            "GIT_AUTHOR_EMAIL": "runner@example.invalid",
            "GIT_COMMITTER_NAME": "runner-user",
            "GIT_COMMITTER_EMAIL": "runner@example.invalid",
        }
        with mock.patch.dict(os.environ, inherited, clear=False):
            environment = worker._isolated_environment()
        for name in inherited:
            self.assertNotIn(name, environment)
        self.assertEqual(environment["GIT_CONFIG_GLOBAL"], os.devnull)
        self.assertEqual(environment["GIT_CONFIG_NOSYSTEM"], "1")

    def test_commit_command_uses_only_explicit_a5_identity(self):
        expected_head = "a" * 40
        new_head = "b" * 40
        request = self._request(expected_head, "codex/issue-94-test")
        calls = []

        def run(command, cwd, env=None, input_text=None):
            calls.append((tuple(command), env))
            if tuple(command) == ("git", "rev-parse", "HEAD"):
                return mock.Mock(returncode=0, stdout=new_head + "\n", stderr="")
            return mock.Mock(returncode=0, stdout="", stderr="")

        inherited = {
            "GIT_AUTHOR_NAME": "runner-user",
            "GIT_AUTHOR_EMAIL": "runner@example.invalid",
            "GIT_COMMITTER_NAME": "runner-user",
            "GIT_COMMITTER_EMAIL": "runner@example.invalid",
        }
        with mock.patch.dict(os.environ, inherited, clear=False), \
             mock.patch.object(worker, "changed_paths", return_value=("tracked.txt",)), \
             mock.patch.object(worker, "_run", side_effect=run):
            result = worker.commit_repair(request, ".")

        self.assertEqual(result, new_head)
        commit_calls = [item for item in calls if "commit" in item[0]]
        self.assertEqual(len(commit_calls), 1)
        command, environment = commit_calls[0]
        self.assertIn("user.useConfigOnly=true", command)
        self.assertIn("user.name=" + worker.A5_GIT_COMMIT_NAME, command)
        self.assertIn("user.email=" + worker.A5_GIT_COMMIT_EMAIL, command)
        self.assertNotIn("runner-user", " ".join(command))
        self.assertNotIn("runner@example.invalid", " ".join(command))
        for name in inherited:
            self.assertNotIn(name, environment)
        self.assertEqual(environment["GIT_CONFIG_GLOBAL"], os.devnull)
        self.assertEqual(environment["GIT_CONFIG_NOSYSTEM"], "1")

    def test_isolated_repository_without_user_identity_can_create_trusted_repair_commit(self):
        with tempfile.TemporaryDirectory() as directory:
            repository = Path(directory)
            self._git(directory, "init", "-q")
            self._git(directory, "checkout", "-qb", "codex/issue-94-test")
            self._git(directory, "config", "user.name", "fixture setup")
            self._git(directory, "config", "user.email", "fixture@example.invalid")
            tracked = repository / "tracked.txt"
            tracked.write_text("before\n", encoding="utf-8")
            self._git(directory, "add", "tracked.txt")
            self._git(directory, "commit", "-qm", "fixture base")
            expected_head = self._git(directory, "rev-parse", "HEAD").stdout.strip()
            self._git(directory, "config", "--unset", "user.name")
            self._git(directory, "config", "--unset", "user.email")
            tracked.write_text("after\n", encoding="utf-8")

            inherited = {
                "GIT_AUTHOR_NAME": "runner-user",
                "GIT_AUTHOR_EMAIL": "runner@example.invalid",
                "GIT_COMMITTER_NAME": "runner-user",
                "GIT_COMMITTER_EMAIL": "runner@example.invalid",
            }
            request = self._request(expected_head, "codex/issue-94-test")
            with mock.patch.dict(os.environ, inherited, clear=False):
                new_head = worker.commit_repair(request, directory)

            self.assertNotEqual(new_head, expected_head)
            identity = self._git(directory, "show", "-s", "--format=%an|%ae|%cn|%ce", "HEAD").stdout.strip()
            expected_identity = "|".join((worker.A5_GIT_COMMIT_NAME, worker.A5_GIT_COMMIT_EMAIL,
                                          worker.A5_GIT_COMMIT_NAME, worker.A5_GIT_COMMIT_EMAIL))
            self.assertEqual(identity, expected_identity)
            self.assertNotIn("runner-user", identity)
            self.assertNotIn("runner@example.invalid", identity)
            self.assertNotIn("fixture setup", identity)
            self.assertNotIn("fixture@example.invalid", identity)
            self.assertNotEqual(self._git(directory, "config", "--local", "--get", "user.name", check=False).returncode, 0)
            self.assertNotEqual(self._git(directory, "config", "--local", "--get", "user.email", check=False).returncode, 0)

    def test_commit_failure_remains_bounded(self):
        expected_head = "a" * 40
        request = self._request(expected_head, "codex/issue-94-test")
        calls = []

        def run(command, cwd, env=None, input_text=None):
            calls.append(tuple(command))
            if "commit" in command:
                return mock.Mock(returncode=128, stdout="secret stdout", stderr="secret stderr")
            return mock.Mock(returncode=0, stdout="", stderr="")

        with mock.patch.object(worker, "changed_paths", return_value=("tracked.txt",)), \
             mock.patch.object(worker, "_run", side_effect=run):
            with self.assertRaisesRegex(worker.RepairError, "could not create repair commit") as caught:
                worker.commit_repair(request, ".")
        self.assertNotIn("secret", str(caught.exception))
        self.assertEqual(len([command for command in calls if "commit" in command]), 1)


if __name__ == "__main__":
    unittest.main()
