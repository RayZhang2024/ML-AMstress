import inspect
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock

from scripts import a5_repair_worker as worker


HEAD = "a" * 40
NEW_HEAD = "b" * 40


def request(**changes):
    value = dict(schema_version=1, repository="RayZhang2024/ML-AMstress", pull_request_number=73,
                 issue_number=73, branch="codex/issue-73-repair", expected_head_sha=HEAD,
                 review_decision_key="a5.2:" + ("a" * 64), current_issue_status="status:in-progress",
                 current_pr_review_state="review:blocker", review_state_head_sha=HEAD,
                 effective_risk="green", accepted_findings=(worker.BlockerFinding("F-1", "tests", "message",
                 "change the test", "test evidence"),), allowed_paths=("tests/test_issue73_repair_worker.py",),
                 attempt_number=1)
    value.update(changes)
    return worker.RepairRequest(**value)


class RepairRequestTests(unittest.TestCase):
    def test_parse_freezes_valid_request_and_key_is_stable(self):
        item = request()
        payload = dict(item.__dict__, accepted_findings=[item.accepted_findings[0].__dict__], allowed_paths=list(item.allowed_paths))
        parsed = worker.parse_repair_request(payload)
        self.assertEqual(parsed, item)
        self.assertEqual(worker.repair_decision_key(item), worker.repair_decision_key(parsed))
        self.assertNotEqual(worker.repair_decision_key(item), worker.repair_decision_key(request(attempt_number=2)))
        self.assertNotEqual(worker.repair_decision_key(item), worker.repair_decision_key(request(expected_head_sha=NEW_HEAD,
                                                                                              review_state_head_sha=NEW_HEAD)))

    def test_state_heads_risk_attempt_and_findings_fail_closed(self):
        invalid = (request(current_issue_status="status:review"), request(current_pr_review_state="review:pending"),
                   request(review_state_head_sha=NEW_HEAD), request(effective_risk="yellow"),
                   request(effective_risk="red"), request(attempt_number=3), request(accepted_findings=()),
                   request(accepted_findings=(worker.BlockerFinding("F-1", "tests", "m", "a", "e"),
                                              worker.BlockerFinding("F-1", "tests", "m", "a", "e"))),
                   request(accepted_findings=(worker.BlockerFinding("F-1", "scientific", "m", "a", "e"),)))
        for item in invalid:
            with self.assertRaises(worker.RepairError):
                worker.validate_request(item)

    def test_path_and_text_policy_is_exact_and_protected(self):
        for path in ("/absolute", "C:/absolute", "../escape", "tests/../escape", ".github/x.yml",
                     "scripts/codex_issue_worker.py", "scripts/a6_abaqus_preflight.py",
                     "scripts/a6_abaqus_probe.py", "AGENTS.md"):
            with self.assertRaises(worker.RepairError):
                worker.validate_request(request(allowed_paths=(path,)))
        with self.assertRaises(worker.RepairError):
            worker.validate_request(request(allowed_paths=("tests/a.py", "tests/a.py")))
        for branch in ("-branch", "branch.lock", "refs/.hidden", "branch@{1}"):
            with self.assertRaises(worker.RepairError):
                worker.validate_request(request(branch=branch))
        with self.assertRaises(worker.RepairError):
            worker.validate_request(request(accepted_findings=(worker.BlockerFinding("F-1", "tests", "x" * 1001, "a", "e"),)))

    def test_prompt_is_bounded_trusted_context(self):
        prompt = worker.build_repair_prompt(request())
        self.assertIn("Do not query GitHub", prompt)
        self.assertIn("only these exact repository-relative paths", prompt)
        self.assertNotIn(HEAD, prompt)

    def test_large_bounded_prompt_is_passed_only_on_stdin(self):
        item = request(accepted_findings=(worker.BlockerFinding("F-1", "tests", "m" * worker.MAX_TEXT,
                                                                 "a" * worker.MAX_TEXT, "e" * worker.MAX_TEXT),))
        completed = mock.Mock(returncode=0, stdout="", stderr="")
        with mock.patch.object(worker, "resolve_codex_executable", return_value="codex"), \
             mock.patch.object(worker, "_run", return_value=completed) as run:
            worker.run_codex(item, ".")
        self.assertGreater(len(run.call_args.args[3]), 2000)
        self.assertNotIn(run.call_args.args[3], run.call_args.args[0])


class RepairExecutionTests(unittest.TestCase):
    @staticmethod
    def _git(cwd, *arguments):
        return subprocess.run(("git", *arguments), cwd=cwd, check=True, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    def test_preflight_requires_clean_branch_and_exact_head(self):
        with mock.patch.object(worker, "_git_text", side_effect=("", "wrong", HEAD)):
            with self.assertRaises(worker.RepairError):
                worker.preflight(request(), ".")
        with mock.patch.object(worker, "_git_text", side_effect=("", request().branch, NEW_HEAD)):
            with self.assertRaises(worker.RepairError):
                worker.preflight(request(), ".")

    def test_codex_command_stdin_and_secret_isolation(self):
        completed = mock.Mock(returncode=0, stdout="", stderr="")
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "app-secret"}, clear=False), \
             mock.patch.object(worker, "resolve_codex_executable", return_value="codex.exe"), \
             mock.patch.object(worker, "_run", return_value=completed) as run:
            worker.run_codex(request(), ".")
        command, _, environment, prompt = run.call_args.args
        self.assertEqual(command[-1], "-")
        self.assertIn("workspace-write", command)
        self.assertIn('approval_policy="never"', command)
        self.assertNotIn(prompt, command)
        self.assertEqual(environment["GIT_CONFIG_GLOBAL"], os.devnull)
        self.assertEqual(environment["GIT_TERMINAL_PROMPT"], "0")
        self.assertNotIn("AUTOMATION_APP_TOKEN", environment)

    def test_codex_failure_does_not_expose_streams(self):
        completed = mock.Mock(returncode=9, stdout="secret stdout", stderr="secret stderr")
        with mock.patch.object(worker, "resolve_codex_executable", return_value="codex"), \
             mock.patch.object(worker, "_run", return_value=completed):
            with self.assertRaisesRegex(worker.RepairError, "Codex execution failed") as caught:
                worker.run_codex(request(), ".")
        self.assertNotIn("secret", str(caught.exception))

    def test_post_codex_identity_rejects_model_commit_or_checkout(self):
        with mock.patch.object(worker, "_git_text", side_effect=(request().branch, NEW_HEAD)):
            with self.assertRaises(worker.RepairError):
                worker.post_codex_identity(request(), ".")

    def test_change_scope_rejects_untrusted_or_protected_paths(self):
        with self.assertRaises(worker.RepairError):
            worker.enforce_change_scope(request(), ("docs/x.md",))
        with self.assertRaises(worker.RepairError):
            worker.enforce_change_scope(request(allowed_paths=(".github/x",)), (".github/x",))

    def test_changed_paths_reports_tracked_modification_once(self):
        with tempfile.TemporaryDirectory() as directory:
            repository = Path(directory)
            self._git(directory, "init", "-q")
            self._git(directory, "config", "user.name", "A5 test")
            self._git(directory, "config", "user.email", "a5@example.invalid")
            tracked = repository / "tracked.txt"
            tracked.write_text("before\n", encoding="utf-8")
            self._git(directory, "add", "tracked.txt")
            self._git(directory, "commit", "-qm", "initial")
            expected_head = self._git(directory, "rev-parse", "HEAD").stdout.strip()

            tracked.write_text("after\n", encoding="utf-8")

            paths = worker.changed_paths(directory, expected_head)
            self.assertEqual(paths, ("tracked.txt",))
            worker.enforce_change_scope(request(allowed_paths=("tracked.txt",)), paths)

    def test_validation_uses_argv_and_prevents_commit_push(self):
        calls = []
        def record(command, cwd, env=None, input_text=None):
            calls.append(command)
            return mock.Mock(returncode=1 if command[0] == "bad" else 0, stdout="", stderr="")
        with mock.patch.object(worker, "_run", side_effect=record):
            with self.assertRaises(worker.RepairError):
                worker.run_validation(".", (("bad", "value"),))
        self.assertEqual(calls[0], ("bad", "value"))
        source = inspect.getsource(worker.run_validation)
        self.assertNotIn("shell=True", source)

    def test_execute_stops_before_commit_when_validation_fails(self):
        with mock.patch.object(worker, "preflight"), mock.patch.object(worker, "run_codex"), \
             mock.patch.object(worker, "post_codex_identity"), \
             mock.patch.object(worker, "changed_paths", return_value=request().allowed_paths), \
             mock.patch.object(worker, "run_validation", side_effect=worker.RepairError("local validation failed")), \
             mock.patch.object(worker, "commit_repair") as commit:
            with self.assertRaises(worker.RepairError):
                worker.execute_repair(request(), ".")
        commit.assert_not_called()

    def test_result_is_bounded_audit_only(self):
        result = worker.RepairResult(1, "RayZhang2024/ML-AMstress", 73, 73, "branch", 1, HEAD, NEW_HEAD,
                                     ("F-1",), ("tests/x.py",), "passed", "a5.3:" + ("c" * 64))
        rendered = worker.serialize_result(result)
        self.assertEqual(json.loads(rendered)["new_head_sha"], NEW_HEAD)
        self.assertNotIn("prompt", rendered)

    def test_push_uses_lease_and_does_not_put_token_in_argv(self):
        captured = {}
        def record(command, cwd, env=None, input_text=None):
            captured["command"], captured["env"] = command, env
            return mock.Mock(returncode=0, stdout="", stderr="")
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "topsecret"}, clear=True), \
             mock.patch.object(worker, "_run", side_effect=record):
            worker.push_repair(request(), ".", NEW_HEAD)
        self.assertIn("--force-with-lease=refs/heads/codex/issue-73-repair:" + HEAD, captured["command"])
        self.assertNotIn("topsecret", " ".join(captured["command"]))
        self.assertNotIn("OPENAI_API_KEY", captured["env"])
        self.assertNotIn("AUTOMATION_APP_TOKEN", captured["env"])

    def test_push_fails_closed_without_automation_app_token(self):
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "state-token"}, clear=True), \
             mock.patch.object(worker, "_run") as run:
            with self.assertRaisesRegex(worker.RepairError, "App push credential is unavailable"):
                worker.push_repair(request(), ".", NEW_HEAD)
        run.assert_not_called()

    def test_push_lease_accepts_expected_head_and_rejects_moved_remote(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            remote, source, mover = root / "remote.git", root / "source", root / "mover"
            branch = "codex/issue-73-repair"
            self._git(directory, "init", "--bare", "-q", str(remote))
            source.mkdir()
            self._git(str(source), "init", "-q")
            self._git(str(source), "config", "user.name", "A5 test")
            self._git(str(source), "config", "user.email", "a5@example.invalid")
            self._git(str(source), "checkout", "-qb", branch)
            (source / "tracked.txt").write_text("initial\n", encoding="utf-8")
            self._git(str(source), "add", "tracked.txt")
            self._git(str(source), "commit", "-qm", "initial")
            self._git(str(source), "remote", "add", "origin", str(remote))
            self._git(str(source), "push", "-q", "origin", "HEAD:refs/heads/" + branch)
            expected_head = self._git(str(source), "rev-parse", "HEAD").stdout.strip()
            (source / "tracked.txt").write_text("repair\n", encoding="utf-8")
            self._git(str(source), "commit", "-am", "repair")
            new_head = self._git(str(source), "rev-parse", "HEAD").stdout.strip()

            with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "test-token"}):
                worker.push_repair(request(branch=branch, expected_head_sha=expected_head), str(source), new_head)
            self.assertEqual(self._git(directory, "--git-dir", str(remote), "rev-parse", "refs/heads/" + branch).stdout.strip(),
                             new_head)

            self._git(directory, "clone", "-q", "--branch", branch, str(remote), str(mover))
            self._git(str(mover), "config", "user.name", "A5 mover")
            self._git(str(mover), "config", "user.email", "mover@example.invalid")
            (mover / "moved.txt").write_text("moved\n", encoding="utf-8")
            self._git(str(mover), "add", "moved.txt")
            self._git(str(mover), "commit", "-qm", "move remote")
            self._git(str(mover), "push", "-q", "origin", "HEAD:refs/heads/" + branch)
            moved_head = self._git(str(mover), "rev-parse", "HEAD").stdout.strip()

            with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "test-token"}):
                with self.assertRaises(worker.RepairError):
                    worker.push_repair(request(branch=branch, expected_head_sha=new_head), str(source), new_head)
            self.assertEqual(self._git(directory, "--git-dir", str(remote), "rev-parse", "refs/heads/" + branch).stdout.strip(),
                             moved_head)

    def test_module_has_no_github_api_or_merge_route(self):
        source = inspect.getsource(worker).lower()
        for forbidden in ("urllib", "requests", "http.client", "graphql", "create pull"):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
