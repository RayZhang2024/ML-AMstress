import base64
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import codex_issue_worker as worker


ROOT = Path(__file__).resolve().parents[1]
TEST_CODEX_VERSION = "codex-cli 0.149.1"


GREEN_BODY = """## Goal
Bounded documentation work.

## Necessity Gate
Evidence and required files are recorded here.

## Required behavior
Do the requested bounded work.

## Do not change
Do not change production behavior.

## Acceptance criteria
- [ ] Criteria are testable.

## Tests/validation
Run normal Python checks.

## Risk classification
Declared risk label: `risk:green`

## Dependencies
- none
"""


def issue(labels=None, body=GREEN_BODY, state="open", updated_at="v1"):
    return {
        "number": 28,
        "title": "A4: Add GREEN-risk Codex issue worker",
        "body": body,
        "state": state,
        "updated_at": updated_at,
        "labels": [{"name": name} for name in (labels or [
            "status:ready", "risk:green", "agent:codex"
        ])],
    }


class FakeClient(object):
    repository = worker.REPOSITORY

    def __init__(self, current_issue, issue_sequence=None, dependencies=None, pulls=None):
        self.current_issue = current_issue
        self.issue_sequence = list(issue_sequence or [])
        self.dependencies = dependencies or {}
        self.pulls = list(pulls or [])
        self.events = []
        self.created_branch = None

    def issue(self, number):
        if self.issue_sequence:
            return self.issue_sequence.pop(0)
        return self.current_issue

    def dependency_issue(self, dependency):
        return {"state": self.dependencies.get(dependency, "closed")}

    def open_pulls(self):
        return list(self.pulls)

    def branch_exists(self, branch):
        return self.created_branch == branch

    def branch_sha(self, branch):
        self.events.append(("branch_sha", branch))
        return "base-sha"

    def create_branch(self, branch, sha):
        self.created_branch = branch
        self.events.append(("create_branch", branch, sha))

    def set_issue_labels(self, number, labels):
        self.events.append(("labels", tuple(labels)))
        self.current_issue["labels"] = [{"name": label} for label in labels]

    def comment(self, number, body):
        self.events.append(("comment", body))

    def create_pr(self, head, title, body):
        self.events.append(("create_pr", head, title, body))
        return {"number": 123, "html_url": "https://example.test/pr/123"}


class WorkerPolicyTests(unittest.TestCase):
    def test_dependency_parser_accepts_none_and_refs(self):
        self.assertEqual(worker.parse_dependencies("- none"), ())
        parsed = worker.parse_dependencies(
            "- blocked-by: #22\n- blocked-by: RayZhang2024/ML-AMstress#24"
        )
        self.assertEqual(parsed[0], worker.Dependency(worker.REPOSITORY, 22, "#22"))
        self.assertEqual(parsed[1].number, 24)

    def test_dependency_parser_rejects_malformed_and_duplicate_entries(self):
        for text in (
            "blocked-by: #22",
            "- blocked-by: 22",
            "- blocked-by: #22\n- blocked-by: #22",
            "- none\n- blocked-by: #22",
        ):
            with self.assertRaises(worker.WorkerError):
                worker.parse_dependencies(text)

    def test_local_python_preflight_rejects_missing_or_malformed_python(self):
        with mock.patch.object(worker.shutil, "which", return_value=None):
            with self.assertRaises(worker.WorkerError) as raised:
                worker.verify_local_python()
        self.assertIn("not available on PATH", str(raised.exception))

        with mock.patch.object(worker.shutil, "which", return_value="python"), mock.patch.object(
            worker, "_probe", return_value=(0, "not a Python version")
        ):
            with self.assertRaises(worker.WorkerError) as raised:
                worker.verify_local_python()
        self.assertIn("malformed version", str(raised.exception))

    def test_local_python_preflight_rejects_unsupported_and_accepts_supported_versions(self):
        for output in ("Python 3.10.14", "Python 4.0.0"):
            with mock.patch.object(worker.shutil, "which", return_value="python"), mock.patch.object(
                worker, "_probe", return_value=(0, output)
            ):
                with self.assertRaises(worker.WorkerError) as raised:
                    worker.verify_local_python()
            self.assertIn("Python 3.11 or newer", str(raised.exception))

        with mock.patch.object(worker.shutil, "which", return_value="python"), mock.patch.object(
            worker, "_probe", return_value=(0, "Python 3.13.14")
        ):
            executable, version = worker.verify_local_python()
        self.assertEqual(executable, "python")
        self.assertEqual(version, (3, 13))

    def test_workspace_status_diagnostic_accepts_clean_and_reports_paths(self):
        self.assertEqual(worker.format_workspace_status("\n"), "")
        self.assertEqual(
            worker.format_workspace_status(" M docs/DEVELOPMENT.md\n"),
            " M docs/DEVELOPMENT.md",
        )
        self.assertEqual(
            worker.format_workspace_status("?? scratch/output.txt\n"),
            "?? scratch/output.txt",
        )

    def test_workspace_status_diagnostic_is_bounded_and_sanitized(self):
        output = "\n".join(" M tests/change-%d.py" % index for index in range(4))
        diagnostic = worker.format_workspace_status(output, limit=2)
        self.assertIn(" M tests/change-0.py", diagnostic)
        self.assertIn(" M tests/change-1.py", diagnostic)
        self.assertNotIn("tests/change-2.py", diagnostic)
        self.assertIn("2 additional entries omitted", diagnostic)

        with mock.patch.dict(os.environ, {"UNRELATED_SECRET": "do-not-leak"}):
            diagnostic = worker.format_workspace_status(
                " M C:\\Users\\runner\\do-not-leak.txt\n",
            )
        self.assertEqual(diagnostic, " M <invalid-repository-path>")
        self.assertNotIn("do-not-leak", diagnostic)

    def test_codex_prompt_separates_trusted_worker_and_local_duties(self):
        prompt = " ".join(worker._codex_prompt(issue(), "codex/issue-53-test").split())
        self.assertIn("trusted worker has already validated control-plane eligibility", prompt)
        self.assertIn("Do not query GitHub or require GitHub API credentials", prompt)
        self.assertIn("labels, status, risk, dependencies, open PRs, branch claims, races", prompt)
        self.assertIn("branch creation and claim, issue labels/status", prompt)
        self.assertIn("commit/push, PR creation, and merge/no-merge policy", prompt)
        self.assertIn("Do not perform, require, or revalidate", prompt)
        self.assertIn("context, not prerequisite tasks for you", prompt)
        self.assertIn("local repository Necessity Gate", prompt)
        self.assertIn("already satisfied", prompt)
        self.assertIn("minimal files needed", prompt)
        self.assertIn("Do-not-change constraint", prompt)
        self.assertIn("scientific intent is ambiguous", prompt)

    def test_codex_prompt_allows_missing_optional_tools_with_truthful_reporting(self):
        prompt = " ".join(worker._codex_prompt(issue(), "codex/issue-53-test").split())
        self.assertIn("focused local checks when tooling is available", prompt)
        self.assertIn("inability to invoke Python or other optional tooling", prompt)
        self.assertIn("not by itself a reason to decline", prompt)
        self.assertIn("authoritative normal-Python validation", prompt)
        self.assertIn("Report exactly which checks you ran and which you could not run", prompt)
        self.assertIn("Exact issue contract", prompt)

    def test_codex_prompt_treats_worker_owned_contract_steps_as_context(self):
        body = GREEN_BODY + "\n## Worker operations\nCreate a branch and update issue labels.\n"
        prompt = worker._codex_prompt(issue(body=body), "codex/issue-53-test")
        instructions, contract = prompt.split("Exact issue contract:", 1)
        instructions = " ".join(instructions.split())
        self.assertIn("Do not query GitHub", instructions)
        self.assertIn("context, not prerequisite tasks for you", instructions)
        self.assertIn("Create a branch and update issue labels.", contract)

    def test_codex_noop_diagnostic_redacts_secrets_tokens_paths_and_controls(self):
        with mock.patch.dict(
            os.environ,
            {
                "GITHUB_TOKEN": "github-secret-value",
                "GH_TOKEN": "gh-secret-value",
                "OPENAI_API_KEY": "openai-secret-value",
            },
            clear=False,
        ):
            diagnostic = worker.format_codex_noop_diagnostic(
                "earlier detail\n"
                "token=github-secret-value\x00\tBearer bearer-secret-value\n"
                "sk-abcdefghijklmnop C:\\Users\\alice\\private.txt\n"
                "Basic YWxhZGRpbjpvcGVuc2VzYW1l /home/alice/private.txt "
                "ghp_abcdefghijklmnop session=chatgpt-session-value\n"
                '{"id_token": "eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue"}\n'
                "refresh_token=refresh-token-secret-value\n"
                "standalone eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue\n"
                "safe tail one\n"
                "safe tail two\n"
                "final    safe    line\n"
                "Exact issue contract:\nshould never be logged\n",
                "",
            )
        self.assertNotIn("github-secret-value", diagnostic)
        self.assertNotIn("bearer-secret-value", diagnostic)
        self.assertNotIn("sk-abcdefghijklmnop", diagnostic)
        self.assertNotIn("ghp_abcdefghijklmnop", diagnostic)
        self.assertNotIn("C:\\Users\\alice", diagnostic)
        self.assertNotIn("/home/alice", diagnostic)
        self.assertNotIn("chatgpt-session-value", diagnostic)
        self.assertNotIn("eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue", diagnostic)
        self.assertNotIn("refresh-token-secret-value", diagnostic)
        self.assertNotIn("should never be logged", diagnostic)
        self.assertNotIn("\x00", diagnostic)
        self.assertIn("final safe line", diagnostic)
        self.assertNotIn("earlier detail", diagnostic)

    def test_codex_noop_diagnostic_redacts_oauth_assignments_and_jwt_tail(self):
        id_token = "id-token-secret-value"
        refresh_token = "refresh-token-secret-value"
        jwt = "eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue"
        diagnostic = worker.format_codex_noop_diagnostic(
            (
                "safe beginning\n"
                + 'id_token="%s"\n' % id_token
                + '{"refresh_token": "%s"}\n' % refresh_token
                + "standalone %s\n" % jwt
                + "safe final"
            ),
            "",
        )
        self.assertNotIn(id_token, diagnostic)
        self.assertNotIn(refresh_token, diagnostic)
        self.assertNotIn(jwt, diagnostic)
        self.assertIn("safe final", diagnostic)
        self.assertLessEqual(len(diagnostic), worker.MAX_CODEX_NOOP_DIAGNOSTIC_CHARS)

    def test_codex_noop_diagnostic_prefers_safe_stdout_final_response(self):
        diagnostic = worker.format_codex_noop_diagnostic(
            "final explanation from Codex",
            "token=stderr-secret-value\nsafe tail two\nfinal safe line\n"
            "command output that must not be shown",
        )
        self.assertIn("final response", diagnostic)
        self.assertIn("final explanation from Codex", diagnostic)
        self.assertNotIn("safe tail two", diagnostic)
        self.assertNotIn("final safe line", diagnostic)
        self.assertNotIn("command output", diagnostic)
        self.assertNotIn("stderr-secret-value", diagnostic)

    def test_codex_noop_diagnostic_uses_sanitized_stderr_only_as_fallback(self):
        jwt = "eyJhbGciOiJub25lIn0.eyJzdWIiOiJ0ZXN0In0.signaturevalue"
        diagnostic = worker.format_codex_noop_diagnostic(
            "Bearer stdout-secret-value",
            "stderr explanation\nrefresh_token=refresh-token-secret-value\n%s" % jwt,
        )
        self.assertIn("stderr fallback", diagnostic)
        self.assertIn("stderr explanation", diagnostic)
        self.assertNotIn("stdout-secret-value", diagnostic)
        self.assertNotIn("refresh-token-secret-value", diagnostic)
        self.assertNotIn(jwt, diagnostic)

        empty_stdout = worker.format_codex_noop_diagnostic("", "safe stderr fallback")
        self.assertIn("stderr fallback", empty_stdout)
        self.assertIn("safe stderr fallback", empty_stdout)

    def test_codex_noop_diagnostic_is_bounded_and_falls_back_when_fully_redacted(self):
        diagnostic = worker.format_codex_noop_diagnostic(
            "\n".join("line %d %s" % (index, "x" * 300) for index in range(6)),
            "",
        )
        self.assertLessEqual(len(diagnostic), worker.MAX_CODEX_NOOP_DIAGNOSTIC_CHARS)
        self.assertNotIn("line 0", diagnostic)
        self.assertIn("line 5", diagnostic)

        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "only-secret"}, clear=False):
            fallback = worker.format_codex_noop_diagnostic("only-secret", "")
        self.assertEqual(fallback, worker.CODEX_NOOP_FALLBACK)

        fallback = worker.format_codex_noop_diagnostic("Bearer bearer-secret", "")
        self.assertEqual(fallback, worker.CODEX_NOOP_FALLBACK)

    def test_preflight_reports_sanitized_dirty_workspace(self):
        directory = self._preflight_workspace()
        try:
            def fake_run(command, **kwargs):
                if command[0] == "codex" and command[1] == "--version":
                    output = TEST_CODEX_VERSION
                elif command[0] == "codex":
                    output = "Logged in using ChatGPT"
                elif command[1:] == ["--version"]:
                    output = "git version 2"
                elif command[1:3] == ["rev-parse", "--is-inside-work-tree"]:
                    output = "true"
                elif command[1:] == ["status", "--porcelain"]:
                    output = " M docs/DEVELOPMENT.md"
                else:
                    output = ""
                return type("Result", (), {"returncode": 0, "stdout": output, "stderr": ""})()

            with mock.patch.dict(os.environ, self._preflight_env(), clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), mock.patch.object(
                worker.shutil, "which", side_effect=lambda name: name
            ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run), self.assertRaises(
                worker.WorkerError
            ) as raised:
                worker.run_preflight(directory.name)
            self.assertIn("worker workspace is not clean:  M docs/DEVELOPMENT.md", str(raised.exception))
        finally:
            directory.cleanup()

    def test_local_line_ending_policy_keeps_isolated_status_clean(self):
        with tempfile.TemporaryDirectory() as directory:
            workspace = Path(directory)
            environment = worker._probe_environment()

            def git(*arguments):
                result = subprocess.run(
                    ["git"] + list(arguments),
                    cwd=str(workspace),
                    env=environment,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    universal_newlines=True,
                    check=False,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                return result.stdout

            git("init")
            git("config", "user.email", "worker-test@example.invalid")
            git("config", "user.name", "Worker Test")
            (workspace / "tracked.txt").write_bytes(b"line one\nline two\n")
            git("add", "tracked.txt")
            git("commit", "-m", "initial")

            # Reproduce a Windows checkout already materialized under inherited
            # core.autocrlf=true, then switch to the isolated local policy.
            git("config", "--local", "core.autocrlf", "true")
            (workspace / "tracked.txt").write_bytes(b"line one\r\nline two\r\n")
            git("config", "--local", "core.autocrlf", "false")
            git("config", "--local", "core.eol", "lf")

            status_code, stale_status = worker._probe(
                ["git", "status", "--porcelain"], str(workspace)
            )
            self.assertEqual(status_code, 0)
            self.assertTrue(stale_status.strip())

            # With checkout itself using the deterministic policy, the file is
            # materialized as LF before the isolated status probe.
            (workspace / "tracked.txt").write_bytes(b"line one\nline two\n")
            status_code, normalized_status = worker._probe(
                ["git", "status", "--porcelain"], str(workspace)
            )
            self.assertEqual(status_code, 0)
            self.assertEqual(normalized_status.strip(), "")

            (workspace / "tracked.txt").write_bytes(b"real modification\n")
            status_code, dirty_status = worker._probe(
                ["git", "status", "--porcelain"], str(workspace)
            )
            self.assertEqual(status_code, 0)
            self.assertIn(" M tracked.txt", dirty_status)

    def test_eligible_green_issue_with_satisfied_dependencies(self):
        current = issue(body=GREEN_BODY.replace("- none", "- blocked-by: #22"))
        contract = worker.parse_contract(current["body"])
        states = {contract.dependencies[0]: "closed"}
        result = worker.evaluate_eligibility(current, states)
        self.assertTrue(result.eligible)

    def test_missing_or_multiple_status_and_non_green_risk_reject(self):
        self.assertFalse(
            worker.evaluate_eligibility(issue(labels=["risk:green", "agent:codex"]), {}).eligible
        )
        self.assertFalse(
            worker.evaluate_eligibility(
                issue(labels=["status:ready", "status:review", "risk:green", "agent:codex"]), {}
            ).eligible
        )
        for risk in ("risk:yellow", "risk:red"):
            self.assertFalse(
                worker.evaluate_eligibility(
                    issue(labels=["status:ready", risk, "agent:codex"]), {}
                ).eligible
            )

    def test_malformed_contract_and_open_dependency_reject(self):
        malformed = issue(body=GREEN_BODY.replace("## Dependencies", "## Dependencies "))
        result = worker.evaluate_eligibility(malformed, {})
        self.assertFalse(result.eligible)
        self.assertTrue(any("missing required section" in reason for reason in result.reasons))

        dependent = issue(body=GREEN_BODY.replace("- none", "- blocked-by: #22"))
        contract = worker.parse_contract(dependent["body"])
        result = worker.evaluate_eligibility(dependent, {contract.dependencies[0]: "open"})
        self.assertFalse(result.eligible)

    def test_duplicate_branch_or_open_pr_reject(self):
        current = issue()
        branch = worker.deterministic_branch_name(28, current["title"])
        self.assertFalse(worker.evaluate_eligibility(current, {}, branch_exists=True).eligible)
        self.assertFalse(
            worker.evaluate_eligibility(
                current, {}, open_prs=[{"state": "open", "headRefName": branch}]
            ).eligible
        )

    def test_green_allowlist_rejects_production_paths(self):
        allowed, rejected = worker.green_changed_paths(
            [
                "docs/change.md",
                "scripts/other_tool.py",
                "scripts/codex_issue_worker.py",
                "scripts/a5_reviewer.py",
                "scripts/a5_review_state.py",
                "scripts/a5_repair_worker.py",
                "scripts/a5_review_orchestrator.py",
                ".github/workflows/candidate.yml",
                "AGENTS.md",
                "docs/AUTONOMOUS_DEVELOPMENT.md",
                "docs/AUTONOMOUS_ORCHESTRATION.md",
                "create_input.py",
            ]
        )
        self.assertEqual(allowed, ("docs/change.md", "scripts/other_tool.py"))
        self.assertEqual(
            rejected,
            (
                "scripts/codex_issue_worker.py",
                "scripts/a5_reviewer.py",
                "scripts/a5_review_state.py",
                "scripts/a5_repair_worker.py",
                "scripts/a5_review_orchestrator.py",
                ".github/workflows/candidate.yml",
                "AGENTS.md",
                "docs/AUTONOMOUS_DEVELOPMENT.md",
                "docs/AUTONOMOUS_ORCHESTRATION.md",
                "create_input.py",
            ),
        )

    def test_codex_process_has_no_github_write_credential_or_git_helper(self):
        captured = {}

        def fake_run(*args, **kwargs):
            captured["command"] = args[0]
            captured["env"] = kwargs["env"]
            return type("Result", (), {"returncode": 0, "stdout": "", "stderr": ""})()

        with mock.patch.dict(
            os.environ,
            {
                "OPENAI_API_KEY": "openai-secret",
                "GITHUB_TOKEN": "github-write-token",
                "GH_TOKEN": "gh-write-token",
                "AUTOMATION_APP_TOKEN": "app-write-token",
                "GIT_CONFIG_COUNT": "2",
                "GIT_CONFIG_KEY_0": "core.autocrlf",
                "GIT_CONFIG_VALUE_0": "false",
                "GIT_CONFIG_KEY_1": "core.eol",
                "GIT_CONFIG_VALUE_1": "lf",
            },
        ), mock.patch.object(
            worker.shutil, "which", return_value=r"C:\Tools\codex.CMD"
        ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
            worker.run_codex(issue(), "codex/issue-28-test", tempfile.gettempdir())

        self.assertNotIn("GITHUB_TOKEN", captured["env"])
        self.assertNotIn("GH_TOKEN", captured["env"])
        self.assertNotIn("OPENAI_API_KEY", captured["env"])
        self.assertNotIn("AUTOMATION_APP_TOKEN", captured["env"])
        self.assertEqual(captured["env"]["GIT_CONFIG_NOSYSTEM"], "1")
        self.assertEqual(captured["env"]["GIT_CONFIG_GLOBAL"], os.devnull)
        self.assertNotIn("GIT_CONFIG_NOGLOBAL", captured["env"])
        self.assertEqual(captured["env"]["GIT_TERMINAL_PROMPT"], "0")
        self.assertNotIn("GIT_CONFIG_COUNT", captured["env"])
        self.assertNotIn("GIT_CONFIG_KEY_0", captured["env"])
        self.assertNotIn("GIT_CONFIG_VALUE_0", captured["env"])
        self.assertNotIn("GIT_CONFIG_KEY_1", captured["env"])
        self.assertNotIn("GIT_CONFIG_VALUE_1", captured["env"])
        self.assertNotIn("github-write-token", captured["command"][-1])

    def test_codex_process_cannot_inherit_runner_global_credential_helper(self):
        captured = {}

        def fake_run(*args, **kwargs):
            captured["env"] = kwargs["env"]
            return type("Result", (), {"returncode": 0, "stdout": "", "stderr": ""})()

        with tempfile.TemporaryDirectory() as directory:
            global_config = Path(directory) / "runner.gitconfig"
            global_config.write_text("[credential]\n\thelper = runner-helper\n")
            with mock.patch.dict(
                os.environ,
                {
                "GITHUB_TOKEN": "github-write-token",
                "GIT_CONFIG_GLOBAL": str(global_config),
                "AUTOMATION_APP_TOKEN": "app-write-token",
                    "GIT_CONFIG_NOGLOBAL": "1",
                },
            ), mock.patch.object(
                worker.shutil, "which", return_value=r"C:\Tools\codex.CMD"
            ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
                worker.run_codex(issue(), "codex/issue-28-test", directory)

            self.assertEqual(captured["env"]["GIT_CONFIG_GLOBAL"], os.devnull)
            self.assertNotIn("GIT_CONFIG_NOGLOBAL", captured["env"])
            result = subprocess.run(
                ["git", "config", "--global", "--get", "credential.helper"],
                env=captured["env"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True,
                check=False,
            )
            self.assertNotEqual(result.returncode, 0)

    def test_run_codex_uses_preflight_resolved_windows_shim(self):
        directory = self._preflight_workspace()
        commands = []
        resolved_codex = r"C:\Users\runner\AppData\Local\Programs\codex.CMD"
        try:
            def fake_run(command, **kwargs):
                commands.append((command, kwargs))
                if command[0] == resolved_codex and command[1] == "--version":
                    output = TEST_CODEX_VERSION
                elif command[0] == resolved_codex and command[1:] == ["login", "status"]:
                    output = "Logged in using ChatGPT"
                elif command[1:] == ["--version"]:
                    output = "git version 2"
                elif command[1:3] == ["rev-parse", "--is-inside-work-tree"]:
                    output = "true"
                else:
                    output = ""
                return type("Result", (), {"returncode": 0, "stdout": output, "stderr": ""})()

            with mock.patch.dict(os.environ, self._preflight_env(), clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), mock.patch.object(
                worker.shutil,
                "which",
                side_effect=lambda name: resolved_codex if name == "codex" else "git",
            ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
                worker.run_preflight(directory.name)
                worker.run_codex(issue(), "codex/issue-28-test", directory.name)

            command_vectors = [item[0] for item in commands]
            self.assertIn([resolved_codex, "--version"], command_vectors)
            self.assertIn([resolved_codex, "login", "status"], command_vectors)
            actual = next(command for command in command_vectors if command[1] == "exec")
            self.assertEqual(actual[0], resolved_codex)
            self.assertNotEqual(actual[0], "codex")
            self.assertIn("--sandbox", actual)
            self.assertEqual(actual[actual.index("--sandbox") + 1], "workspace-write")
            self.assertIn("-c", actual)
            self.assertIn('approval_policy="never"', actual)
            self.assertNotIn("--full-auto", actual)
            self.assertNotIn("--approve-for-me", actual)
            self.assertNotIn("--dangerously-bypass-approvals-and-sandbox", actual)
            self.assertNotIn("danger-full-access", actual)
            self.assertEqual(actual[-1], "-")
            self.assertNotIn("Exact issue contract", actual)
            actual_kwargs = next(kwargs for command, kwargs in commands if command == actual)
            self.assertNotIn("shell", actual_kwargs)
            self.assertEqual(
                actual_kwargs["input"], worker._codex_prompt(issue(), "codex/issue-28-test")
            )
            self.assertTrue(actual_kwargs["universal_newlines"])
        finally:
            directory.cleanup()

    def test_run_codex_delivers_long_issue_contract_intact_via_stdin(self):
        marker = "long-contract-marker"
        body = GREEN_BODY + "\n" + marker + ("x" * 17000)
        long_issue = issue(body=body)
        captured = {}

        def fake_run(command, **kwargs):
            captured["command"] = command
            captured["kwargs"] = kwargs
            return type("Result", (), {"returncode": 0, "stdout": "", "stderr": ""})()

        with mock.patch.object(
            worker.shutil, "which", return_value=r"C:\Tools\codex.CMD"
        ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
            worker.run_codex(long_issue, "codex/issue-61-test", tempfile.gettempdir())

        prompt = worker._codex_prompt(long_issue, "codex/issue-61-test")
        self.assertGreater(len(prompt), 16 * 1024)
        self.assertEqual(captured["command"][-1], "-")
        self.assertNotIn(marker, " ".join(captured["command"]))
        self.assertNotIn(body, captured["command"])
        self.assertEqual(captured["kwargs"]["input"], prompt)

    def test_run_codex_supports_explicit_executable_file(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / "codex.cmd"
            executable.write_text("shim", encoding="utf-8")
            captured = {}

            def fake_run(command, **kwargs):
                captured["command"] = command
                captured["kwargs"] = kwargs
                return type("Result", (), {"returncode": 0, "stdout": "", "stderr": ""})()

            with mock.patch.dict(
                os.environ, {"CODEX_EXECUTABLE": str(executable)}, clear=False
            ), mock.patch.object(worker.shutil, "which", return_value=None), mock.patch.object(
                worker.subprocess, "run", side_effect=fake_run
            ):
                worker.run_codex(issue(), "codex/issue-28-test", directory)

            self.assertEqual(captured["command"][0], os.path.abspath(str(executable)))
            self.assertNotIn("shell", captured["kwargs"])

    def test_run_codex_rejects_missing_executable_without_launching(self):
        with mock.patch.dict(
            os.environ, {"CODEX_EXECUTABLE": "missing-codex"}, clear=False
        ), mock.patch.object(worker.shutil, "which", return_value=None), mock.patch.object(
            worker.os.path, "isfile", return_value=False
        ), mock.patch.object(worker.subprocess, "run") as run:
            with self.assertRaises(worker.WorkerError) as raised:
                worker.run_codex(issue(), "codex/issue-28-test", tempfile.gettempdir())
        self.assertIn("Codex executable is not available", str(raised.exception))
        run.assert_not_called()

    def test_run_codex_nonzero_remains_fail_closed_without_output(self):
        result = type(
            "Result",
            (),
            {"returncode": 2, "stdout": "raw no-op-secret", "stderr": "raw stderr"},
        )()
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "no-op-secret"}, clear=False), mock.patch.object(
            worker.shutil, "which", return_value=r"C:\Tools\codex.CMD"
        ), mock.patch.object(worker.subprocess, "run", return_value=result):
            with self.assertRaises(worker.WorkerError) as raised:
                worker.run_codex(issue(), "codex/issue-28-test", tempfile.gettempdir())
        self.assertEqual(str(raised.exception), "Codex exited with status 2")
        self.assertNotIn("no-op-secret", str(raised.exception))

    def test_trusted_push_uses_origin_scoped_basic_auth_only_for_git_command(self):
        captured = {}

        def fake_run(command, cwd=None, env=None, **kwargs):
            captured["command"] = command
            captured["env"] = env
            return ""

        with mock.patch.dict(
            os.environ,
            {
                "GITHUB_TOKEN": "github-write-token",
                "GH_TOKEN": "gh-write-token",
                "OPENAI_API_KEY": "openai-secret",
                "AUTOMATION_APP_TOKEN": "app-write-token",
            },
        ), mock.patch.object(worker, "_run", side_effect=fake_run):
            worker.push_branch(tempfile.gettempdir(), "codex/issue-28-test")

        self.assertNotIn("GITHUB_TOKEN", captured["env"])
        self.assertNotIn("GH_TOKEN", captured["env"])
        self.assertNotIn("OPENAI_API_KEY", captured["env"])
        self.assertNotIn("AUTOMATION_APP_TOKEN", captured["env"])
        self.assertNotIn("github-write-token", captured["env"].values())
        self.assertNotIn("app-write-token", captured["env"].values())
        self.assertEqual(
            captured["env"]["GIT_CONFIG_KEY_0"],
            "http.https://github.com/.extraheader",
        )
        header = captured["env"]["GIT_CONFIG_VALUE_0"]
        self.assertTrue(header.startswith("AUTHORIZATION: basic "))
        encoded_credentials = header[len("AUTHORIZATION: basic "):]
        self.assertEqual(
            base64.b64decode(encoded_credentials).decode("utf-8"),
            "x-access-token:app-write-token",
        )
        self.assertNotIn("github-write-token", captured["command"])
        self.assertNotIn(encoded_credentials, captured["command"])
        self.assertEqual(
            captured["command"], ["git", "push", "origin", "codex/issue-28-test"]
        )

    def test_trusted_push_fails_closed_without_automation_app_token(self):
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "state-token"}, clear=True), \
             mock.patch.object(worker, "_run") as run:
            with self.assertRaisesRegex(worker.WorkerError, "AUTOMATION_APP_TOKEN"):
                worker.push_branch(tempfile.gettempdir(), "codex/issue-28-test")
        run.assert_not_called()

    def test_main_fails_closed_when_app_token_mint_output_is_missing(self):
        with tempfile.TemporaryDirectory() as directory:
            event_path = Path(directory) / "event.json"
            event_path.write_text(json.dumps({
                "action": "labeled",
                "label": {"name": "agent:codex"},
                "issue": {"number": 28},
                "repository": {"full_name": worker.REPOSITORY},
            }), encoding="utf-8")
            with mock.patch.dict(os.environ, {
                "GITHUB_EVENT_PATH": str(event_path),
                "GITHUB_TOKEN": "state-token",
            }, clear=True), mock.patch.object(worker.Worker, "execute") as execute:
                with self.assertRaisesRegex(worker.WorkerError, "AUTOMATION_APP_TOKEN"):
                    worker.main([])
        execute.assert_not_called()

    def test_claimed_branch_checkout_tracks_new_remote_when_no_local_branch_exists(self):
        branch = worker.deterministic_branch_name(28, issue()["title"])
        calls = []

        def fake_run(command, **kwargs):
            calls.append((command, kwargs))
            return ""

        with mock.patch.object(worker, "_run", side_effect=fake_run):
            worker.checkout_claimed_worker_branch(branch, "worker-cwd")

        self.assertEqual(calls[0][0], ["git", "fetch", "origin", branch])
        self.assertEqual(calls[1][0], ["git", "branch", "--list", branch])
        self.assertTrue(calls[1][1]["capture"])
        self.assertEqual(
            calls[2][0], ["git", "switch", "-c", branch, "--track", "origin/" + branch]
        )

    def test_claimed_branch_checkout_recreates_only_stale_local_worker_branch(self):
        branch = worker.deterministic_branch_name(28, issue()["title"])
        unrelated = "maintainer/keep-this-branch"
        calls = []

        def fake_run(command, **kwargs):
            calls.append((command, kwargs))
            if command[:3] == ["git", "branch", "--list"]:
                return branch + "\n"
            return ""

        with mock.patch.object(worker, "_run", side_effect=fake_run):
            worker.checkout_claimed_worker_branch(branch, "worker-cwd")

        commands = [command for command, _ in calls]
        self.assertIn(
            ["git", "switch", "-C", branch, "--track", "origin/" + branch], commands
        )
        self.assertFalse(any(unrelated in command for command in commands))
        self.assertFalse(any(command[:2] == ["git", "reset"] for command in commands))
        self.assertFalse(any(command[:3] == ["git", "branch", "-D"] for command in commands))

    def test_preexisting_remote_claim_blocks_before_local_checkout_or_codex(self):
        client = FakeClient(issue())
        branch = worker.deterministic_branch_name(28, issue()["title"])
        client.created_branch = branch
        with mock.patch.object(worker, "checkout_claimed_worker_branch") as checkout:
            with self.assertRaises(worker.WorkerError):
                worker.Worker(client, 28, "run-duplicate", codex_runner=lambda *args: None).execute()
        checkout.assert_not_called()
        self.assertFalse(any(event[0] == "create_branch" for event in client.events))

    def test_state_change_before_claim_fails_without_branch(self):
        client = FakeClient(issue(), issue_sequence=[issue(updated_at="v1"), issue(updated_at="v2")])
        with self.assertRaises(worker.WorkerError):
            worker.Worker(client, 28, "run-race", codex_runner=lambda *args: None).execute()
        self.assertFalse(any(event[0] == "create_branch" for event in client.events))
        self.assertIn("status:blocked", [item["name"] for item in client.current_issue["labels"]])

    def test_codex_failure_is_blocked_without_pr(self):
        client = FakeClient(issue())

        def fail(*args):
            raise worker.WorkerError("simulated Codex failure")

        with mock.patch.object(worker, "_run", return_value=""), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/change.md",)
        ), mock.patch.object(worker, "_git_paths", return_value=("docs/change.md",)):
            with self.assertRaises(worker.WorkerError):
                worker.Worker(
                    client,
                    28,
                    "run-fail",
                    codex_runner=fail,
                    validation_runner=lambda cwd: None,
                    push_runner=lambda cwd, branch: None,
                ).execute()
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))
        self.assertIn("status:blocked", [item["name"] for item in client.current_issue["labels"]])
        self.assertEqual(
            client.created_branch,
            worker.deterministic_branch_name(28, client.current_issue["title"]),
        )

    def test_successful_codex_noop_reports_only_sanitized_diagnostic(self):
        client = FakeClient(issue())
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "no-op-secret"}, clear=False), mock.patch.object(
            worker, "checkout_claimed_worker_branch"
        ), mock.patch.object(worker, "_all_changed_paths", return_value=()):
            with self.assertRaises(worker.WorkerError) as raised:
                worker.Worker(
                    client,
                    28,
                    "run-noop",
                    codex_runner=lambda *args: (
                        "Codex summary\nBearer no-op-secret\ncompleted without edits", ""
                    ),
                    validation_runner=lambda cwd: None,
                    push_runner=lambda cwd, branch: None,
                ).execute()
        self.assertIn("completed without edits", str(raised.exception))
        self.assertNotIn("no-op-secret", str(raised.exception))
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))

    def test_success_records_claim_then_review_without_merge(self):
        client = FakeClient(issue())
        with mock.patch.object(worker, "_run", return_value=""), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/change.md",)
        ), mock.patch.object(worker, "_git_paths", return_value=("docs/change.md",)):
            result = worker.Worker(
                client,
                28,
                "run-success",
                codex_runner=lambda *args: None,
                validation_runner=lambda cwd: None,
                push_runner=lambda cwd, branch: None,
            ).execute()
        self.assertEqual(result["number"], 123)
        labels_events = [event for event in client.events if event[0] == "labels"]
        self.assertIn("status:in-progress", labels_events[0][1])
        self.assertIn("status:review", labels_events[-1][1])
        self.assertTrue(any(event[0] == "create_pr" for event in client.events))
        self.assertTrue(
            any(
                "codex-worker-claim issue:28 run:run-success" in event[1]
                for event in client.events
                if event[0] == "comment"
            )
        )
        self.assertFalse(any("merge" in event[0] for event in client.events))

    def test_event_writes_use_app_client_while_state_writes_use_builtin_client(self):
        state_client = FakeClient(issue())
        app_client = FakeClient(issue())
        with mock.patch.object(worker, "_run", return_value=""), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/change.md",)
        ), mock.patch.object(worker, "_git_paths", return_value=("docs/change.md",)):
            worker.Worker(
                state_client,
                28,
                "run-app-client",
                codex_runner=lambda *args: None,
                validation_runner=lambda cwd: None,
                push_runner=lambda cwd, branch: None,
                event_client=app_client,
            ).execute()
        self.assertTrue(any(event[0] == "create_branch" for event in app_client.events))
        self.assertTrue(any(event[0] == "create_pr" for event in app_client.events))
        self.assertFalse(any(event[0] in ("create_branch", "create_pr") for event in state_client.events))
        self.assertTrue(any(event[0] == "labels" for event in state_client.events))

    def _preflight_workspace(self):
        directory = tempfile.TemporaryDirectory()
        root = Path(directory.name)
        (root / ".git").mkdir()
        (root / "scripts").mkdir()
        (root / "AGENTS.md").write_text("policy")
        (root / "scripts" / "codex_issue_worker.py").write_text("worker")
        return directory

    def _preflight_env(self):
        return {
            "RUNNER_OS": "Windows",
            "RUNNER_ARCH": "X64",
            "RUNNER_NAME": "ml-amstress-runner",
            "CODEX_EXPECTED_RUNNER_NAME": "ml-amstress-runner",
            "CODEX_EXPECTED_WINDOWS_USER": "runner-user",
            "CODEX_EXPECTED_VERSION": TEST_CODEX_VERSION,
            "CODEX_EXECUTABLE": "codex",
        }

    def test_worker_test_contract_pins_codex_cli_01491(self):
        self.assertEqual(TEST_CODEX_VERSION, "codex-cli 0.149.1")
        self.assertEqual(self._preflight_env()["CODEX_EXPECTED_VERSION"], TEST_CODEX_VERSION)

    def test_preflight_rejects_missing_codex_executable(self):
        directory = self._preflight_workspace()
        try:
            with mock.patch.dict(os.environ, self._preflight_env(), clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), mock.patch.object(
                worker.shutil, "which", side_effect=lambda name: "git" if name == "git" else None
            ), self.assertRaises(worker.WorkerError) as raised:
                worker.run_preflight(directory.name)
            self.assertIn("Codex executable is not available", str(raised.exception))
        finally:
            directory.cleanup()

    def test_preflight_rejects_unusable_chatgpt_authentication(self):
        directory = self._preflight_workspace()
        try:
            def fake_run(command, **kwargs):
                if command[0] == "codex" and command[1] == "--version":
                    return type("Result", (), {"returncode": 0, "stdout": TEST_CODEX_VERSION, "stderr": ""})()
                if command[0] == "codex":
                    return type("Result", (), {"returncode": 1, "stdout": "Not logged in", "stderr": ""})()
                if command[1:] == ["--version"]:
                    return type("Result", (), {"returncode": 0, "stdout": "git version 2", "stderr": ""})()
                if command[1:3] == ["rev-parse", "--is-inside-work-tree"]:
                    return type("Result", (), {"returncode": 0, "stdout": "true", "stderr": ""})()
                return type("Result", (), {"returncode": 0, "stdout": "", "stderr": ""})()

            with mock.patch.dict(os.environ, self._preflight_env(), clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), mock.patch.object(
                worker.shutil, "which", side_effect=lambda name: name
            ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run), self.assertRaises(
                worker.WorkerError
            ) as raised:
                worker.run_preflight(directory.name)
            self.assertIn("ChatGPT authentication is unavailable", str(raised.exception))
        finally:
            directory.cleanup()

    def test_preflight_accepts_authenticated_windows_runner(self):
        directory = self._preflight_workspace()
        commands = []
        try:
            def fake_run(command, **kwargs):
                commands.append(command)
                if command[0] == "codex" and command[1] == "--version":
                    output = TEST_CODEX_VERSION
                elif command[0] == "codex":
                    output = "Logged in using ChatGPT"
                elif command[1:] == ["--version"]:
                    output = "git version 2"
                elif command[1:3] == ["rev-parse", "--is-inside-work-tree"]:
                    output = "true"
                else:
                    output = ""
                return type("Result", (), {"returncode": 0, "stdout": output, "stderr": ""})()

            with mock.patch.dict(os.environ, self._preflight_env(), clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), mock.patch.object(
                worker.shutil, "which", side_effect=lambda name: name
            ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
                worker.run_preflight(directory.name)

            self.assertIn(["codex", "--version"], commands)
            self.assertIn(["codex", "login", "status"], commands)
            self.assertIn(["git", "status", "--porcelain"], commands)
        finally:
            directory.cleanup()

    def test_preflight_rejects_api_key_fallback(self):
        directory = self._preflight_workspace()
        try:
            values = self._preflight_env()
            values["OPENAI_API_KEY"] = "must-not-be-used"
            with mock.patch.dict(os.environ, values, clear=False), mock.patch.object(
                worker.getpass, "getuser", return_value="runner-user"
            ), self.assertRaises(worker.WorkerError) as raised:
                worker.run_preflight(directory.name)
            self.assertIn("OPENAI_API_KEY is unsupported", str(raised.exception))
        finally:
            directory.cleanup()

    def test_event_gate_requires_agent_codex_label(self):
        with tempfile.TemporaryDirectory() as directory:
            event_path = Path(directory) / "event.json"
            event_path.write_text(json.dumps({"action": "labeled", "label": {"name": "status:ready"}, "issue": {"number": 28}}))
            with self.assertRaises(worker.WorkerError):
                worker._event_issue_number(str(event_path))


class WorkflowContractTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = (ROOT / ".github" / "workflows" / "codex-green-worker.yml").read_text(encoding="utf-8")
        cls.docs = (ROOT / "docs" / "DEVELOPMENT.md").read_text(encoding="utf-8")

    def test_workflow_trigger_permissions_and_no_merge(self):
        self.assertIn("issues:", self.workflow)
        self.assertIn("types: [labeled]", self.workflow)
        self.assertIn("github.event.label.name == 'agent:codex'", self.workflow)
        self.assertIn("runs-on: [self-hosted, windows, x64, ml-amstress-codex]", self.workflow)
        self.assertIn("contents: read", self.workflow)
        self.assertIn("issues: write", self.workflow)
        self.assertIn("pull-requests: read", self.workflow)
        self.assertNotIn("OPENAI_API_KEY", self.workflow)
        self.assertNotIn("CODEX_CLI_PACKAGE", self.workflow)
        self.assertNotIn("npm install", self.workflow)
        self.assertIn("CODEX_EXECUTABLE: codex", self.workflow)
        self.assertIn("persist-credentials: false", self.workflow)
        self.assertIn("GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}", self.workflow)
        self.assertIn("uses: actions/create-github-app-token@v3", self.workflow)
        self.assertIn("client-id: ${{ vars.AUTOMATION_APP_CLIENT_ID }}", self.workflow)
        self.assertIn("private-key: ${{ secrets.AUTOMATION_APP_PRIVATE_KEY }}", self.workflow)
        self.assertIn("repositories: ML-AMstress", self.workflow)
        self.assertIn("AUTOMATION_APP_TOKEN: ${{ steps.automation-app-token.outputs.token }}", self.workflow)
        self.assertIn("permission-contents: write", self.workflow)
        self.assertIn("permission-pull-requests: write", self.workflow)
        self.assertNotIn("gh pr merge", self.workflow)
        self.assertNotIn("enablePullRequestAutoMerge", self.workflow)

    def test_workflow_uses_verified_local_python_before_dependencies(self):
        self.assertNotIn("actions/setup-python", self.workflow)
        preflight = self.workflow.index("      - name: Verify local Python")
        install_dependencies = self.workflow.index(
            "      - name: Install normal-Python worker test dependencies"
        )
        self.assertLess(preflight, install_dependencies)
        preflight_step = self.workflow[preflight:install_dependencies]
        self.assertIn("shell: powershell", preflight_step)
        self.assertNotIn("shell: pwsh", preflight_step)
        self.assertIn("Get-Command python", preflight_step)
        self.assertIn("--verify-local-python", preflight_step)
        self.assertIn("$ErrorActionPreference = 'Stop'", preflight_step)
        self.assertIn("Local Python executable could not run.", preflight_step)

    def test_workflow_applies_and_persists_line_endings_before_any_workspace_gate(self):
        checkout = self.workflow.index("      - name: Check out main")
        line_endings = self.workflow.index(
            "      - name: Persist repository-local Git line endings"
        )
        local_python = self.workflow.index("      - name: Verify local Python")
        diagnostic = self.workflow.index(
            "      - name: Diagnose isolated workspace cleanliness"
        )
        worker_step = self.workflow.index("      - name: Run fail-closed GREEN worker")
        self.assertLess(checkout, line_endings)
        self.assertLess(line_endings, local_python)
        self.assertLess(line_endings, diagnostic)
        self.assertLess(line_endings, worker_step)
        checkout_step = self.workflow[checkout:line_endings]
        self.assertIn("GIT_CONFIG_COUNT: \"2\"", checkout_step)
        self.assertIn("GIT_CONFIG_KEY_0: core.autocrlf", checkout_step)
        self.assertIn("GIT_CONFIG_VALUE_0: \"false\"", checkout_step)
        self.assertIn("GIT_CONFIG_KEY_1: core.eol", checkout_step)
        self.assertIn("GIT_CONFIG_VALUE_1: lf", checkout_step)
        policy_step = self.workflow[line_endings:local_python]
        self.assertIn("shell: powershell", policy_step)
        self.assertIn("git config --local core.autocrlf false", policy_step)
        self.assertIn("git config --local core.eol lf", policy_step)
        self.assertNotIn("checkout-index", self.workflow)
        self.assertNotIn("git clean", policy_step)
        self.assertNotIn("git reset", policy_step)

    def test_workflow_diagnoses_workspace_after_dependencies_before_worker(self):
        dependencies = self.workflow.index(
            "      - name: Install normal-Python worker test dependencies"
        )
        diagnostic = self.workflow.index(
            "      - name: Diagnose isolated workspace cleanliness"
        )
        worker_step = self.workflow.index("      - name: Run fail-closed GREEN worker")
        self.assertLess(dependencies, diagnostic)
        self.assertLess(diagnostic, worker_step)
        diagnostic_step = self.workflow[diagnostic:worker_step]
        self.assertIn("shell: powershell", diagnostic_step)
        self.assertIn("--diagnose-workspace", diagnostic_step)
        self.assertNotIn("git clean", diagnostic_step)
        self.assertNotIn("git reset", diagnostic_step)

    def test_auth_and_controlled_integration_setup_are_documented(self):
        for text in (
            "ChatGPT",
            "GITHUB_TOKEN",
            "CODEX_EXPECTED_VERSION",
            "CODEX_EXPECTED_RUNNER_NAME",
            "CODEX_EXPECTED_WINDOWS_USER",
            "status:review",
            "status:blocked",
        ):
            self.assertIn(text, self.docs)
        self.assertIn("Preflight and controlled integration sequence", self.docs)
        self.assertIn("Issue #30 is the controlled live", self.docs)
        self.assertIn("end-to-end GREEN worker integration test", self.docs)
        self.assertIn("trusted-worker preflight", self.docs)
        self.assertIn("authoritative final normal-Python validation", self.docs)
        self.assertIn("not by itself a reason to decline", self.docs)
        self.assertNotIn("Keep Issue #30 untriggered", self.docs)
        self.assertIn("auto-merge operation is available", self.docs)


if __name__ == "__main__":
    unittest.main()
