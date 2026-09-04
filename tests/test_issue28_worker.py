import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import codex_issue_worker as worker


ROOT = Path(__file__).resolve().parents[1]


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
                "scripts/codex_issue_worker.py",
                ".github/workflows/candidate.yml",
                "create_input.py",
            ]
        )
        self.assertEqual(allowed, ("docs/change.md", "scripts/codex_issue_worker.py"))
        self.assertEqual(rejected, (".github/workflows/candidate.yml", "create_input.py"))

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
            },
        ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
            worker.run_codex(issue(), "codex/issue-28-test", tempfile.gettempdir())

        self.assertNotIn("GITHUB_TOKEN", captured["env"])
        self.assertNotIn("GH_TOKEN", captured["env"])
        self.assertEqual(captured["env"]["GIT_CONFIG_NOSYSTEM"], "1")
        self.assertEqual(captured["env"]["GIT_CONFIG_NOGLOBAL"], "1")
        self.assertEqual(captured["env"]["GIT_TERMINAL_PROMPT"], "0")
        self.assertNotIn("github-write-token", captured["command"][-1])

    def test_trusted_push_injects_token_only_for_git_command(self):
        captured = {}

        def fake_run(command, cwd=None, env=None, **kwargs):
            captured["command"] = command
            captured["env"] = env
            return ""

        with mock.patch.dict(
            os.environ,
            {"GITHUB_TOKEN": "github-write-token", "OPENAI_API_KEY": "openai-secret"},
        ), mock.patch.object(worker, "_run", side_effect=fake_run):
            worker.push_branch(tempfile.gettempdir(), "codex/issue-28-test")

        self.assertNotIn("GITHUB_TOKEN", captured["env"])
        self.assertNotIn("OPENAI_API_KEY", captured["env"])
        self.assertEqual(captured["env"]["GIT_CONFIG_KEY_0"], "http.extraheader")
        self.assertIn("github-write-token", captured["env"]["GIT_CONFIG_VALUE_0"])

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

    def test_missing_openai_auth_fails_before_branch_claim(self):
        client = FakeClient(issue())
        with mock.patch.dict(os.environ, {"OPENAI_API_KEY": ""}):
            with self.assertRaises(worker.WorkerError):
                worker.Worker(client, 28, "run-auth").execute()
        self.assertFalse(any(event[0] == "create_branch" for event in client.events))

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
        self.assertIn("contents: write", self.workflow)
        self.assertIn("issues: write", self.workflow)
        self.assertIn("pull-requests: write", self.workflow)
        self.assertIn("OPENAI_API_KEY", self.workflow)
        self.assertIn("persist-credentials: false", self.workflow)
        self.assertIn("GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}", self.workflow)
        self.assertNotIn("gh pr merge", self.workflow)
        self.assertNotIn("enablePullRequestAutoMerge", self.workflow)

    def test_auth_and_controlled_setup_are_documented(self):
        for text in ("OPENAI_API_KEY", "GITHUB_TOKEN", "CODEX_CLI_PACKAGE", "status:review", "status:blocked"):
            self.assertIn(text, self.docs)
        self.assertIn("controlled test issue", self.docs)
        self.assertIn("auto-merge operation is available", self.docs)


if __name__ == "__main__":
    unittest.main()
