import copy
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import codex_issue_worker as worker
from scripts import a5_review_orchestrator as a5
from scripts import yellow_lane_policy as policy
from tests.test_issue28_worker import GREEN_BODY


ROOT = Path(__file__).resolve().parents[1]
BASE_SHA = "a" * 40
HEAD_SHA = "b" * 40
ISSUE_NUMBER = 149
TITLE = "Activate trusted YELLOW implementation"
BRANCH = policy.yellow_branch(ISSUE_NUMBER, TITLE)
YELLOW_BODY = GREEN_BODY.replace("risk:green", "risk:yellow")


def labels(*names):
    return [{"name": name} for name in names]


def yellow_issue(**changes):
    value = {
        "number": ISSUE_NUMBER,
        "title": TITLE,
        "body": YELLOW_BODY,
        "state": "open",
        "updated_at": "v1",
        "labels": labels("status:ready", "risk:yellow", "agent:codex"),
    }
    value.update(changes)
    return value


def green_issue(**changes):
    value = yellow_issue(
        body=GREEN_BODY,
        labels=labels("status:ready", "risk:green", "agent:codex"),
    )
    value.update(changes)
    return value


def prestart_value(paths=None, **changes):
    value = {
        "schema_version": 1,
        "repository": worker.REPOSITORY,
        "issue_number": ISSUE_NUMBER,
        "trusted_base_sha": BASE_SHA,
        "declared_risk": "yellow",
        "effective_risk": "yellow",
        "authorized_paths": paths or ["docs/example.md"],
        "scientific_runtime_prohibited": True,
    }
    value.update(changes)
    return value


def prestart_comment(paths=None, author=None, serialized=None):
    serialized = serialized or policy.serialize_prestart(prestart_value(paths))
    return {
        "body": "<!-- yellow-implementation-prestart:" + serialized + " -->",
        "user": {"login": author or worker.TRUSTED_YELLOW_AUTHORIZATION_AUTHOR},
    }


class FakeClient:
    repository = worker.REPOSITORY

    def __init__(self, issue=None, comments=None, dependencies=None, pulls=None):
        self.issue_data = copy.deepcopy(issue or yellow_issue())
        self.comment_data = copy.deepcopy(comments if comments is not None else [prestart_comment()])
        self.dependencies = dependencies or {}
        self.pulls = list(pulls or [])
        self.refs = {worker.BASE_BRANCH: BASE_SHA}
        self.events = []

    def issue(self, number):
        self.assert_number(number)
        return self.issue_data

    def issue_comments(self, number):
        self.assert_number(number)
        return self.comment_data

    def dependency_issue(self, dependency):
        return {"state": self.dependencies.get(dependency.number, "closed")}

    def open_pulls(self):
        return list(self.pulls)

    def branch_exists(self, branch):
        return branch in self.refs

    def branch_sha(self, branch):
        return self.refs[branch]

    def create_branch(self, branch, sha):
        if branch in self.refs:
            raise worker.WorkerError("duplicate branch")
        self.refs[branch] = sha
        self.events.append(("create_branch", branch, sha))

    def set_issue_labels(self, number, names):
        self.assert_number(number)
        self.issue_data["labels"] = labels(*names)
        self.events.append(("labels", tuple(names)))

    def comment(self, number, body):
        self.assert_number(number)
        self.comment_data.append({"body": body, "user": {"login": worker.TRUSTED_WORKER_AUDIT_AUTHOR}})
        self.events.append(("comment", body))

    def create_pr(self, head, title, body):
        self.events.append(("create_pr", head, title, body))
        return {"number": 250, "html_url": "https://example.invalid/pr/250"}

    def assert_number(self, number):
        if number != ISSUE_NUMBER:
            raise AssertionError("unexpected issue")


class RoutingAndAuthorizationTests(unittest.TestCase):
    def test_router_selects_exactly_one_green_or_yellow_lane(self):
        for issue_data, expected in ((green_issue(), policy.GREEN), (yellow_issue(), policy.YELLOW)):
            with self.subTest(expected=expected):
                client = FakeClient(issue=issue_data, comments=[] if expected == policy.GREEN else None)
                calls = []

                class Stub:
                    def __init__(self, lane):
                        self.lane = lane

                    def execute(self):
                        calls.append(self.lane)
                        return self.lane

                def green_factory(*args, **kwargs):
                    return Stub(policy.GREEN)

                def yellow_factory(*args, **kwargs):
                    return Stub(policy.YELLOW)

                result = worker.IssueRouter(
                    client, client, ISSUE_NUMBER, "900", green_factory=green_factory,
                    yellow_factory=yellow_factory,
                ).execute()
                self.assertEqual(result, expected)
                self.assertEqual(calls, [expected])

    def test_yellow_requires_exact_maintainer_canonical_prestart(self):
        canonical = policy.serialize_prestart(prestart_value())
        cases = (
            [],
            [prestart_comment(), prestart_comment()],
            [prestart_comment(author="untrusted")],
            [prestart_comment(serialized="{bad}")],
            [prestart_comment(serialized=json.dumps(prestart_value(), sort_keys=False))],
            [prestart_comment(serialized=policy.serialize_prestart(prestart_value(trusted_base_sha="c" * 40)))],
        )
        for comments in cases:
            with self.subTest(comments=comments), self.assertRaises(worker.WorkerError):
                worker.select_issue_lane(FakeClient(comments=comments), ISSUE_NUMBER)
        evidence, serialized = worker.yellow_prestart_authorization(
            [prestart_comment()], ISSUE_NUMBER, BASE_SHA
        )
        self.assertEqual(serialized, canonical)
        self.assertEqual(evidence.authorized_paths, ("docs/example.md",))

    def test_status_risk_contract_dependency_duplicate_stale_and_scientific_reject(self):
        dependent_body = YELLOW_BODY.replace("- none", "- blocked-by: #147")
        open_pr = {"state": "open", "headRefName": BRANCH, "title": "", "body": "Refs #149"}
        cases = (
            FakeClient(issue=yellow_issue(state="closed")),
            FakeClient(issue=yellow_issue(labels=labels("risk:yellow", "agent:codex"))),
            FakeClient(issue=yellow_issue(labels=labels("status:ready", "risk:red", "agent:codex")), comments=[]),
            FakeClient(issue=yellow_issue(labels=labels("status:ready", "risk:green", "risk:yellow", "agent:codex"))),
            FakeClient(issue=yellow_issue(body="malformed")),
            FakeClient(issue=yellow_issue(body=dependent_body), dependencies={147: "open"}),
            FakeClient(pulls=[open_pr]),
            FakeClient(comments=[prestart_comment(paths=["create_input.py"])]),
        )
        branch_duplicate = FakeClient(); branch_duplicate.refs[BRANCH] = BASE_SHA
        cases += (branch_duplicate,)
        for client in cases:
            with self.subTest(issue=client.issue_data), self.assertRaises(worker.WorkerError):
                worker.select_issue_lane(client, ISSUE_NUMBER)

    def test_authorized_changed_paths_are_exact_subset_and_reject_unsafe_scope(self):
        evidence = policy.validate_prestart(prestart_value([".github/workflows/codex-green-worker.yml", "docs/example.md"]))
        self.assertEqual(worker.yellow_changed_paths(["docs/example.md"], evidence), ("docs/example.md",))
        self.assertEqual(
            worker.yellow_changed_paths(["docs/example.md", ".github/workflows/codex-green-worker.yml"], evidence),
            (".github/workflows/codex-green-worker.yml", "docs/example.md"),
        )
        for paths in ([], ["docs/other.md"], ["docs/example.md", "docs/other.md"],
                      ["docs/example.md", "docs/example.md"], ["../docs/example.md"], ["C:/Users/x"]):
            with self.subTest(paths=paths), self.assertRaises(worker.WorkerError):
                worker.yellow_changed_paths(paths, evidence)
        scientific = policy.validate_prestart(prestart_value(["create_input.py"]))
        with self.assertRaisesRegex(worker.WorkerError, "scientific/runtime"):
            worker.yellow_changed_paths(["create_input.py"], scientific)

    def test_live_yellow_policy_is_protected_from_green_self_modification(self):
        allowed, denied = worker.green_changed_paths(["scripts/yellow_lane_policy.py"])
        self.assertEqual(allowed, ())
        self.assertEqual(denied, ("scripts/yellow_lane_policy.py",))


class YellowExecutionTests(unittest.TestCase):
    def _execute(self, client=None, paths=("docs/example.md",), codex=None):
        client = client or FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)
        codex = codex or mock.Mock(return_value=("implemented", ""))
        validate = mock.Mock()

        def push(cwd, branch):
            client.refs[branch] = HEAD_SHA

        def fake_run(command, **kwargs):
            if command[:3] == ["git", "status", "--porcelain"]:
                return " M docs/example.md\n"
            if command[:3] == ["git", "rev-parse", "HEAD"]:
                return HEAD_SHA + "\n"
            return ""

        with mock.patch.object(worker, "checkout_claimed_worker_branch"), mock.patch.object(
            worker, "_all_changed_paths", return_value=tuple(paths)
        ), mock.patch.object(worker, "_git_paths", return_value=tuple(paths)), mock.patch.object(
            worker, "_run", side_effect=fake_run
        ):
            result = worker.YellowWorker(
                client, client, decision, "900", cwd="work", codex_runner=codex,
                validation_runner=validate, push_runner=push,
            ).execute()
        return client, codex, validate, result

    def test_success_claims_validates_pushes_one_refs_pr_and_moves_to_review(self):
        client, codex, validate, result = self._execute()
        self.assertEqual(result["number"], 250)
        codex.assert_called_once_with(client.issue_data, BRANCH, ("docs/example.md",), "work")
        validate.assert_called_once_with("work")
        created = [event for event in client.events if event[0] == "create_pr"]
        self.assertEqual(len(created), 1)
        body = created[0][3]
        self.assertEqual(body.splitlines()[0], "Refs #149")
        self.assertEqual(body.count("Refs #149"), 1)
        self.assertNotIn("Closes #149", body)
        statuses = [name for name in worker._label_names(client.issue_data) if name.startswith("status:")]
        self.assertEqual(statuses, ["status:review"])
        bodies = [comment["body"] for comment in client.comment_data]
        self.assertEqual(sum(body.startswith("<!-- a5.yellow-prestart:") for body in bodies), 1)
        self.assertEqual(sum(body.startswith("<!-- a5.yellow-claim:") for body in bodies), 1)
        a5_identity = a5._automated_yellow_authorization(
            client.comment_data, ISSUE_NUMBER, BRANCH, BASE_SHA
        )
        self.assertEqual(policy.parse_prestart(a5_identity["prestart"]).authorized_paths,
                         ("docs/example.md",))
        self.assertEqual(policy.parse_claim(a5_identity["claim"]).branch, BRANCH)
        result_markers = [body for body in bodies if body.startswith("<!-- yellow-worker-result:")]
        self.assertEqual(len(result_markers), 1)
        for value in (BASE_SHA, HEAD_SHA, BRANCH, '"runtime_evidence":"pending"'):
            self.assertIn(value, result_markers[0])

    def test_scope_failure_after_codex_blocks_before_validation_push_pr_or_repair(self):
        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)
        validate, push = mock.Mock(), mock.Mock()
        with mock.patch.object(worker, "checkout_claimed_worker_branch"), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/unauthorized.md",)
        ), mock.patch.object(worker, "_repair", create=True) as repair_call:
            with self.assertRaisesRegex(worker.WorkerError, "pre-authorized scope"):
                worker.YellowWorker(
                    client, client, decision, "900", codex_runner=mock.Mock(return_value=("done", "")),
                    validation_runner=validate, push_runner=push,
                ).execute()
        validate.assert_not_called(); push.assert_not_called(); repair_call.assert_not_called()
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))
        self.assertIn("status:blocked", worker._label_names(client.issue_data))

    def test_scope_is_rechecked_after_validation_and_before_push(self):
        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)
        validate, push = mock.Mock(), mock.Mock()
        with mock.patch.object(worker, "checkout_claimed_worker_branch"), mock.patch.object(
            worker, "_all_changed_paths",
            side_effect=(("docs/example.md",), ("docs/example.md", "docs/unauthorized.md")),
        ):
            with self.assertRaisesRegex(worker.WorkerError, "pre-authorized scope"):
                worker.YellowWorker(
                    client, client, decision, "900",
                    codex_runner=mock.Mock(return_value=("done", "")),
                    validation_runner=validate, push_runner=push,
                ).execute()
        validate.assert_called_once_with(os.getcwd())
        push.assert_not_called()
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))

    def test_prestart_evidence_is_rechecked_after_validation(self):
        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)

        def mutate_authorization(_cwd):
            client.comment_data[0] = prestart_comment(paths=["docs/other.md"])

        with mock.patch.object(worker, "checkout_claimed_worker_branch"), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/example.md",)
        ):
            with self.assertRaisesRegex(worker.WorkerError, "pre-start evidence changed"):
                worker.YellowWorker(
                    client, client, decision, "900",
                    codex_runner=mock.Mock(return_value=("done", "")),
                    validation_runner=mutate_authorization, push_runner=mock.Mock(),
                ).execute()
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))

    def test_issue_race_after_push_cannot_create_a_pr(self):
        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)

        def push_then_mutate(_cwd, branch):
            client.refs[branch] = HEAD_SHA
            client.issue_data["body"] += "\nraced after push"

        def fake_run(command, **_kwargs):
            if command[:3] == ["git", "status", "--porcelain"]:
                return " M docs/example.md\n"
            if command[:3] == ["git", "rev-parse", "HEAD"]:
                return HEAD_SHA + "\n"
            return ""

        with mock.patch.object(worker, "checkout_claimed_worker_branch"), mock.patch.object(
            worker, "_all_changed_paths", return_value=("docs/example.md",)
        ), mock.patch.object(worker, "_git_paths", return_value=("docs/example.md",)), mock.patch.object(
            worker, "_run", side_effect=fake_run
        ):
            with self.assertRaisesRegex(worker.WorkerError, "identity changed"):
                worker.YellowWorker(
                    client, client, decision, "900",
                    codex_runner=mock.Mock(return_value=("done", "")),
                    validation_runner=mock.Mock(), push_runner=push_then_mutate,
                ).execute()
        self.assertFalse(any(event[0] == "create_pr" for event in client.events))

    def test_preclaim_and_postclaim_races_fail_closed(self):
        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)
        client.issue_data["updated_at"] = "v2"
        with self.assertRaisesRegex(worker.WorkerError, "changed before"):
            worker.YellowWorker(
                client, client, decision, "900", codex_runner=mock.Mock(),
                validation_runner=mock.Mock(), push_runner=mock.Mock(),
            ).execute()
        self.assertNotIn(BRANCH, client.refs)

        client = FakeClient()
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)

        def mutate_claim(branch, sha):
            client.refs[branch] = sha
            client.issue_data["body"] += "\nrace"

        client.create_branch = mutate_claim
        with self.assertRaisesRegex(worker.WorkerError, "identity changed"):
            worker.YellowWorker(
                client, client, decision, "900", codex_runner=mock.Mock(),
                validation_runner=mock.Mock(), push_runner=mock.Mock(),
            ).execute()

    def test_existing_or_conflicting_claim_evidence_never_replays_execution(self):
        claim = policy.serialize_claim({
            "schema_version": 1, "repository": worker.REPOSITORY,
            "issue_number": ISSUE_NUMBER, "trusted_base_sha": BASE_SHA,
            "branch": BRANCH, "lane": policy.AUTOMATED_YELLOW_LANE,
        })
        claim_comment = {"body": "<!-- a5.yellow-claim:" + claim + " -->",
                         "user": {"login": worker.TRUSTED_WORKER_AUDIT_AUTHOR}}
        client = FakeClient(comments=[prestart_comment(), claim_comment])
        decision = worker.select_issue_lane(client, ISSUE_NUMBER)
        codex = mock.Mock()
        with self.assertRaisesRegex(worker.WorkerError, "exists before branch claim"):
            worker.YellowWorker(
                client, client, decision, "900", codex_runner=codex,
                validation_runner=mock.Mock(), push_runner=mock.Mock(),
            ).execute()
        codex.assert_not_called()

    def test_yellow_codex_uses_workspace_sandbox_stdin_and_strips_credentials(self):
        captured = {}

        def fake_run(command, **kwargs):
            captured["command"] = command
            captured["kwargs"] = kwargs
            return type("Result", (), {"returncode": 0, "stdout": "ok", "stderr": ""})()

        secrets = {"GITHUB_TOKEN": "state", "GH_TOKEN": "gh", "AUTOMATION_APP_TOKEN": "app",
                   "OPENAI_API_KEY": "forbidden"}
        with mock.patch.dict(os.environ, secrets, clear=False), mock.patch.object(
            worker, "resolve_codex_executable", return_value="C:/trusted/codex.exe"
        ), mock.patch.object(worker.subprocess, "run", side_effect=fake_run):
            worker.run_yellow_codex(yellow_issue(), BRANCH, ("docs/example.md",), "work")
        self.assertEqual(captured["command"][-1], "-")
        self.assertIn("workspace-write", captured["command"])
        self.assertIn('approval_policy="never"', captured["command"])
        self.assertNotIn("Exact issue contract", " ".join(captured["command"]))
        self.assertIn("docs/example.md", captured["kwargs"]["input"])
        for name in secrets:
            self.assertNotIn(name, captured["kwargs"]["env"])
        self.assertFalse(captured["kwargs"].get("shell", False))


class WorkflowAndNoMergeTests(unittest.TestCase):
    def test_shared_workflow_has_one_router_job_and_unchanged_credentials_permissions(self):
        workflow = (ROOT / ".github/workflows/codex-green-worker.yml").read_text(encoding="utf-8")
        self.assertIn("name: Codex issue worker", workflow)
        self.assertEqual(workflow.count("runs-on: [self-hosted, windows, x64, ml-amstress-codex]"), 1)
        self.assertIn("Run fail-closed GREEN/YELLOW router", workflow)
        self.assertIn("python scripts/codex_issue_worker.py", workflow)
        self.assertEqual(workflow.count("github.event.label.name == 'agent:codex'"), 1)
        self.assertEqual(workflow.count("  run-worker:"), 1)
        self.assertNotIn("run-green-worker:", workflow)
        self.assertNotIn("run-yellow-worker:", workflow)
        for value in ("contents: read", "issues: write", "pull-requests: read",
                      "persist-credentials: false", "AUTOMATION_APP_TOKEN"):
            self.assertIn(value, workflow)
        for forbidden in ("gh pr merge", "enablePullRequestAutoMerge", "auto-merge"):
            self.assertNotIn(forbidden, workflow)
        source = Path(worker.__file__).read_text(encoding="utf-8")
        self.assertNotIn('"/merges"', source)
        self.assertNotIn("auto_merge", source)


if __name__ == "__main__":
    unittest.main()
