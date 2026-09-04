import copy
import io
import inspect
import os
from pathlib import Path
import subprocess
import unittest
import urllib.error
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_repair_worker as repair
from scripts import a5_review_state as state_contract
from scripts import a5_reviewer as reviewer
from scripts import codex_issue_worker as green_worker


HEAD = "a" * 40
NEW_HEAD = "b" * 40
BRANCH = "codex/issue-75-a5-green-task"
ROOT = Path(__file__).resolve().parents[1]

ISSUE_BODY = """## Goal
Test.
## Necessity Gate
Test.
## Required behavior
Test.
## Do not change
Test.
## Acceptance criteria
Test.
## Tests/validation
Test.
## Risk classification
Declared risk label: `risk:green`
## Dependencies
- none
"""


def labels(*names):
    return [{"name": name} for name in names]


def issue(**changes):
    value = {"number": 75, "state": "open", "title": "A5 green task", "body": ISSUE_BODY,
             "labels": labels("status:review", "risk:green", "agent:codex")}
    value.update(changes)
    return value


def pull_request(**changes):
    value = {"number": 175, "state": "open", "title": "Issue #75", "body": "Closes #75",
             "labels": labels(), "base": {"ref": "main", "sha": "c" * 40},
             "head": {"sha": HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}}}
    value.update(changes)
    return value


def event(conclusion="success", sha=HEAD):
    return {"workflow_run": {"id": 100, "name": "Normal Python CI", "status": "completed",
                              "conclusion": conclusion, "head_sha": sha}}


def clean_verdict():
    return reviewer.ReviewVerdict(1, "clean", HEAD, "green", "clean", (), "")


def blocker_verdict():
    finding = reviewer.Finding("F-1", "tests", "test blocker", "update test", "test passes")
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "green", "blocker", (finding,), "")


class FakeClient:
    def __init__(self, pr=None, linked_issue=None, files=None):
        self.pr_data = copy.deepcopy(pr or pull_request())
        self.issue_data = copy.deepcopy(linked_issue or issue())
        self.files_data = copy.deepcopy(files or [{"filename": "docs/change.md", "patch": "+safe"}])
        self.comment_data = []
        self.label_updates = []
        self.repository_label_data = labels(*sorted(orchestrator.REVIEW_LABELS))
        self.created_labels = []

    def open_prs_for_head(self, head):
        return [self.pr_data] if self.pr_data["head"]["sha"] == head else []

    def pr(self, number):
        self.assert_number(number, self.pr_data["number"])
        return self.pr_data

    def issue(self, number):
        self.assert_number(number, self.issue_data["number"])
        return self.issue_data

    @staticmethod
    def assert_number(actual, expected):
        if actual != expected:
            raise AssertionError("wrong GitHub object requested")

    def comments(self, number):
        self.assert_number(number, self.pr_data["number"])
        return self.comment_data

    def changed_files(self, number):
        self.assert_number(number, self.pr_data["number"])
        return self.files_data

    def dependency_issue(self, dependency):
        return {"state": "closed"}

    def repository_labels(self):
        return self.repository_label_data

    def create_label(self, specification):
        self.created_labels.append(specification)
        self.repository_label_data.append({"name": specification["name"]})

    def set_labels(self, number, names):
        target = self.pr_data if number == self.pr_data["number"] else self.issue_data
        target["labels"] = labels(*names)
        self.label_updates.append((number, tuple(names)))

    def comment(self, number, body):
        self.assert_number(number, self.pr_data["number"])
        self.comment_data.append({"body": body, "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})


class WorkflowAndEligibilityTests(unittest.TestCase):
    def test_a5_workflow_mints_repository_scoped_app_token_for_repair_push_only(self):
        workflow = (ROOT / ".github" / "workflows" / "a5-review-loop.yml").read_text(encoding="utf-8")
        self.assertIn("uses: actions/create-github-app-token@v3", workflow)
        self.assertIn("client-id: ${{ vars.AUTOMATION_APP_CLIENT_ID }}", workflow)
        self.assertIn("private-key: ${{ secrets.AUTOMATION_APP_PRIVATE_KEY }}", workflow)
        self.assertIn("owner: RayZhang2024", workflow)
        self.assertIn("repositories: ML-AMstress", workflow)
        self.assertIn("permission-contents: write", workflow)
        self.assertIn("AUTOMATION_APP_TOKEN: ${{ steps.automation-app-token.outputs.token }}", workflow)
        self.assertIn("contents: read", workflow)
        self.assertIn("issues: write", workflow)
        self.assertIn("pull-requests: read", workflow)
        self.assertIn("persist-credentials: false", workflow)
        self.assertNotIn("enablePullRequestAutoMerge", workflow)

    def test_a5_workflow_runs_orchestrator_as_a_package_module(self):
        workflow = (ROOT / ".github" / "workflows" / "a5-review-loop.yml").read_text(encoding="utf-8")
        self.assertIn("python -m scripts.a5_review_orchestrator", workflow)
        self.assertNotIn("python scripts/a5_review_orchestrator.py", workflow)

    def test_orchestrator_requires_app_token_before_trusted_execution(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "AUTOMATION_APP_TOKEN"):
                orchestrator.require_automation_app_token()


class TrustedRestBoundaryTests(unittest.TestCase):
    def test_request_uses_fixed_user_agent_and_stable_operation(self):
        response = mock.MagicMock()
        response.read.return_value = b'{"number": 175}'
        response.__enter__.return_value = response
        captured = {}

        def open_request(request, timeout):
            captured["request"], captured["timeout"] = request, timeout
            return response

        with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=open_request):
            value = orchestrator.GitHubClient("sentinel-token").pr(175)
        self.assertEqual(value, {"number": 175})
        self.assertEqual(captured["timeout"], 30)
        self.assertEqual(captured["request"].get_header("User-agent"), orchestrator.A5_GITHUB_USER_AGENT)
        self.assertNotIn("sentinel-token", orchestrator.A5_GITHUB_USER_AGENT)
        self.assertIn("get-pr", orchestrator.REST_OPERATIONS)

    def test_http_failure_exposes_only_operation_and_numeric_status(self):
        secret = "sentinel-token-response-body-prompt-C:/Users/private"
        failure = urllib.error.HTTPError(
            "https://api.github.com/repos/private/repo/pulls?secret=" + secret,
            403,
            "sentinel GitHub error text",
            {"X-Secret": secret},
            io.BytesIO(secret.encode("utf-8")),
        )
        with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=failure):
            with self.assertRaises(orchestrator.OrchestrationError) as raised:
                orchestrator.GitHubClient(secret).comment(175, "prompt-like payload " + secret)
        self.assertEqual(str(raised.exception), "GitHub create-audit-comment: HTTP 403")
        for forbidden in (secret, "https://", "private/repo", "GitHub error", "payload", "C:/Users"):
            self.assertNotIn(forbidden, str(raised.exception))

    def test_transport_failure_exposes_only_fixed_category(self):
        secret = "sentinel-token-transport-C:/Users/private"
        with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=urllib.error.URLError(secret)):
            with self.assertRaises(orchestrator.OrchestrationError) as raised:
                orchestrator.GitHubClient(secret).issue(75)
        self.assertEqual(str(raised.exception), "GitHub get-issue: transport failure")
        self.assertNotIn(secret, str(raised.exception))
        self.assertNotIn("C:/Users", str(raised.exception))

    def test_invalid_response_exposes_only_fixed_category(self):
        secret = "sentinel-token-invalid-json-prompt-C:/Users/private"
        response = mock.MagicMock()
        response.read.return_value = ("{invalid:" + secret + "}").encode("utf-8")
        response.__enter__.return_value = response
        with mock.patch.object(orchestrator.urllib.request, "urlopen", return_value=response):
            with self.assertRaises(orchestrator.OrchestrationError) as raised:
                orchestrator.GitHubClient(secret).pr(175)
        self.assertEqual(str(raised.exception), "GitHub get-pr: invalid response")
        for forbidden in (secret, "C:/Users", "invalid:"):
            self.assertNotIn(forbidden, str(raised.exception))

    def test_all_trusted_rest_operations_are_bounded_and_named(self):
        source = inspect.getsource(orchestrator.GitHubClient)
        for operation in orchestrator.REST_OPERATIONS:
            self.assertIn('"' + operation + '"', source)
        self.assertNotIn("trusted GitHub API request failed", source)

    def test_review_label_provisioning_is_idempotent_and_creates_missing(self):
        existing = FakeClient()
        orchestrator.ensure_review_labels(existing)
        self.assertEqual(existing.created_labels, [])

        missing = FakeClient()
        missing.repository_label_data = [{"name": "review:pending"}]
        orchestrator.ensure_review_labels(missing)
        self.assertEqual({item["name"] for item in missing.created_labels}, orchestrator.REVIEW_LABELS - {"review:pending"})

    def test_review_label_creation_failure_fails_closed(self):
        client = FakeClient()
        client.repository_label_data = []
        client.create_label = mock.Mock(side_effect=orchestrator.OrchestrationError("API failure"))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.ensure_review_labels(client)
    def test_workflow_run_requires_exact_name_completed_terminal_and_sha(self):
        self.assertEqual(orchestrator.parse_workflow_run(event()).head_sha, HEAD)
        for changed in ({"name": "Other"}, {"status": "in_progress"}, {"conclusion": None}, {"head_sha": "bad"}):
            bad = event()
            bad["workflow_run"].update(changed)
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.parse_workflow_run(bad)

    def test_exact_one_pr_link_and_internal_identity_are_required(self):
        self.assertEqual(orchestrator.canonical_linked_issue(pull_request()), 75)
        for body in ("", "Closes #75\nFixes #76"):
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.canonical_linked_issue(pull_request(body=body))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_pr_identity(pull_request(head={"sha": HEAD, "ref": BRANCH, "repo": {"full_name": "fork/repo"}}),
                                              orchestrator.parse_workflow_run(event()))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_issue_identity(issue(labels=labels("status:review", "risk:green")), BRANCH, 75)

    def test_open_dependency_fails_closed(self):
        dependent = issue(body=ISSUE_BODY.replace("- none", "- blocked-by: #1"))
        contract = orchestrator.validate_issue_identity(dependent, BRANCH, 75)
        client = FakeClient(linked_issue=dependent)
        client.dependency_issue = lambda _: {"state": "open"}
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_dependencies(client, contract)

    def test_non_success_ci_records_one_observation_and_never_reviews(self):
        client = FakeClient()
        review = mock.Mock()
        self.assertEqual(orchestrator.orchestrate(client, event("failure"), ".", review), "ci-non-success")
        self.assertEqual(orchestrator.orchestrate(client, event("failure"), ".", review), "ci-non-success")
        review.assert_not_called()
        self.assertEqual(len(client.comment_data), 1)

    def test_ambiguous_head_never_mutates_state(self):
        client = FakeClient()
        client.open_prs_for_head = lambda _: [client.pr_data, client.pr_data]
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.orchestrate(client, event(), ".", mock.Mock())
        self.assertEqual(client.comment_data, [])

    def test_snapshot_is_exact_head_and_green_changed_paths_only(self):
        client = FakeClient(files=[{"filename": "docs/change.md", "patch": "+safe"}])
        snapshot, paths = orchestrator.build_snapshot(client.pr_data, client.issue_data, orchestrator.parse_workflow_run(event()), client.files_data)
        self.assertEqual((snapshot["head_sha"], paths, snapshot["ci_checks"]), (HEAD, ("docs/change.md",), [{"name": "Normal Python CI", "status": "success"}]))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.build_snapshot(client.pr_data, client.issue_data, orchestrator.parse_workflow_run(event()),
                                        [{"filename": "create_input.py", "patch": "+physics"}])


class StateAndRepairTests(unittest.TestCase):
    def test_initial_clean_transition_uses_exclusive_labels_and_audit(self):
        client = FakeClient()
        self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: clean_verdict()), "review-clean")
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:clean"])
        self.assertEqual([item["name"] for item in client.issue_data["labels"] if item["name"].startswith("status:")], ["status:review"])
        self.assertEqual(len(client.comment_data), 2)
        self.assertIn("a5.4a-state", client.comment_data[-1]["body"])

    def test_escalation_persists_on_later_head(self):
        client = FakeClient()
        verdict = reviewer.ReviewVerdict(1, "escalate", HEAD, "red", "unsafe", (), "scientific ambiguity")
        self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: verdict), "review-escalated")
        client.pr_data["head"]["sha"] = NEW_HEAD
        self.assertEqual(orchestrator.orchestrate(client, event(sha=NEW_HEAD), ".", mock.Mock()), "review-escalated")

    def test_non_green_blocker_cannot_reach_repair(self):
        client = FakeClient()
        verdict = reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", "unsafe", (reviewer.Finding(
            "F-1", "scope", "risk", "stop", "human"),), "")
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.orchestrate(client, event(), ".", lambda *_: verdict)
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:pending"])

    def test_stale_head_after_review_is_rejected_before_mutation(self):
        client = FakeClient()
        def stale(*_):
            client.pr_data["head"]["sha"] = NEW_HEAD
            return clean_verdict()
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.orchestrate(client, event(), ".", stale)
        self.assertEqual(client.comment_data[0]["body"].startswith("<!-- a5.4a-state:"), True)

    def test_reviewer_metadata_races_cannot_apply_verdict_or_repair(self):
        changes = (
            lambda client: client.issue_data["labels"].__setitem__(1, {"name": "risk:yellow"}),
            lambda client: client.issue_data.__setitem__("body", ISSUE_BODY.replace("- none", "- blocked-by: #1")),
            lambda client: (client.issue_data.__setitem__("body", ISSUE_BODY.replace("- none", "- blocked-by: #1")),
                            setattr(client, "dependency_issue", lambda _: {"state": "open"})),
            lambda client: client.pr_data.__setitem__("body", "Closes #76"),
            lambda client: client.pr_data["base"].__setitem__("ref", "release"),
            lambda client: client.pr_data["head"].__setitem__("ref", "codex/issue-75-other"),
        )
        for change in changes:
            client = FakeClient()
            def raced(*_):
                change(client)
                return clean_verdict()
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.orchestrate(client, event(), ".", raced)
            self.assertNotIn("review:clean", [item["name"] for item in client.pr_data["labels"]])
            self.assertFalse(any("a5.4a-repair" in item["body"] for item in client.comment_data))

    def test_repair_attempt_markers_are_bounded_and_unique(self):
        comments = [{"body": orchestrator._repair_marker(175, 75, HEAD, "a5.2:" + "a" * 64, 1, ("F-1",)),
                     "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}}]
        self.assertEqual(orchestrator.repair_attempt_count(comments, 175), 1)
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.repair_attempt_count(comments * 2, 175)

    def test_successful_repair_uses_exact_existing_paths_and_returns_pending(self):
        client = FakeClient()
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        state = orchestrator._state_input(175, 75, HEAD, current, "verdict", blocker_verdict())
        plan = state_contract.transition(state)
        client.pr_data["labels"] = labels("review:blocker")
        client.issue_data["labels"] = labels("status:in-progress", "risk:green", "agent:codex")
        client.comment_data.append({"body": orchestrator._audit_body(state, plan),
                                    "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})
        captured = {}
        def repaired(request, cwd):
            captured["request"] = request
            client.pr_data["head"]["sha"] = NEW_HEAD
            return repair.RepairResult(1, orchestrator.REPOSITORY, 175, 75, BRANCH, 1, HEAD, NEW_HEAD,
                                       ("F-1",), ("docs/change.md",), "passed", "a5.3:" + "b" * 64)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
             mock.patch.object(orchestrator.repair, "execute_repair", side_effect=repaired):
            result = orchestrator._repair(client, client.pr_data, client.issue_data, client.comment_data, current,
                                           blocker_verdict(), plan.decision_key, ("docs/change.md",), ".")
        self.assertEqual(result, "repair-pushed")
        self.assertEqual(captured["request"].allowed_paths, ("docs/change.md",))
        self.assertEqual(captured["request"].review_decision_key, plan.decision_key)
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:pending"])

    def test_same_head_blocker_replay_recovers_original_key_for_second_attempt(self):
        client = FakeClient()
        pending = orchestrator.CurrentReviewState("status:review", "review:pending", HEAD)
        accepted_input = orchestrator._state_input(175, 75, HEAD, pending, "verdict", blocker_verdict())
        accepted_plan = state_contract.transition(accepted_input)
        client.pr_data["labels"] = labels("review:blocker")
        client.issue_data["labels"] = labels("status:in-progress", "risk:green", "agent:codex")
        client.comment_data.extend((
            {"body": orchestrator._audit_body(accepted_input, accepted_plan),
             "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}},
            {"body": orchestrator._repair_marker(175, 75, HEAD, accepted_plan.decision_key, 1, ("F-1",)),
             "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}},
        ))
        captured = {}
        def second_attempt(request, cwd):
            captured["request"] = request
            client.pr_data["head"]["sha"] = NEW_HEAD
            return repair.RepairResult(1, orchestrator.REPOSITORY, 175, 75, BRANCH, 2, HEAD, NEW_HEAD,
                                       ("F-1",), ("docs/change.md",), "passed", "a5.3:" + "b" * 64)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
             mock.patch.object(orchestrator.repair, "execute_repair", side_effect=second_attempt):
            self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: blocker_verdict()), "repair-pushed")
        self.assertEqual(captured["request"].review_decision_key, accepted_plan.decision_key)
        self.assertEqual(captured["request"].attempt_number, 2)
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 2)

    def test_repair_failure_keeps_blocker_state(self):
        client = FakeClient()
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch", side_effect=orchestrator.OrchestrationError("no")):
            result = orchestrator._repair(client, client.pr_data, client.issue_data, [], current, blocker_verdict(),
                                           "a5.2:" + "a" * 64, ("docs/change.md",), ".")
        self.assertEqual(result, "repair-failed")
        self.assertIn("repair-failed", client.comment_data[-1]["body"])

    def test_incompatible_repair_request_consumes_no_attempt_or_runs_codex(self):
        client = FakeClient()
        current = orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD)
        scientific = reviewer.ReviewVerdict(1, "blocker", HEAD, "green", "blocked", (reviewer.Finding(
            "F-1", "scientific", "unsafe", "stop", "human"),), "")
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch") as checkout:
            with self.assertRaises(repair.RepairError):
                orchestrator._repair(client, client.pr_data, client.issue_data, [], current, scientific,
                                     "a5.2:" + "a" * 64, ("docs/change.md",), ".")
        checkout.assert_not_called()
        self.assertEqual(client.comment_data, [])

    def test_exact_branch_checkout_requires_matching_remote_and_local_heads(self):
        calls = []
        def run(command, **_):
            calls.append(command)
            if command[1:3] == ["rev-parse", "origin/" + BRANCH] or command[1:] == ["rev-parse", "HEAD"]:
                return subprocess.CompletedProcess(command, 0, HEAD + "\n", "")
            return subprocess.CompletedProcess(command, 0, "", "")
        with mock.patch.object(orchestrator.subprocess, "run", side_effect=run):
            orchestrator.checkout_exact_pr_branch(BRANCH, HEAD, ".")
        self.assertIn("refs/heads/%s:refs/remotes/origin/%s" % (BRANCH, BRANCH), calls[0])
        with mock.patch.object(orchestrator.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, NEW_HEAD + "\n", "")):
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.checkout_exact_pr_branch(BRANCH, HEAD, ".")


class BoundaryTests(unittest.TestCase):
    def test_live_a5_files_are_denied_to_green_worker(self):
        allowed, denied = green_worker.green_changed_paths(("scripts/a5_reviewer.py", "scripts/a5_review_state.py",
                                                             "scripts/a5_repair_worker.py", "scripts/a5_review_orchestrator.py"))
        self.assertEqual(allowed, ())
        self.assertEqual(denied, ("scripts/a5_reviewer.py", "scripts/a5_review_state.py", "scripts/a5_repair_worker.py",
                                  "scripts/a5_review_orchestrator.py"))

    def test_untrusted_comment_cannot_supply_review_state_evidence(self):
        client = FakeClient()
        current = orchestrator.CurrentReviewState("status:review", "review:pending", HEAD)
        plan_input = orchestrator._state_input(175, 75, HEAD, current, "verdict", clean_verdict())
        plan = state_contract.transition(plan_input)
        client.pr_data["labels"] = labels("review:clean")
        client.comment_data.append({"body": orchestrator._audit_body(plan_input, plan), "user": {"login": "untrusted"}})
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.current_review_state(client.pr_data, client.issue_data, client.comment_data)

    def test_orchestrator_has_no_merge_or_auto_merge_route(self):
        source = inspect.getsource(orchestrator).lower()
        self.assertNotIn('"/merges"', source)
        self.assertNotIn('"/auto_merge"', source)


if __name__ == "__main__":
    unittest.main()
