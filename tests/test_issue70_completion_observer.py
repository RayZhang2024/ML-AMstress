import copy
import inspect
from pathlib import Path
import unittest

from scripts import codex_completion_observer as observer


ROOT = Path(__file__).resolve().parents[1]
HEAD = "a" * 40
BRANCH = "codex/issue-70-completion-observer"
ISSUE_NUMBER = 70
RUN_ID = 700


def event(conclusion="success", **changes):
    run = {
        "id": RUN_ID,
        "name": observer.WORKFLOW_NAME,
        "event": observer.WORKFLOW_EVENT,
        "status": "completed",
        "conclusion": conclusion,
        "head_branch": BRANCH,
        "head_sha": HEAD,
        "created_at": "2026-09-05T00:00:00Z",
        "updated_at": "2026-09-05T00:01:00Z",
    }
    run.update(changes)
    return {"repository": {"full_name": observer.REPOSITORY}, "workflow_run": run}


def claim(number=ISSUE_NUMBER, run_id=RUN_ID, branch=BRANCH):
    return {
        "body": "<!-- codex-worker-claim issue:%d run:%d branch:%s -->\ntrusted worker claim" % (
            number, run_id, branch
        ),
        "user": {"login": observer.TRUSTED_CLAIM_AUTHOR},
    }


class FakeClient:
    def __init__(self, issues=None, comments=None, pulls=None):
        self.issues_data = copy.deepcopy(issues if issues is not None else [{"number": ISSUE_NUMBER}])
        self.comments_data = copy.deepcopy(comments if comments is not None else {ISSUE_NUMBER: [claim()]})
        self.pulls_data = copy.deepcopy(pulls if pulls is not None else [])
        self.comment_calls = []

    def open_issues(self):
        return self.issues_data

    def comments(self, number):
        return self.comments_data.get(number, [])

    def open_prs_for_branch(self, branch):
        if branch != BRANCH:
            raise AssertionError("unexpected branch")
        return self.pulls_data

    def comment(self, number, body):
        self.comment_calls.append((number, body))
        self.comments_data.setdefault(number, []).append(
            {"body": body, "user": {"login": observer.TRUSTED_CLAIM_AUTHOR}}
        )


class EventAndClaimTests(unittest.TestCase):
    def test_exact_terminal_event_is_required(self):
        run = observer.parse_workflow_run(event())
        self.assertEqual(
            (run.run_id, run.conclusion, run.branch, run.head_sha, run.created_at, run.updated_at),
            (RUN_ID, "success", BRANCH, HEAD, "2026-09-05T00:00:00Z", "2026-09-05T00:01:00Z"),
        )
        cases = (
            {"name": "Other workflow"},
            {"event": "pull_request"},
            {"status": "in_progress"},
            {"conclusion": None},
            {"head_branch": "feature/unsafe"},
            {"head_sha": "bad"},
            {"created_at": "unbounded"},
        )
        for changes in cases:
            with self.assertRaises(observer.ObserverError):
                observer.parse_workflow_run(event(**changes))
        wrong_repo = event()
        wrong_repo["repository"]["full_name"] = "other/repository"
        with self.assertRaises(observer.ObserverError):
            observer.parse_workflow_run(wrong_repo)

    def test_claim_parser_accepts_only_exact_first_line(self):
        self.assertEqual(observer.parse_claim_marker(claim()["body"]), observer.Claim(ISSUE_NUMBER, RUN_ID, BRANCH))
        self.assertIsNone(observer.parse_claim_marker("claim issue:70 run:700 branch:" + BRANCH))
        self.assertIsNone(observer.parse_claim_marker("<!-- codex-worker-claim issue:70 run:local branch:" + BRANCH + " -->"))

    def test_run_to_issue_resolution_is_exact_and_fails_closed(self):
        client = FakeClient()
        number, comments = observer.resolve_claimed_issue(client, observer.parse_workflow_run(event()))
        self.assertEqual(number, ISSUE_NUMBER)
        self.assertEqual(len(comments), 1)

        no_match = FakeClient(comments={ISSUE_NUMBER: [claim(run_id=RUN_ID + 1)]})
        with self.assertRaises(observer.ObserverError):
            observer.resolve_claimed_issue(no_match, observer.parse_workflow_run(event()))

        duplicate = FakeClient(
            issues=[{"number": ISSUE_NUMBER}, {"number": 71}],
            comments={ISSUE_NUMBER: [claim()], 71: [claim(number=71)]},
        )
        with self.assertRaises(observer.ObserverError):
            observer.resolve_claimed_issue(duplicate, observer.parse_workflow_run(event()))

    def test_untrusted_or_mismatched_claim_cannot_map_an_issue(self):
        unsafe = claim()
        unsafe["user"]["login"] = "maintainer"
        client = FakeClient(comments={ISSUE_NUMBER: [unsafe]})
        with self.assertRaises(observer.ObserverError):
            observer.resolve_claimed_issue(client, observer.parse_workflow_run(event()))
        client = FakeClient(comments={ISSUE_NUMBER: [claim(number=71)]})
        with self.assertRaises(observer.ObserverError):
            observer.resolve_claimed_issue(client, observer.parse_workflow_run(event()))


class ObservationTests(unittest.TestCase):
    def test_key_is_deterministic_and_binds_terminal_conclusion(self):
        run = observer.parse_workflow_run(event())
        self.assertEqual(observer.completion_key(run, ISSUE_NUMBER), observer.completion_key(run, ISSUE_NUMBER))
        self.assertNotEqual(
            observer.completion_key(run, ISSUE_NUMBER),
            observer.completion_key(observer.parse_workflow_run(event("failure")), ISSUE_NUMBER),
        )

    def test_success_records_one_bounded_audit_without_state_mutation(self):
        client = FakeClient()
        result = observer.observe(client, event())
        self.assertEqual(result, "worker-success")
        self.assertEqual(len(client.comment_calls), 1)
        number, body = client.comment_calls[0]
        self.assertEqual(number, ISSUE_NUMBER)
        self.assertLessEqual(len(body), observer.MAX_AUDIT)
        self.assertIn('"conclusion":"success"', body)
        self.assertIn('"created_at":"2026-09-05T00:00:00Z"', body)
        self.assertIn('"head_sha":"' + HEAD + '"', body)
        self.assertIn('"pr_number":null', body)
        self.assertNotIn("status:", body)
        self.assertFalse(hasattr(client, "set_labels"))

    def test_terminal_non_success_records_once_without_retry_or_codex(self):
        client = FakeClient()
        self.assertEqual(observer.observe(client, event("failure")), "worker-terminal-non-success")
        self.assertEqual(observer.observe(client, event("failure")), "worker-terminal-non-success")
        self.assertEqual(len(client.comment_calls), 1)
        source = inspect.getsource(observer).lower()
        self.assertNotIn("subprocess", source)
        self.assertNotIn("auto_merge", source)
        self.assertNotIn('"/merges"', source)

    def test_optional_pr_identity_must_be_exact(self):
        pull = {
            "number": 170,
            "state": "open",
            "base": {"ref": "main"},
            "head": {"ref": BRANCH, "sha": HEAD, "repo": {"full_name": observer.REPOSITORY}},
        }
        client = FakeClient(pulls=[pull])
        observer.observe(client, event())
        self.assertIn('"pr_number":170', client.comment_calls[0][1])
        pull["head"]["sha"] = "b" * 40
        with self.assertRaises(observer.ObserverError):
            observer.observe(FakeClient(pulls=[pull]), event())

    def test_marker_is_deterministic_and_does_not_accept_unbounded_unsafe_content(self):
        run = observer.parse_workflow_run(event())
        marker = observer.completion_marker(run, ISSUE_NUMBER, None)
        self.assertEqual(marker, observer.completion_marker(run, ISSUE_NUMBER, None))
        for forbidden in ("token", "prompt", "stderr", "C:/Users", "\\\\"):
            self.assertNotIn(forbidden, marker.lower())
        malformed = {ISSUE_NUMBER: [claim(), {
            "body": "<!-- a4.18-completion:{unsafe-token} -->",
            "user": {"login": observer.TRUSTED_CLAIM_AUTHOR},
        }]}
        with self.assertRaises(observer.ObserverError):
            observer.observe(FakeClient(comments=malformed), event())


class WorkflowContractTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = (ROOT / ".github" / "workflows" / "codex-green-completion-observer.yml").read_text(encoding="utf-8")

    def test_workflow_is_exact_workflow_run_trusted_main_and_least_privilege(self):
        self.assertIn("workflow_run:", self.workflow)
        self.assertIn('workflows: ["GREEN Codex issue worker"]', self.workflow)
        self.assertIn("types: [completed]", self.workflow)
        self.assertIn("ref: main", self.workflow)
        self.assertIn("persist-credentials: false", self.workflow)
        for permission in ("actions: read", "contents: read", "issues: write", "pull-requests: read"):
            self.assertIn(permission, self.workflow)
        self.assertNotIn("contents: write", self.workflow)
        self.assertNotIn("pull-requests: write", self.workflow)
        self.assertIn("python -m scripts.codex_completion_observer", self.workflow)
        self.assertNotIn("codex exec", self.workflow)
        self.assertNotIn("gh pr merge", self.workflow)
        self.assertNotIn("enablePullRequestAutoMerge", self.workflow)


if __name__ == "__main__":
    unittest.main()
