import copy
import dataclasses
import errno
import io
import inspect
import json
import os
from pathlib import Path
import socket
import ssl
import subprocess
import unittest
import urllib.error
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_repair_worker as repair
from scripts import a5_yellow_repair_worker as yellow_repair
from scripts import a5_review_state as state_contract
from scripts import a5_reviewer as reviewer
from scripts import codex_issue_worker as green_worker
from scripts import yellow_lane_policy as yellow_policy


HEAD = "a" * 40
NEW_HEAD = "b" * 40
BRANCH = "codex/issue-75-a5-green-task"
PROTECTED_BRANCH = "protected/issue-75-a5-yellow-task"
PROTECTED_BASE = "d" * 40
AUTOMATED_YELLOW_TITLE = "A5 automated yellow task"
AUTOMATED_YELLOW_BRANCH = yellow_policy.yellow_branch(75, AUTOMATED_YELLOW_TITLE)
AUTOMATED_YELLOW_BASE = "e" * 40
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
    value = {"number": 175, "state": "open", "title": "Issue #75", "body": "Refs #75",
             "labels": labels(), "base": {"ref": "main", "sha": "c" * 40,
                                              "repo": {"full_name": orchestrator.REPOSITORY}},
             "head": {"sha": HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}}}
    value.update(changes)
    return value


def protected_issue(**changes):
    value = issue(
        title="A5 protected yellow task",
        body=ISSUE_BODY.replace("risk:green", "risk:yellow"),
        labels=labels("status:review", "risk:yellow"),
    )
    value.update(changes)
    return value


def protected_pull_request(**changes):
    value = pull_request(
        base={"ref": "main", "sha": PROTECTED_BASE, "repo": {"full_name": orchestrator.REPOSITORY}},
        head={"sha": HEAD, "ref": PROTECTED_BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}},
    )
    value.update(changes)
    return value


def protected_authorization(**changes):
    value = {
        "base_sha": PROTECTED_BASE,
        "branch": PROTECTED_BRANCH,
        "issue_number": 75,
        "schema_version": 1,
        "scope": "implementation-only",
    }
    value.update(changes)
    return {
        "body": "<!-- protected-implementation-authorization:" + json.dumps(value, sort_keys=True, separators=(",", ":")) + " -->",
        "user": {"login": orchestrator.TRUSTED_MAINTAINER_AUTHOR},
    }


def automated_yellow_issue(**changes):
    value = issue(
        title=AUTOMATED_YELLOW_TITLE,
        body=ISSUE_BODY.replace("risk:green", "risk:yellow"),
        labels=labels("status:review", "risk:yellow"),
    )
    value.update(changes)
    return value


def automated_yellow_pull_request(**changes):
    value = pull_request(
        base={"ref": "main", "sha": AUTOMATED_YELLOW_BASE,
              "repo": {"full_name": orchestrator.REPOSITORY}},
        head={"sha": HEAD, "ref": AUTOMATED_YELLOW_BRANCH,
              "repo": {"full_name": orchestrator.REPOSITORY}},
    )
    value.update(changes)
    return value


def automated_prestart_value(**changes):
    value = {
        "schema_version": 1,
        "repository": orchestrator.REPOSITORY,
        "issue_number": 75,
        "trusted_base_sha": AUTOMATED_YELLOW_BASE,
        "declared_risk": "yellow",
        "effective_risk": "yellow",
        "authorized_paths": ["docs/change.md"],
        "scientific_runtime_prohibited": True,
    }
    value.update(changes)
    return value


def automated_claim_value(**changes):
    value = {
        "schema_version": 1,
        "repository": orchestrator.REPOSITORY,
        "issue_number": 75,
        "trusted_base_sha": AUTOMATED_YELLOW_BASE,
        "branch": AUTOMATED_YELLOW_BRANCH,
        "lane": yellow_policy.AUTOMATED_YELLOW_LANE,
    }
    value.update(changes)
    return value


def automated_evidence(prestart=None, claim=None, author=None):
    prestart_json = (yellow_policy.serialize_prestart(automated_prestart_value())
                     if prestart is None else prestart)
    claim_json = (yellow_policy.serialize_claim(automated_claim_value())
                  if claim is None else claim)
    login = author or orchestrator.TRUSTED_AUDIT_AUTHOR
    return [
        {"body": "<!-- a5.yellow-prestart:" + prestart_json + " -->", "user": {"login": login}},
        {"body": "<!-- a5.yellow-claim:" + claim_json + " -->", "user": {"login": login}},
    ]


def event(conclusion="success", sha=HEAD):
    return {"workflow_run": {"id": 100, "name": "Normal Python CI", "status": "completed",
                              "conclusion": conclusion, "head_sha": sha}}


def clean_verdict():
    return reviewer.ReviewVerdict(1, "clean", HEAD, "green", "clean", (), "")


def blocker_verdict():
    finding = reviewer.Finding("F-1", "tests", "test blocker", "update test", "[AC-1] test passes")
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "green", "blocker", (finding,), "")


def protected_blocker_verdict(findings=None, summary="validated protected blocker"):
    if findings is None:
        findings = (reviewer.Finding(
            "F-1", "policy", "validated finding", "make the protected repair", "[AC-1] validate the policy"
        ),)
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", summary, tuple(findings), "")


class FakeClient:
    def __init__(self, pr=None, linked_issue=None, files=None, issue_comments=None):
        self.pr_data = copy.deepcopy(pr or pull_request())
        self.issue_data = copy.deepcopy(linked_issue or issue())
        self.files_data = copy.deepcopy(files or [{"filename": "docs/change.md", "patch": "+safe"}])
        self.comment_data = []
        self.issue_comment_data = copy.deepcopy(issue_comments or [])
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
        if number == self.pr_data["number"]:
            return self.comment_data
        self.assert_number(number, self.issue_data["number"])
        return self.issue_comment_data

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
        self.assertIn("actions: read", workflow)
        self.assertIn("contents: read", workflow)
        self.assertIn("issues: write", workflow)
        self.assertIn("pull-requests: write", workflow)
        self.assertNotIn("pull-requests: read", workflow)
        self.assertIn("persist-credentials: false", workflow)
        self.assertNotIn("gh pr merge", workflow)
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

    def test_transport_failure_exposes_only_allowlisted_category(self):
        secret = "sentinel-token-transport-C:/Users/private"
        with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=urllib.error.URLError(secret)):
            with self.assertRaises(orchestrator.OrchestrationError) as raised:
                orchestrator.GitHubClient(secret).issue(75)
        self.assertEqual(str(raised.exception), "GitHub get-issue: transport other-transport")
        self.assertNotIn(secret, str(raised.exception))
        self.assertNotIn("C:/Users", str(raised.exception))

    def test_transport_categories_are_deterministic_bounded_and_secret_safe(self):
        secret = "sentinel-token-C:/Users/private?proxy=secret"
        cases = (
            (TimeoutError(secret), "timeout"),
            (socket.gaierror(socket.EAI_NONAME, secret), "dns"),
            (ssl.SSLCertVerificationError(secret), "tls"),
            (OSError("Tunnel connection failed: 407 Proxy " + secret), "proxy"),
            (OSError(errno.ECONNREFUSED, secret), "connection-refused"),
            (OSError(errno.ECONNRESET, secret), "connection-reset"),
            (OSError(errno.ENETUNREACH, secret), "connection-unreachable"),
            (ValueError(secret), "other-transport"),
        )
        for reason, expected in cases:
            failure = urllib.error.URLError(reason)
            self.assertEqual(orchestrator.classify_transport_failure(failure), expected)
            with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=failure):
                with self.assertRaises(orchestrator.OrchestrationError) as raised:
                    orchestrator.GitHubClient(secret).issue(75)
            diagnostic = str(raised.exception)
            self.assertEqual(diagnostic, "GitHub get-issue: transport " + expected)
            self.assertIn(expected, orchestrator.TRANSPORT_CATEGORIES)
            for forbidden in (secret, "C:/Users", "Proxy", "Tunnel", "407"):
                self.assertNotIn(forbidden, diagnostic)

    def test_http_failure_remains_distinct_from_transport_classification(self):
        failure = urllib.error.HTTPError("https://example.invalid", 503, "unused", {}, None)
        with mock.patch.object(orchestrator.urllib.request, "urlopen", side_effect=failure):
            with self.assertRaises(orchestrator.OrchestrationError) as raised:
                orchestrator.GitHubClient("sentinel-token").issue(75)
        self.assertEqual(str(raised.exception), "GitHub get-issue: HTTP 503")

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
        self.assertEqual(orchestrator.canonical_linked_issue(pull_request(body="Closes #75")), 75)
        for body in ("", "Refs #75\nRefs #75", "Refs #75\nCloses #75", "Refs #75 extra",
                     "Closes #75\nFixes #76"):
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.canonical_linked_issue(pull_request(body=body))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_pr_identity(pull_request(head={"sha": HEAD, "ref": BRANCH, "repo": {"full_name": "fork/repo"}}),
                                              orchestrator.parse_workflow_run(event()))
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_issue_identity(issue(labels=labels("status:review", "risk:green")), BRANCH, 75)

    def test_green_identity_path_remains_unchanged(self):
        contract = orchestrator.validate_issue_identity(issue(), BRANCH, 75)
        self.assertEqual(contract.risk, "risk:green")
        self.assertEqual(orchestrator.review_lane(BRANCH), "green")
        self.assertEqual(orchestrator.canonical_linked_issue(pull_request(body="Closes #75")), 75)

    def test_protected_yellow_identity_accepts_one_exact_maintainer_authorization(self):
        pr = protected_pull_request()
        issue_data = protected_issue()
        comments = [protected_authorization()]
        run = orchestrator.parse_workflow_run(event())
        self.assertEqual(orchestrator.validate_pr_identity(pr, run), (175, PROTECTED_BRANCH))
        self.assertEqual(orchestrator.canonical_linked_issue(pr, require_refs=True), 75)
        contract = orchestrator.validate_issue_identity(issue_data, PROTECTED_BRANCH, 75, comments, PROTECTED_BASE)
        self.assertEqual(contract.risk, "risk:yellow")
        self.assertEqual(orchestrator.review_lane(PROTECTED_BRANCH), "protected-yellow")
        snapshot, paths = orchestrator.build_snapshot(
            pr, issue_data, run, [{"filename": "docs/AUTONOMOUS_DEVELOPMENT.md", "patch": "+policy"}], "protected-yellow"
        )
        self.assertEqual((snapshot["declared_risk"], snapshot["trusted_risk_floor"], paths),
                         ("yellow", "yellow", ("docs/AUTONOMOUS_DEVELOPMENT.md",)))

    def test_protected_yellow_identity_rejects_invalid_authorization_and_identity(self):
        valid_issue = protected_issue()
        valid_pr = protected_pull_request()
        valid_comments = [protected_authorization()]
        invalid_cases = (
            (valid_issue, PROTECTED_BRANCH, (), "authorization is missing or ambiguous"),
            (valid_issue, PROTECTED_BRANCH, valid_comments * 2, "authorization is missing or ambiguous"),
            (valid_issue, PROTECTED_BRANCH, [{**protected_authorization(), "user": {"login": "untrusted"}}], "untrusted author"),
            (valid_issue, PROTECTED_BRANCH, [{"body": "<!-- protected-implementation-authorization:{bad} -->", "user": {"login": orchestrator.TRUSTED_MAINTAINER_AUTHOR}}], "marker JSON is malformed"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(schema_version=2)], "unsupported scope"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(scope="runtime")], "unsupported scope"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(issue_number=76)], "issue does not match"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(branch="protected/issue-75-other")], "branch does not match"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(base_sha="e" * 40)], "base SHA does not match"),
            (valid_issue, PROTECTED_BRANCH, [protected_authorization(base_sha="A" * 40)], "base SHA"),
            (protected_issue(labels=labels("status:review", "risk:green")), PROTECTED_BRANCH, valid_comments, "protected YELLOW"),
            (protected_issue(labels=labels("status:unknown", "risk:yellow")), PROTECTED_BRANCH, valid_comments, "unsupported implementation status"),
            (valid_issue, "protected/issue-76-a5-yellow-task", valid_comments, "branch does not match"),
        )
        for issue_data, branch, comments, reason in invalid_cases:
            with self.subTest(reason=reason):
                with self.assertRaisesRegex(orchestrator.OrchestrationError, reason):
                    orchestrator.validate_issue_identity(issue_data, branch, 75, comments, PROTECTED_BASE)
        for body in ("Closes #75", "Refs #75\nRefs #75", "Refs #75\nCloses #75"):
            with self.subTest(body=body):
                with self.assertRaises(orchestrator.OrchestrationError):
                    orchestrator.canonical_linked_issue(protected_pull_request(body=body), require_refs=True)
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_pr_identity(
                protected_pull_request(head={"sha": HEAD, "ref": PROTECTED_BRANCH, "repo": {"full_name": "fork/repo"}}),
                orchestrator.parse_workflow_run(event()),
            )
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.validate_pr_identity(valid_pr, orchestrator.parse_workflow_run(event(sha=NEW_HEAD)))

    def test_automated_yellow_identity_accepts_exact_trusted_policy_evidence(self):
        pr = automated_yellow_pull_request()
        issue_data = automated_yellow_issue()
        evidence = automated_evidence()
        run = orchestrator.parse_workflow_run(event())
        self.assertEqual(orchestrator.validate_pr_identity(pr, run), (175, AUTOMATED_YELLOW_BRANCH))
        self.assertEqual(orchestrator.review_lane(AUTOMATED_YELLOW_BRANCH), "automated-yellow")
        self.assertEqual(orchestrator.canonical_linked_issue(pr, require_refs=True), 75)
        contract = orchestrator.validate_issue_identity(
            issue_data, AUTOMATED_YELLOW_BRANCH, 75, evidence, AUTOMATED_YELLOW_BASE
        )
        self.assertEqual(contract.risk, "risk:yellow")
        snapshot, paths = orchestrator.build_snapshot(
            pr, issue_data, run, [{"filename": "docs/change.md", "patch": "+safe"}],
            "automated-yellow", ("docs/change.md",),
        )
        self.assertEqual((snapshot["declared_risk"], snapshot["trusted_risk_floor"], paths),
                         ("yellow", "yellow", ("docs/change.md",)))

    def test_automated_yellow_requires_one_canonical_trusted_evidence_pair(self):
        valid = automated_evidence()
        noncanonical_prestart = json.dumps(automated_prestart_value(), sort_keys=False)
        malformed_extra = {
            "body": "<!-- a5.yellow-claim:not-json -->",
            "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR},
        }
        oversized_extra = {
            "body": "<!-- a5.yellow-prestart:" + ("x" * orchestrator.MAX_AUDIT) + " -->",
            "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR},
        }
        cases = (
            ((), "missing or ambiguous"),
            ((valid[0],), "missing or ambiguous"),
            ((valid[1],), "missing or ambiguous"),
            (tuple(valid + [valid[0]]), "missing or ambiguous"),
            (tuple(valid + [valid[1]]), "missing or ambiguous"),
            (tuple([valid[0], automated_evidence(prestart=json.dumps(
                automated_prestart_value(authorized_paths=["docs/other.md"]),
                sort_keys=True, separators=(",", ":"),
            ))[0], valid[1]]), "missing or ambiguous"),
            (tuple(automated_evidence(author="untrusted")), "untrusted author"),
            (tuple(automated_evidence(prestart="{bad}")), "invalid"),
            (tuple(automated_evidence(claim="{bad}")), "invalid"),
            (tuple(valid + [malformed_extra]), "invalid"),
            (tuple(valid + [oversized_extra]), "invalid"),
            (tuple([{"body": " " + valid[0]["body"], "user": valid[0]["user"]}, valid[1]]), "invalid"),
            (tuple(automated_evidence(prestart=noncanonical_prestart)), "not canonical"),
        )
        for evidence, reason in cases:
            with self.subTest(reason=reason):
                with self.assertRaisesRegex(orchestrator.OrchestrationError, reason):
                    orchestrator.validate_issue_identity(
                        automated_yellow_issue(), AUTOMATED_YELLOW_BRANCH, 75,
                        evidence, AUTOMATED_YELLOW_BASE,
                    )

    def test_automated_yellow_policy_bindings_fail_closed(self):
        def raw(value):
            return json.dumps(value, sort_keys=True, separators=(",", ":"))

        invalid_pairs = (
            (raw(automated_prestart_value(repository="other/repo")), raw(automated_claim_value())),
            (raw(automated_prestart_value(issue_number=76)), raw(automated_claim_value())),
            (raw(automated_prestart_value(trusted_base_sha="f" * 40)), raw(automated_claim_value())),
            (raw(automated_prestart_value(declared_risk="green")), raw(automated_claim_value())),
            (raw(automated_prestart_value(effective_risk="red")), raw(automated_claim_value())),
            (raw(automated_prestart_value(scientific_runtime_prohibited=False)), raw(automated_claim_value())),
            (raw(automated_prestart_value(extra="unknown")), raw(automated_claim_value())),
            (raw(automated_prestart_value()), raw(automated_claim_value(repository="other/repo"))),
            (raw(automated_prestart_value()), raw(automated_claim_value(issue_number=76))),
            (raw(automated_prestart_value()), raw(automated_claim_value(trusted_base_sha="f" * 40))),
            (raw(automated_prestart_value()), raw(automated_claim_value(branch="codex-yellow/issue-75-other"))),
            (raw(automated_prestart_value()), raw(automated_claim_value(lane="green"))),
            (raw(automated_prestart_value()), raw(automated_claim_value(extra="unknown"))),
        )
        for prestart, claim in invalid_pairs:
            with self.subTest(prestart=prestart, claim=claim):
                with self.assertRaisesRegex(orchestrator.OrchestrationError, "invalid"):
                    orchestrator.validate_issue_identity(
                        automated_yellow_issue(), AUTOMATED_YELLOW_BRANCH, 75,
                        automated_evidence(prestart, claim), AUTOMATED_YELLOW_BASE,
                    )

    def test_automated_yellow_identity_rejects_wrong_issue_link_state_risk_and_dependency(self):
        evidence = automated_evidence()
        for body in ("Closes #75", "Refs #75\nRefs #75", "Refs #75\nCloses #75"):
            with self.subTest(body=body):
                with self.assertRaises(orchestrator.OrchestrationError):
                    issue_number = orchestrator.canonical_linked_issue(
                        automated_yellow_pull_request(body=body), require_refs=True
                    )
                    self.assertEqual(issue_number, 75)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "branch does not match"):
            orchestrator.validate_issue_identity(
                automated_yellow_issue(number=76), AUTOMATED_YELLOW_BRANCH, 76,
                evidence, AUTOMATED_YELLOW_BASE,
            )
        invalid_issues = (
            automated_yellow_issue(state="closed"),
            automated_yellow_issue(labels=labels("status:review")),
            automated_yellow_issue(labels=labels("status:ready", "status:review", "risk:yellow")),
            automated_yellow_issue(labels=labels("status:unknown", "risk:yellow")),
            automated_yellow_issue(labels=labels("status:review", "risk:green")),
            automated_yellow_issue(labels=labels("status:review", "risk:yellow", "risk:red")),
            automated_yellow_issue(body=ISSUE_BODY),
        )
        for issue_data in invalid_issues:
            with self.subTest(issue=issue_data):
                with self.assertRaises(orchestrator.OrchestrationError):
                    orchestrator.validate_issue_identity(
                        issue_data, AUTOMATED_YELLOW_BRANCH, 75, evidence, AUTOMATED_YELLOW_BASE
                    )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "branch does not match"):
            orchestrator.validate_issue_identity(
                automated_yellow_issue(), "codex-yellow/issue-76-other", 75, evidence, AUTOMATED_YELLOW_BASE
            )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "deterministic issue branch"):
            orchestrator.validate_issue_identity(
                automated_yellow_issue(), "codex-yellow/issue-75-other", 75,
                evidence, AUTOMATED_YELLOW_BASE,
            )
        for pr, run in (
            (automated_yellow_pull_request(head={"sha": HEAD, "ref": AUTOMATED_YELLOW_BRANCH,
                                                "repo": {"full_name": "fork/repo"}}),
             orchestrator.parse_workflow_run(event())),
            (automated_yellow_pull_request(), orchestrator.parse_workflow_run(event(sha=NEW_HEAD))),
        ):
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.validate_pr_identity(pr, run)
        wrong_base_client = FakeClient(
            pr=automated_yellow_pull_request(base={
                "ref": "main", "sha": "f" * 40,
                "repo": {"full_name": orchestrator.REPOSITORY},
            }),
            linked_issue=automated_yellow_issue(), issue_comments=evidence,
        )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "authorization is invalid"):
            orchestrator.orchestrate(wrong_base_client, event(), ".", mock.Mock())
        dependent = automated_yellow_issue(body=automated_yellow_issue()["body"].replace("- none", "- blocked-by: #1"))
        contract = orchestrator.validate_issue_identity(
            dependent, AUTOMATED_YELLOW_BRANCH, 75, evidence, AUTOMATED_YELLOW_BASE
        )
        client = FakeClient(pr=automated_yellow_pull_request(), linked_issue=dependent, issue_comments=evidence)
        client.dependency_issue = lambda _: {"state": "open"}
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "dependency"):
            orchestrator.validate_dependencies(client, contract)

    def test_manual_and_automated_yellow_authorizations_are_not_interchangeable(self):
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "protected implementation authorization"):
            orchestrator.validate_issue_identity(
                protected_issue(), PROTECTED_BRANCH, 75, automated_evidence(), PROTECTED_BASE
            )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "automated YELLOW authorization"):
            orchestrator.validate_issue_identity(
                automated_yellow_issue(), AUTOMATED_YELLOW_BRANCH, 75,
                [protected_authorization()], AUTOMATED_YELLOW_BASE,
            )
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "eligible GREEN"):
            orchestrator.validate_issue_identity(
                automated_yellow_issue(), BRANCH, 75, automated_evidence(), AUTOMATED_YELLOW_BASE
            )
        with self.assertRaises(orchestrator.OrchestrationError):
            orchestrator.review_lane("codex-yellow/issue-75-Bad")

    def test_protected_yellow_dependency_failure_fails_closed(self):
        contract = orchestrator.validate_issue_identity(
            protected_issue(), PROTECTED_BRANCH, 75, [protected_authorization()], PROTECTED_BASE
        )
        client = FakeClient(pr=protected_pull_request(), linked_issue=protected_issue(), issue_comments=[protected_authorization()])
        client.dependency_issue = lambda _: {"state": "open"}
        contract = dataclasses.replace(contract, dependencies=(green_worker.Dependency(orchestrator.REPOSITORY, 1, "#1"),))
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "dependency"):
            orchestrator.validate_dependencies(client, contract)

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


class AuthorizationFingerprintTests(unittest.TestCase):
    def _fingerprint(self, pr=None, issue_data=None, comments=(), client=None, run=None):
        client = client or FakeClient(pr=pr, linked_issue=issue_data, issue_comments=comments)
        pr = pr or client.pr_data
        issue_data = issue_data or client.issue_data
        return orchestrator.authorization_fingerprint(
            client, pr, issue_data, run or orchestrator.parse_workflow_run(event()), comments
        )

    def test_irrelevant_nested_rest_metadata_does_not_change_green_or_protected_fingerprint(self):
        cases = (
            (pull_request(), issue(), (), "green"),
            (protected_pull_request(), protected_issue(), (protected_authorization(),), "protected-yellow"),
            (automated_yellow_pull_request(), automated_yellow_issue(), tuple(automated_evidence()), "automated-yellow"),
        )
        for pr, issue_data, comments, lane in cases:
            with self.subTest(lane=lane):
                changed = copy.deepcopy(pr)
                changed["base"]["repo"].update({
                    "url": "https://example.invalid/base", "watchers_count": 999,
                    "updated_at": "2099-01-01T00:00:00Z",
                })
                changed["head"]["repo"].update({
                    "url": "https://example.invalid/head", "forks_count": 999,
                    "permissions": {"admin": True},
                })
                self.assertEqual(
                    self._fingerprint(pr, issue_data, comments),
                    self._fingerprint(changed, issue_data, comments),
                )

    def test_automated_yellow_fingerprint_is_deterministic_and_binds_both_evidence_records(self):
        pr, issue_data, evidence = (
            automated_yellow_pull_request(), automated_yellow_issue(), tuple(automated_evidence())
        )
        baseline = self._fingerprint(pr, issue_data, evidence)
        self.assertEqual(baseline, self._fingerprint(copy.deepcopy(pr), copy.deepcopy(issue_data), evidence))
        changed_prestart = json.dumps(
            automated_prestart_value(authorized_paths=["docs/other.md"]),
            sort_keys=True, separators=(",", ":"),
        )
        changed_evidence = tuple(automated_evidence(prestart=changed_prestart))
        self.assertNotEqual(baseline, self._fingerprint(pr, issue_data, changed_evidence))
        for invalid in (
            tuple(automated_evidence(claim=json.dumps(
                automated_claim_value(trusted_base_sha="f" * 40), sort_keys=True, separators=(",", ":")
            ))),
            tuple(automated_evidence(author="untrusted")),
        ):
            with self.assertRaises(orchestrator.OrchestrationError):
                self._fingerprint(pr, issue_data, invalid)

    def test_relevant_green_authorization_fields_are_binding(self):
        original_pr, original_issue = pull_request(), issue()
        baseline = self._fingerprint(original_pr, original_issue)
        changes = (
            ("head SHA", lambda pr, issue_data, run: (pr["head"].__setitem__("sha", NEW_HEAD),
                                                        dataclasses.replace(run, head_sha=NEW_HEAD))[1], True),
            ("head branch", lambda pr, issue_data, run: pr["head"].__setitem__("ref", "codex/issue-75-other"), False),
            ("head repository", lambda pr, issue_data, run: pr["head"].__setitem__("repo", {"full_name": "fork/repo"}), False),
            ("base SHA", lambda pr, issue_data, run: pr["base"].__setitem__("sha", "d" * 40), True),
            ("base ref", lambda pr, issue_data, run: pr["base"].__setitem__("ref", "release"), False),
            ("base repository", lambda pr, issue_data, run: pr["base"].__setitem__("repo", {"full_name": "fork/repo"}), False),
            ("PR title", lambda pr, issue_data, run: pr.__setitem__("title", "Changed title"), True),
            ("PR body", lambda pr, issue_data, run: pr.__setitem__("body", "Refs #75\nChanged"), True),
            ("PR labels", lambda pr, issue_data, run: pr.__setitem__("labels", labels("changed")), True),
            ("issue title", lambda pr, issue_data, run: issue_data.__setitem__("title", "Changed issue"), False),
            ("issue body", lambda pr, issue_data, run: issue_data.__setitem__("body", ISSUE_BODY.replace("Test.", "Changed.", 1)), True),
            ("issue labels", lambda pr, issue_data, run: issue_data.__setitem__("labels", labels("status:review", "risk:green", "agent:codex", "changed")), True),
        )
        for name, change, changes_fingerprint in changes:
            with self.subTest(name=name):
                pr, issue_data, run = copy.deepcopy(original_pr), copy.deepcopy(original_issue), orchestrator.parse_workflow_run(event())
                run = change(pr, issue_data, run) or run
                try:
                    observed = self._fingerprint(pr, issue_data, run=run)
                except orchestrator.OrchestrationError:
                    self.assertFalse(changes_fingerprint)
                else:
                    self.assertTrue(changes_fingerprint)
                    self.assertNotEqual(observed, baseline)

    def test_dependency_state_and_protected_authorization_changes_fail_closed(self):
        dependent = issue(body=ISSUE_BODY.replace("- none", "- blocked-by: #1"))
        client = FakeClient(linked_issue=dependent)
        client.dependency_issue = lambda _: {"state": "open"}
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "dependency"):
            self._fingerprint(issue_data=dependent, client=client)

        protected_pr, protected_issue_data = protected_pull_request(), protected_issue()
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "unsupported scope"):
            self._fingerprint(protected_pr, protected_issue_data, (protected_authorization(scope="runtime"),))

    def test_malformed_repository_ref_sha_and_protected_authorization_fail_closed(self):
        malformed = (
            (pull_request(base={"ref": "main", "sha": "c" * 40}), issue(), ()),
            (pull_request(head={"sha": HEAD, "ref": BRANCH}), issue(), ()),
            (pull_request(base={"ref": None, "sha": "c" * 40, "repo": {"full_name": orchestrator.REPOSITORY}}), issue(), ()),
            (pull_request(base={"ref": "main", "sha": "BAD", "repo": {"full_name": orchestrator.REPOSITORY}}), issue(), ()),
            (protected_pull_request(), protected_issue(), ()),
        )
        for pr, issue_data, comments in malformed:
            with self.subTest(pr=pr):
                with self.assertRaises(orchestrator.OrchestrationError):
                    self._fingerprint(pr, issue_data, comments)

    def test_post_review_refetch_rejects_changed_exact_review_state(self):
        client = FakeClient()
        current = orchestrator.CurrentReviewState("status:review", "review:pending", HEAD)
        accepted = orchestrator._state_input(175, 75, HEAD, current, "verdict", clean_verdict())
        plan = state_contract.transition(accepted)
        client.pr_data["labels"] = labels("review:clean")
        client.comment_data.append({
            "body": orchestrator._audit_body(accepted, plan),
            "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR},
        })
        baseline_client = FakeClient()
        authorization = self._fingerprint(client=baseline_client)
        with mock.patch.object(orchestrator, "authorization_fingerprint", return_value=authorization):
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "review state changed"):
                orchestrator._refetch_unchanged(
                    client, 175, 75, HEAD, orchestrator.parse_workflow_run(event()), current, authorization
                )


class StateAndRepairTests(unittest.TestCase):
    def test_initial_clean_transition_uses_exclusive_labels_and_audit(self):
        client = FakeClient()
        self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: clean_verdict()), "review-clean")
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:clean"])
        self.assertEqual([item["name"] for item in client.issue_data["labels"] if item["name"].startswith("status:")], ["status:review"])
        self.assertEqual(len(client.comment_data), 2)
        self.assertIn("a5.4a-state", client.comment_data[-1]["body"])

    def test_protected_yellow_valid_authorization_can_complete_review_without_repair(self):
        client = FakeClient(
            pr=protected_pull_request(),
            linked_issue=protected_issue(),
            files=[{"filename": "docs/AUTONOMOUS_DEVELOPMENT.md", "patch": "+policy"}],
            issue_comments=[protected_authorization()],
        )
        verdict = reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")
        self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: verdict), "review-clean")
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:clean"])
        self.assertEqual(
            [item["name"] for item in client.issue_data["labels"] if item["name"].startswith("status:")],
            ["status:review"],
        )

    def test_automated_yellow_valid_authorization_can_complete_clean_review(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        verdict = reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")
        self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: verdict), "review-clean")
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:clean"])
        self.assertEqual(
            [item["name"] for item in client.issue_data["labels"] if item["name"].startswith("status:")],
            ["status:review"],
        )

    def test_automated_yellow_exact_or_subset_authorized_paths_reach_review(self):
        cases = (
            (["docs/change.md"], [{"filename": "docs/change.md", "patch": "+safe"}]),
            (["docs/change.md", "tests/test_safe.py"], [{"filename": "docs/change.md", "patch": "+safe"}]),
        )
        for authorized, files in cases:
            with self.subTest(authorized=authorized, files=files):
                prestart = json.dumps(
                    automated_prestart_value(authorized_paths=authorized),
                    sort_keys=True, separators=(",", ":"),
                )
                client = FakeClient(
                    pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
                    files=files, issue_comments=automated_evidence(prestart=prestart),
                )
                review_call = mock.Mock(return_value=reviewer.ReviewVerdict(
                    1, "clean", HEAD, "yellow", "clean", (), ""
                ))
                self.assertEqual(orchestrator.orchestrate(client, event(), ".", review_call), "review-clean")
                review_call.assert_called_once()

    def test_automated_yellow_unauthorized_or_duplicate_paths_fail_before_review_and_repair(self):
        cases = (
            [{"filename": "tests/unauthorized.py", "patch": "+unsafe"}],
            [
                {"filename": "docs/change.md", "patch": "+safe"},
                {"filename": "tests/unauthorized.py", "patch": "+unsafe"},
            ],
            [
                {"filename": "docs/change.md", "patch": "+safe"},
                {"filename": "docs/change.md", "patch": "+duplicate"},
            ],
        )
        for files in cases:
            with self.subTest(files=files):
                client = FakeClient(
                    pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
                    files=files, issue_comments=automated_evidence(),
                )
                review_call = mock.Mock()
                with mock.patch.object(orchestrator, "_repair") as repair_call:
                    with self.assertRaisesRegex(orchestrator.OrchestrationError, "authorized scope"):
                        orchestrator.orchestrate(client, event(), ".", review_call)
                review_call.assert_not_called()
                repair_call.assert_not_called()
                self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)

    def test_automated_yellow_new_head_cannot_gain_review_with_added_unauthorized_path(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(
                head={"sha": NEW_HEAD, "ref": AUTOMATED_YELLOW_BRANCH,
                      "repo": {"full_name": orchestrator.REPOSITORY}},
            ),
            linked_issue=automated_yellow_issue(),
            files=[
                {"filename": "docs/change.md", "patch": "+safe"},
                {"filename": "tests/unauthorized.py", "patch": "+unsafe"},
            ],
            issue_comments=automated_evidence(),
        )
        review_call = mock.Mock()
        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "authorized scope"):
                orchestrator.orchestrate(client, event(sha=NEW_HEAD), ".", review_call)
        review_call.assert_not_called()
        repair_call.assert_not_called()

    def test_automated_yellow_changed_paths_after_review_fail_before_verdict_or_repair(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )

        def add_unauthorized_path(*_):
            client.files_data.append({"filename": "tests/unauthorized.py", "patch": "+unsafe"})
            return reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")

        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "authorized scope"):
                orchestrator.orchestrate(client, event(), ".", add_unauthorized_path)
        repair_call.assert_not_called()
        self.assertNotIn("review:clean", [item["name"] for item in client.pr_data["labels"]])

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

    def test_protected_yellow_authorization_never_invokes_green_repair_or_runtime(self):
        client = FakeClient(
            pr=protected_pull_request(),
            linked_issue=protected_issue(),
            files=[{"filename": "docs/AUTONOMOUS_DEVELOPMENT.md", "patch": "+policy"}],
            issue_comments=[protected_authorization()],
        )
        verdict = reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", "blocked", (reviewer.Finding(
            "F-1", "tests", "blocked", "update", "[AC-1] test passes"),), "")
        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "protected YELLOW review"):
                orchestrator.orchestrate(client, event(), ".", lambda *_: verdict)
        repair_call.assert_not_called()
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "protected YELLOW review"):
            orchestrator._repair(
                client, client.pr_data, client.issue_data, (),
                orchestrator.CurrentReviewState("status:in-progress", "review:blocker", HEAD), verdict,
                "a5.2:" + "a" * 64, ("docs/AUTONOMOUS_DEVELOPMENT.md",), ".",
            )

    def test_automated_yellow_blocker_persists_audit_before_distinct_repair(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        verdict = protected_blocker_verdict()
        def yellow_call(_client, _pr, _issue, comments, *_args):
            self.assertTrue(any("a5.4b-protected-blocker" in item["body"] for item in comments))
            return "repair-pushed"
        with mock.patch.object(orchestrator, "_repair") as green_call, \
                mock.patch.object(orchestrator, "_yellow_repair", side_effect=yellow_call) as yellow_call_mock:
            self.assertEqual(orchestrator.orchestrate(client, event(), ".", lambda *_: verdict), "repair-pushed")
        green_call.assert_not_called()
        yellow_call_mock.assert_called_once()
        blocker_markers = [
            item["body"] for item in client.comment_data if "a5.4b-protected-blocker" in item["body"]
        ]
        self.assertEqual(len(blocker_markers), 1)

    def test_automated_yellow_repair_binds_authority_pushes_same_pr_and_returns_pending(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        captured = {}
        def execute(request, _cwd):
            captured["request"] = request
            client.pr_data["head"]["sha"] = NEW_HEAD
            return yellow_repair.YellowRepairResult(
                1, request.repository, request.pull_request_number, request.issue_number,
                request.branch, request.attempt_number, request.expected_head_sha, NEW_HEAD,
                tuple(item.finding_id for item in request.accepted_findings),
                ("docs/change.md",), "passed", "a5.yellow-repair:" + "f" * 64,
            )
        with mock.patch.object(orchestrator, "checkout_exact_pr_branch"), \
                mock.patch.object(yellow_repair, "execute_repair", side_effect=execute):
            result = orchestrator.orchestrate(
                client, event(), ".", lambda *_: protected_blocker_verdict()
            )
        self.assertEqual(result, "repair-pushed")
        request = captured["request"]
        self.assertEqual(request.branch, AUTOMATED_YELLOW_BRANCH)
        self.assertEqual(request.expected_head_sha, HEAD)
        self.assertEqual(request.authorized_paths, ("docs/change.md",))
        self.assertRegex(request.authorization_key, yellow_repair.EVIDENCE_KEY_RE)
        self.assertRegex(request.blocker_evidence_key, yellow_repair.EVIDENCE_KEY_RE)
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 1)
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:pending"])
        self.assertEqual([item["name"] for item in client.issue_data["labels"] if item["name"].startswith("status:")],
                         ["status:review"])

    def test_automated_yellow_external_finding_persists_evidence_but_consumes_no_attempt(self):
        body = automated_yellow_issue()["body"].replace(
            "## Acceptance criteria\nTest.",
            "## Acceptance criteria\nHosted Normal Python CI passes on the exact PR head.",
        )
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(body=body),
            issue_comments=automated_evidence(),
        )
        verdict = protected_blocker_verdict((reviewer.Finding(
            "F-1", "ci", "CI evidence absent", "wait for CI", "[AC-1] hosted CI passes"
        ),))
        with mock.patch.object(orchestrator, "_yellow_repair") as yellow_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair boundary"):
                orchestrator.orchestrate(client, event(), ".", lambda *_: verdict)
        yellow_call.assert_not_called()
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)
        self.assertEqual(sum("a5.4b-protected-blocker" in item["body"] for item in client.comment_data), 1)

    def test_automated_yellow_tampered_blocker_evidence_fails_before_state_or_attempt(self):
        class TamperingClient(FakeClient):
            def comment(self, number, body):
                if "a5.4b-protected-blocker" in body:
                    body = body.replace("validated finding", "conflicting finding")
                super().comment(number, body)

        client = TamperingClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        with mock.patch.object(orchestrator, "_yellow_repair") as yellow_call:
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.orchestrate(client, event(), ".", lambda *_: protected_blocker_verdict())
        yellow_call.assert_not_called()
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)
        self.assertEqual([item["name"] for item in client.pr_data["labels"]], ["review:pending"])

    def test_automated_yellow_stale_authorization_after_evidence_consumes_no_attempt(self):
        class RacingClient(FakeClient):
            def comment(self, number, body):
                super().comment(number, body)
                if "a5.4b-protected-blocker" in body:
                    changed = json.dumps(
                        automated_prestart_value(authorized_paths=["docs/other.md"]),
                        sort_keys=True, separators=(",", ":"),
                    )
                    self.issue_comment_data = automated_evidence(prestart=changed)

        client = RacingClient(
            pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )
        with mock.patch.object(orchestrator, "_yellow_repair") as yellow_call:
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.orchestrate(client, event(), ".", lambda *_: protected_blocker_verdict())
        yellow_call.assert_not_called()
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)

    def test_automated_yellow_authorization_race_blocks_transition_and_repair(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(),
            issue_comments=automated_evidence(),
        )

        def changed_authorization(*_):
            changed_claim = json.dumps(
                automated_claim_value(trusted_base_sha="f" * 40), sort_keys=True, separators=(",", ":")
            )
            client.issue_comment_data = automated_evidence(claim=changed_claim)
            return reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")

        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "authorization is invalid"):
                orchestrator.orchestrate(client, event(), ".", changed_authorization)
        repair_call.assert_not_called()
        self.assertNotIn("review:clean", [item["name"] for item in client.pr_data["labels"]])

    def test_automated_yellow_prestart_and_head_races_fail_closed_after_review(self):
        for race in ("prestart", "head"):
            with self.subTest(race=race):
                client = FakeClient(
                    pr=automated_yellow_pull_request(),
                    linked_issue=automated_yellow_issue(),
                    issue_comments=automated_evidence(),
                )

                def changed_authorization(*_):
                    if race == "head":
                        client.pr_data["head"]["sha"] = NEW_HEAD
                    else:
                        changed = json.dumps(
                            automated_prestart_value(authorized_paths=["docs/other.md"]),
                            sort_keys=True, separators=(",", ":"),
                        )
                        client.issue_comment_data = automated_evidence(prestart=changed)
                    return reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")

                with mock.patch.object(orchestrator, "_repair") as repair_call:
                    with self.assertRaises(orchestrator.OrchestrationError):
                        orchestrator.orchestrate(client, event(), ".", changed_authorization)
                repair_call.assert_not_called()
                self.assertNotIn("review:clean", [item["name"] for item in client.pr_data["labels"]])

    def test_automated_yellow_escalation_and_invalid_blocker_risk_fail_closed(self):
        def client():
            return FakeClient(
                pr=automated_yellow_pull_request(), linked_issue=automated_yellow_issue(),
                issue_comments=automated_evidence(),
            )

        escalated = reviewer.ReviewVerdict(
            1, "escalate", HEAD, "red", "scientific ambiguity", (), "controlled validation required"
        )
        escalation_client = client()
        self.assertEqual(
            orchestrator.orchestrate(escalation_client, event(), ".", lambda *_: escalated),
            "review-escalated",
        )
        self.assertIn("review:escalated", [item["name"] for item in escalation_client.pr_data["labels"]])

        wrong_risk = reviewer.ReviewVerdict(
            1, "blocker", HEAD, "green", "wrong floor", (reviewer.Finding(
                "F-1", "scope", "risk floor lost", "stop", "retain YELLOW risk"
            ),), ""
        )
        blocked_client = client()
        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.orchestrate(blocked_client, event(), ".", lambda *_: wrong_risk)
        repair_call.assert_not_called()
        self.assertFalse(any("a5.4b-protected-blocker" in item["body"] for item in blocked_client.comment_data))

    def test_protected_authorization_race_blocks_verdict_transition(self):
        client = FakeClient(
            pr=protected_pull_request(),
            linked_issue=protected_issue(),
            files=[{"filename": "docs/AUTONOMOUS_DEVELOPMENT.md", "patch": "+policy"}],
            issue_comments=[protected_authorization()],
        )
        def changed_authorization(*_):
            client.issue_comment_data = [protected_authorization(base_sha="e" * 40)]
            return reviewer.ReviewVerdict(1, "clean", HEAD, "yellow", "clean", (), "")
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "base SHA does not match"):
            orchestrator.orchestrate(client, event(), ".", changed_authorization)
        self.assertNotIn("review:clean", [item["name"] for item in client.pr_data["labels"]])

    def test_external_only_finding_is_rejected_before_a53(self):
        external_body = ISSUE_BODY.replace(
            "## Acceptance criteria\nTest.",
            "## Acceptance criteria\n- [ ] The fixture file contains exactly two lines.\n- [ ] Issue labels/status are observed on GitHub.",
        )
        client = FakeClient(linked_issue=issue(body=external_body))
        external = reviewer.ReviewVerdict(1, "blocker", HEAD, "green", "pending", (reviewer.Finding(
            "F-1", "evidence", "External evidence is pending.", "Write it into a fixture.",
            "[AC-2] GitHub-side observation.",
        ),), "")
        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaises(orchestrator.OrchestrationError):
                orchestrator.orchestrate(client, event(), ".", lambda *_: external)
        repair_call.assert_not_called()

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


class ProtectedBlockerEvidenceTests(unittest.TestCase):
    def _client(self):
        return FakeClient(
            pr=protected_pull_request(),
            linked_issue=protected_issue(),
            files=[{"filename": "docs/AUTONOMOUS_DEVELOPMENT.md", "patch": "+policy"}],
            issue_comments=[protected_authorization()],
        )

    def test_validated_protected_blocker_is_persisted_before_fail_closed_stop(self):
        client = self._client()
        verdict = protected_blocker_verdict(summary="RAW REVIEWER OUTPUT MUST NOT PERSIST")
        with mock.patch.object(orchestrator, "_repair") as repair_call:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "protected YELLOW review"):
                orchestrator.orchestrate(client, event(), ".", lambda *_: verdict)
        repair_call.assert_not_called()
        markers = orchestrator._protected_blocker_markers(client.comment_data)
        self.assertEqual(len(markers), 1)
        marker = markers[0]
        self.assertEqual(
            {"schema_version", "repository", "issue_number", "pr_number", "reviewed_head_sha", "effective_risk", "verdict", "findings"},
            set(marker),
        )
        self.assertEqual(
            (marker["repository"], marker["issue_number"], marker["pr_number"], marker["reviewed_head_sha"], marker["effective_risk"], marker["verdict"]),
            (orchestrator.REPOSITORY, 75, 175, HEAD, "yellow", "blocker"),
        )
        self.assertEqual(marker["findings"], [{
            "id": "F-1", "message": "validated finding", "required_action": "make the protected repair",
            "required_evidence": "[AC-1] validate the policy",
        }])
        self.assertNotIn("RAW REVIEWER OUTPUT", client.comment_data[-1]["body"])
        comment_count = len(client.comment_data)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "protected YELLOW review"):
            orchestrator.orchestrate(client, event(), ".", lambda *_: verdict)
        self.assertEqual(len(client.comment_data), comment_count)

    def test_blocker_evidence_is_deterministic_idempotent_and_supports_bounded_multiple_findings(self):
        client = self._client()
        findings = (
            reviewer.Finding("F-1", "policy", "first", "first action", "[AC-1] first"),
            reviewer.Finding("F-2", "tests", "second", "second action", "[AC-2] second"),
        )
        verdict = protected_blocker_verdict(findings)
        orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict)
        marker = client.comment_data[-1]["body"]
        self.assertEqual(marker, orchestrator._protected_blocker_marker(orchestrator._protected_blocker_markers(client.comment_data)[0]))
        orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict)
        self.assertEqual([item["body"] for item in client.comment_data].count(marker), 1)

    def test_conflicting_malformed_or_stale_protected_evidence_fails_closed(self):
        client = self._client()
        verdict = protected_blocker_verdict()
        orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict)
        conflicting = protected_blocker_verdict((reviewer.Finding(
            "F-1", "policy", "different", "make the protected repair", "[AC-1] validate the policy"
        ),))
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "conflicts"):
            orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, HEAD, conflicting)
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "strictly validated"):
            orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, NEW_HEAD, verdict)
        client.comment_data.append({"body": "<!-- a5.4b-protected-blocker:{bad} -->", "user": {"login": orchestrator.TRUSTED_AUDIT_AUTHOR}})
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "audit marker JSON is malformed"):
            orchestrator.persist_protected_blocker_evidence(client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict)

    def test_unvalidated_secrets_and_absolute_paths_never_reach_blocker_evidence(self):
        client = self._client()
        unsafe = (
            reviewer.ReviewVerdict(2, "blocker", HEAD, "yellow", "", (), ""),
            protected_blocker_verdict((reviewer.Finding(
                "F-1", "policy", "token=ghp_abcdefghijk", "C:/Users/private/action", "[AC-1] /home/private"
            ),)),
        )
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": "app-secret-value"}, clear=False):
            environment_secret = protected_blocker_verdict((reviewer.Finding(
                "F-1", "policy", "app-secret-value", "action", "[AC-1] evidence"
            ),))
            for verdict in unsafe + (environment_secret,):
                with self.subTest(verdict=verdict.schema_version):
                    with self.assertRaises(orchestrator.OrchestrationError):
                        orchestrator.persist_protected_blocker_evidence(
                            client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict
                        )
        self.assertEqual(client.comment_data, [])

    def test_reviewer_prompt_markers_never_reach_blocker_evidence(self):
        fields = ("message", "required_action", "required_evidence")
        for field in fields:
            with self.subTest(field=field):
                client = self._client()
                finding = {
                    "message": "A bounded finding message.",
                    "required_action": "Make the bounded repair.",
                    "required_evidence": "Run the bounded regression.",
                }
                finding[field] = reviewer.REVIEWER_PROMPT_MARKERS[0]
                verdict = protected_blocker_verdict((reviewer.Finding(
                    "F-1", "policy", finding["message"], finding["required_action"],
                    finding["required_evidence"],
                ),))
                with self.assertRaisesRegex(orchestrator.OrchestrationError, "unsafe finding field"):
                    orchestrator.persist_protected_blocker_evidence(
                        client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict
                    )
                self.assertFalse(any("a5.4b-protected-blocker" in comment.get("body", "")
                                     for comment in client.comment_data))

    def test_oversized_valid_fields_fail_closed_without_writing_evidence(self):
        client = self._client()
        text = "x" * 1000
        verdict = protected_blocker_verdict((
            reviewer.Finding("F-1", "policy", text, text, text),
            reviewer.Finding("F-2", "tests", text, text, text),
        ))
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "oversized"):
            orchestrator.persist_protected_blocker_evidence(
                client, client.comment_data, client.pr_data, client.issue_data, HEAD, verdict
            )
        self.assertEqual(client.comment_data, [])


class BoundaryTests(unittest.TestCase):
    def test_live_a5_files_are_denied_to_green_worker(self):
        allowed, denied = green_worker.green_changed_paths(("scripts/a5_reviewer.py", "scripts/a5_review_state.py",
                                                             "scripts/a5_repair_worker.py", "scripts/a5_review_orchestrator.py"))
        self.assertEqual(allowed, ())
        self.assertEqual(denied, ("scripts/a5_reviewer.py", "scripts/a5_review_state.py", "scripts/a5_repair_worker.py",
                                  "scripts/a5_review_orchestrator.py"))

    def test_yellow_repair_control_plane_is_protected_from_green_and_green_repair(self):
        path = "scripts/a5_yellow_repair_worker.py"
        allowed, denied = green_worker.green_changed_paths((path,))
        self.assertEqual((allowed, denied), ((), (path,)))
        self.assertTrue(repair.is_protected_path(path))

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
