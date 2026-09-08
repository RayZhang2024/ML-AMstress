import copy
import os
import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer


HEAD = "a" * 40
BASE = "b" * 40
BRANCH = "protected/issue-187-credential-bootstrap"


def pull_request(**changes):
    value = {
        "number": 187,
        "title": "Credential bootstrap",
        "body": "Refs #187",
        "base": {"sha": BASE, "repo": {"full_name": orchestrator.REPOSITORY}},
        "head": {"sha": HEAD, "ref": BRANCH, "repo": {"full_name": orchestrator.REPOSITORY}},
    }
    value.update(changes)
    return value


def issue(**changes):
    value = {
        "number": 187,
        "title": "Credential bootstrap",
        "body": "## Acceptance criteria\n- [ ] Fixture remains safe.\n",
        "labels": [{"name": "risk:yellow"}, {"name": "status:review"}],
    }
    value.update(changes)
    return value


def runtime_pattern_token():
    return "gh" + "p_" + "abcdefgh"


def runtime_app_token():
    return "runtime" + "-app" + "-value"


class PatchCredentialBootstrapTests(unittest.TestCase):
    def build(self, patch, pr=None, linked_issue=None):
        return orchestrator.build_snapshot(
            pull_request() if pr is None else pr,
            issue() if linked_issue is None else linked_issue,
            orchestrator.WorkflowRun(187, HEAD, "success"),
            [{"filename": "docs/change.md", "patch": patch}], "protected-yellow",
        )

    def test_benign_patch_is_byte_for_byte_identical(self):
        patch = "diff --git a/docs/change.md b/docs/change.md\n@@ -1 +1 @@\n-old\n+new\n"
        snapshot, _ = self.build(patch)
        self.assertEqual(snapshot["changed_files"][0]["patch"], patch)

    def test_pattern_credential_on_added_line_fails_closed(self):
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "adds credential"):
            self.build("+value = " + runtime_pattern_token() + "\n")

    def test_exact_app_credential_on_added_line_fails_closed(self):
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": runtime_app_token()}, clear=False):
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "adds credential"):
                self.build("+value = " + runtime_app_token() + "\n")

    def test_context_deletion_and_metadata_are_redacted_before_prompt(self):
        token = runtime_app_token()
        patch = (
            "diff --git a/docs/change.md b/docs/change.md\n"
            "--- a/docs/" + token + "\n"
            "+++ b/docs/" + token + "\n"
            "@@ -1,2 +1,2 @@\n"
            " context " + token + "\n"
            "-old " + token + "\n"
            "+safe\n"
        )
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": token}, clear=False):
            snapshot, _ = self.build(patch)
            sanitized = snapshot["changed_files"][0]["patch"]
            prompt = reviewer.build_prompt(reviewer.validate_snapshot(snapshot))
        self.assertNotIn(token, sanitized)
        self.assertNotIn(token, prompt)
        self.assertEqual(sanitized.count(orchestrator.CREDENTIAL_PLACEHOLDER), 4)
        self.assertIn("+++ b/docs/" + orchestrator.CREDENTIAL_PLACEHOLDER, sanitized)
        self.assertIn("@@ -1,2 +1,2 @@", sanitized)
        self.assertIn("+safe\n", sanitized)

    def test_non_patch_exact_credential_fails_closed(self):
        token = runtime_app_token()
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": token}, clear=False):
            unsafe_pr = pull_request(title="Review " + token)
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "snapshot input contains credential"):
                self.build("+safe\n", pr=unsafe_pr)
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "unsafe finding"):
                orchestrator._audit_safe_finding_text("finding " + token, "finding message")

    def test_missing_or_untrusted_repository_identity_still_fails(self):
        for repository in (None, {"full_name": "elsewhere/repository"}):
            with self.subTest(repository=repository):
                invalid = pull_request()
                invalid["head"] = copy.deepcopy(invalid["head"])
                invalid["head"]["repo"] = repository
                with self.assertRaisesRegex(orchestrator.OrchestrationError, "trusted repository"):
                    self.build("+safe\n", pr=invalid)


if __name__ == "__main__":
    unittest.main()
