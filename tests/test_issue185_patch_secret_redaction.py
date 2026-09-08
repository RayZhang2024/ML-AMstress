import copy
import os
import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer


HEAD = "a" * 40
BASE = "b" * 40
BRANCH = "codex/issue-185-patch-credential-redaction"


def pull_request():
    repository = {"full_name": orchestrator.REPOSITORY}
    return {
        "number": 185,
        "state": "open",
        "title": "Issue #185",
        "body": "Refs #185",
        "labels": [],
        "base": {"ref": "main", "sha": BASE, "repo": copy.deepcopy(repository)},
        "head": {"sha": HEAD, "ref": BRANCH, "repo": copy.deepcopy(repository)},
    }


def issue():
    return {
        "number": 185,
        "title": "Patch boundary",
        "body": "Patch boundary contract.",
        "labels": [{"name": "risk:yellow"}],
    }


def run():
    return orchestrator.parse_workflow_run({"workflow_run": {
        "id": 185, "name": "Normal Python CI", "status": "completed",
        "conclusion": "success", "head_sha": HEAD,
    }})


def snapshot_for(patch):
    return orchestrator.build_snapshot(
        pull_request(), issue(), run(), [{"filename": "docs/change.md", "patch": patch}],
        "protected-yellow",
    )[0]


class PatchSecretRedactionTests(unittest.TestCase):
    def test_benign_patch_is_preserved_byte_for_byte(self):
        patch = "@@ -1 +1 @@\n-safe\n+safe\n"
        self.assertEqual(snapshot_for(patch)["changed_files"][0]["patch"], patch)

    def test_credential_pattern_on_added_line_blocks_before_snapshot(self):
        token = "gh" + "p_" + "abcdefgh"
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "credential material"):
            snapshot_for("+value = " + token + "\n")

    def test_credential_pattern_on_context_and_deletion_lines_is_redacted(self):
        token = "gh" + "p_" + "abcdefgh"
        snapshot = snapshot_for(" context " + token + "\n-old " + token + "\n")
        sanitized = snapshot["changed_files"][0]["patch"]
        self.assertNotIn(token, sanitized)
        self.assertEqual(sanitized.count(reviewer.PATCH_CREDENTIAL_PLACEHOLDER), 2)

    def test_exact_automation_app_token_blocks_added_line_and_redacts_nonadded_lines(self):
        token = "opaque" + "-runtime-value-185"
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": token}, clear=False):
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "credential material"):
                snapshot_for("+value = " + token + "\n")
            patch = " context " + token + "\n-old " + token + "\n"
            snapshot = snapshot_for(patch)
        sanitized = snapshot["changed_files"][0]["patch"]
        self.assertNotIn(token, sanitized)
        self.assertEqual(sanitized.count(reviewer.PATCH_CREDENTIAL_PLACEHOLDER), 2)
        self.assertNotIn(token, reviewer.build_prompt(reviewer.validate_snapshot(snapshot)))

    def test_exact_automation_app_token_in_nonpatch_snapshot_field_fails_closed(self):
        token = "opaque" + "-runtime-value-185"
        pr = pull_request()
        pr["title"] = token
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": token}, clear=False):
            with self.assertRaisesRegex(reviewer.ReviewError, "credential material"):
                orchestrator.build_snapshot(
                    pr, issue(), run(), [{"filename": "docs/change.md", "patch": "+safe\n"}],
                    "protected-yellow",
                )

    def test_metadata_is_not_misclassified_as_added_content(self):
        token = "opaque" + "-runtime-value-185"
        with mock.patch.dict(os.environ, {"AUTOMATION_APP_TOKEN": token}, clear=False):
            for header in ("+++ b/", "+++\tb/"):
                with self.subTest(header=header):
                    snapshot = snapshot_for(header + token + "\n@@ -1 +1 @@\n+safe\n")
                    sanitized = snapshot["changed_files"][0]["patch"]
                    self.assertTrue(sanitized.startswith(header))
                    self.assertNotIn(token, sanitized)
                    self.assertIn("@@ -1 +1 @@\n+safe\n", sanitized)

    def test_trusted_pr_identity_is_required_without_weakening_production_checks(self):
        trusted = pull_request()
        self.assertEqual(orchestrator._pr_branch(trusted), BRANCH)
        untrusted = pull_request()
        untrusted["head"]["repo"] = {"full_name": "other/repository"}
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "trusted repository"):
            orchestrator._pr_branch(untrusted)
        missing = pull_request()
        missing["head"]["repo"] = None
        with self.assertRaisesRegex(orchestrator.OrchestrationError, "trusted repository"):
            orchestrator._pr_branch(missing)


if __name__ == "__main__":
    unittest.main()
