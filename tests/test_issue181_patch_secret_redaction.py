import os
import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer


HEAD = "a" * 40


def credential_like_value():
    return "gh" + "p_" + "abcdefgh" + "ijklmnop"


def trusted_credential_value():
    return "trusted" + "-credential" + "-value"


def issue():
    return {
        "number": 181,
        "title": "Patch redaction",
        "body": "Contract.",
        "labels": [{"name": "status:review"}, {"name": "risk:yellow"}],
    }


def pull_request():
    return {
        "number": 281,
        "title": "Issue #181",
        "body": "Refs #181",
        "base": {"sha": "b" * 40, "repo": {"full_name": orchestrator.REPOSITORY}},
        "head": {"sha": HEAD, "ref": "codex-yellow/issue-181-task",
                 "repo": {"full_name": orchestrator.REPOSITORY}},
    }


def run():
    return orchestrator.WorkflowRun(1, HEAD, "success")


def snapshot_for(patch):
    return orchestrator.build_snapshot(
        pull_request(), issue(), run(), [{"filename": "tests/example.py", "patch": patch}], "protected-yellow"
    )[0]


class PatchSecretRedactionTests(unittest.TestCase):
    def test_benign_patch_is_preserved_byte_for_byte(self):
        patch = "@@ -1 +1 @@\n-old\n+new\n"
        self.assertEqual(snapshot_for(patch)["changed_files"][0]["patch"], patch)

    def test_credential_like_addition_fails_closed_before_snapshot_validation(self):
        patch = "+value = '" + credential_like_value() + "'\n"
        with mock.patch.object(reviewer, "validate_snapshot") as validate_snapshot:
            with self.assertRaisesRegex(reviewer.ReviewError, "added line"):
                snapshot_for(patch)
        validate_snapshot.assert_not_called()

    def test_trusted_credential_addition_fails_closed_before_snapshot_validation(self):
        value = trusted_credential_value()
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": value}, clear=False):
            with mock.patch.object(reviewer, "validate_snapshot") as validate_snapshot:
                with self.assertRaisesRegex(reviewer.ReviewError, "added line"):
                    snapshot_for("+value = '" + value + "'\n")
            validate_snapshot.assert_not_called()

    def test_context_and_deletion_credential_material_is_redacted_from_snapshot_and_prompt(self):
        value = credential_like_value()
        patch = "@@ -2,2 +2,2 @@\n context = '" + value + "'\n-old = '" + value + "'\n+new = 'safe'\n"
        snapshot = snapshot_for(patch)
        sanitized = snapshot["changed_files"][0]["patch"]
        prompt = reviewer.build_prompt(reviewer.validate_snapshot(snapshot))
        self.assertIn(reviewer.PATCH_CREDENTIAL_PLACEHOLDER, sanitized)
        self.assertNotIn(value, sanitized)
        self.assertNotIn(value, prompt)
        self.assertIn("@@ -2,2 +2,2 @@", sanitized)
        self.assertIn("+new = 'safe'", sanitized)

    def test_trusted_credential_context_and_deletion_are_redacted(self):
        value = trusted_credential_value()
        patch = " context = '" + value + "'\n-old = '" + value + "'\n+new = 'safe'\n"
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": value}, clear=False):
            sanitized = snapshot_for(patch)["changed_files"][0]["patch"]
        self.assertNotIn(value, sanitized)
        self.assertEqual(sanitized.count(reviewer.PATCH_CREDENTIAL_PLACEHOLDER), 2)

    def test_metadata_header_is_not_an_added_repository_line(self):
        value = credential_like_value()
        sanitized = snapshot_for("+++ b/" + value + "\n+safe = True\n")["changed_files"][0]["patch"]
        self.assertTrue(sanitized.startswith("+++ b/"))
        self.assertIn(reviewer.PATCH_CREDENTIAL_PLACEHOLDER, sanitized)
        self.assertIn("+safe = True", sanitized)

    def test_added_content_beginning_with_plus_signs_still_fails_closed(self):
        patch = "++++value = '" + credential_like_value() + "'\n"
        with self.assertRaisesRegex(reviewer.ReviewError, "added line"):
            snapshot_for(patch)

    def test_non_patch_snapshot_fields_remain_fail_closed(self):
        value = credential_like_value()
        with self.assertRaises(reviewer.ReviewError):
            reviewer.validate_snapshot(dict(snapshot_for("+safe\n"), pr_body=value))


if __name__ == "__main__":
    unittest.main()
