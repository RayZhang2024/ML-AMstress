import unittest

from scripts import a5_reviewer as reviewer


HEAD = "2" * 40
AC_13 = "Focused and full normal-Python tests, Python syntax compilation, and git diff --check origin/main...HEAD pass."
AC_14 = "Hosted exact-head Normal Python CI and trusted-current-main A5 complete successfully without unauthorized repair or synthesized evidence."


def snapshot(criteria):
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 217,
        "issue_number": 217,
        "base_sha": "1" * 40,
        "head_sha": HEAD,
        "pr_title": "Issue #217 compound validation clause",
        "pr_body": "Refs #217",
        "issue_title": "Compound validation clause",
        "issue_body": "## Acceptance criteria\n" + "\n".join("- [ ] " + item for item in criteria) + "\n",
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": [{"path": "scripts/a5_reviewer.py", "patch": "+classifier change"}],
        "ci_checks": [{"name": "Normal Python CI", "status": "pending"}],
        "worker_metadata": {"worker_run_id": "34319556756", "branch": "codex-yellow/issue-217"},
    }


def blocker(finding):
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", "summary", (finding,), "")


class Issue217ExactCompoundValidationClauseTests(unittest.TestCase):
    def test_exact_ac13_ac14_and_compound_validation_outcomes_are_external(self):
        criteria = (
            AC_13,
            AC_14,
            "Focused tests, Python syntax compilation, and git diff --check origin/main...HEAD passed.",
            "Full normal-Python test suite, hosted Normal Python CI, and trusted A5 successful completion.",
        )
        trusted = reviewer.validate_snapshot(snapshot(criteria))
        self.assertEqual([item.kind for item in trusted.acceptance_requirements], ["external"] * len(criteria))

    def test_compound_validation_subjects_share_each_supported_outcome(self):
        outcomes = (
            "pass",
            "passes",
            "passed",
            "report success",
            "succeeds",
            "complete successfully",
            "completed successfully",
            "reach successful completion",
        )
        criteria = tuple(
            "Focused tests, Python syntax compilation, and git diff --check origin/main...HEAD " + outcome + "."
            for outcome in outcomes
        )
        trusted = reviewer.validate_snapshot(snapshot(criteria))
        self.assertEqual([item.kind for item in trusted.acceptance_requirements], ["external"] * len(criteria))

    def test_repository_wording_without_validation_subjects_remains_repairable(self):
        criteria = (
            "The current-head source must contain the complete successful pass wording.",
            "Focused repository behavior must pass its content requirements.",
            "The full file must preserve complete source content.",
        )
        trusted = reviewer.validate_snapshot(snapshot(criteria))
        self.assertEqual([item.kind for item in trusted.acceptance_requirements], ["repository"] * len(criteria))

    def test_ac13_and_ac14_findings_cannot_authorize_repair(self):
        trusted = reviewer.validate_snapshot(snapshot((AC_13, AC_14)))
        for number, criterion in enumerate((AC_13, AC_14), 1):
            finding = reviewer.Finding(
                "F-1", "evidence", "External validation is missing.", "Edit repository evidence.",
                "[AC-%d] %s" % (number, criterion),
            )
            with self.subTest(acceptance_criterion=number):
                with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
                    reviewer.validate_repairable_findings(trusted, blocker(finding))


if __name__ == "__main__":
    unittest.main()
