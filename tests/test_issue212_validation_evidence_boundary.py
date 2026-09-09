import unittest

from scripts import a5_reviewer as reviewer


HEAD = "2" * 40
ISSUE_210_VALIDATION_CRITERIA = (
    "Existing GREEN and automatic-YELLOW A5 repair regressions continue to pass.",
    "Full normal-Python suite, Python syntax compilation, git diff --check origin/main...HEAD, hosted exact-head Normal Python CI, and trusted-current-main A5 pass.",
)
EQUIVALENT_VALIDATION_CRITERIA = (
    "Unit test suite passes.",
    "Unit test suites passed.",
    "Regression tests succeed.",
    "Python syntax compile checks pass.",
    "git diff --check succeeds.",
    "Lint and static analysis checks passed.",
    "Hosted CI check success is recorded.",
    "Trusted reviewer succeeds.",
)
REPOSITORY_CRITERIA = (
    "scripts/a5_reviewer.py must classify validation results as external evidence.",
    "tests/test_issue212_validation_evidence_boundary.py must contain focused regression coverage.",
    "The current-head reviewer source must preserve repository-editable file and content requirements.",
)


def issue_body(criteria):
    return "## Acceptance criteria\n" + "\n".join("- [ ] " + item for item in criteria) + "\n"


def snapshot(criteria, ci_status="pending", changed_files=None):
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 212,
        "issue_number": 212,
        "base_sha": "1" * 40,
        "head_sha": HEAD,
        "pr_title": "Issue #212 validation evidence boundary",
        "pr_body": "Refs #212",
        "issue_title": "Validation evidence boundary",
        "issue_body": issue_body(criteria),
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": changed_files or [{
            "path": "scripts/a5_reviewer.py", "patch": "+classifier change",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": ci_status}],
        "worker_metadata": {"worker_run_id": "34313708875", "branch": "codex-yellow/issue-212"},
    }


def blocker(finding):
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", "summary", (finding,), "")


class Issue212ValidationEvidenceBoundaryTests(unittest.TestCase):
    def test_issue_210_and_equivalent_validation_results_are_external(self):
        criteria = ISSUE_210_VALIDATION_CRITERIA + EQUIVALENT_VALIDATION_CRITERIA
        trusted = reviewer.validate_snapshot(snapshot(criteria))
        self.assertEqual([item.kind for item in trusted.acceptance_requirements], ["external"] * len(criteria))

    def test_current_head_source_file_and_content_requirements_remain_repairable(self):
        trusted = reviewer.validate_snapshot(snapshot(REPOSITORY_CRITERIA))
        self.assertEqual([item.kind for item in trusted.acceptance_requirements], ["repository"] * len(REPOSITORY_CRITERIA))
        finding = reviewer.Finding(
            "F-1", "tests", "Focused coverage is absent.", "Add the focused regression.",
            "[AC-2] " + REPOSITORY_CRITERIA[1],
        )
        reviewer.validate_repairable_findings(trusted, blocker(finding))

    def test_issue_210_external_blockers_cannot_authorize_repair(self):
        trusted = reviewer.validate_snapshot(snapshot(ISSUE_210_VALIDATION_CRITERIA))
        for number, criterion in enumerate(ISSUE_210_VALIDATION_CRITERIA, 1):
            finding = reviewer.Finding(
                "F-1", "evidence", "Validation evidence is missing.",
                "Edit repository files to claim validation success.", "[AC-%d] %s" % (number, criterion),
            )
            with self.subTest(acceptance_criterion=number):
                with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
                    reviewer.validate_repairable_findings(trusted, blocker(finding))

    def test_synthesized_repository_success_cannot_repair_missing_validation_evidence(self):
        synthesized_files = [
            {"path": "scripts/a5_review_orchestrator.py", "patch": "+record Normal Python CI success"},
            {"path": "tests/test_issue210_repair_push_verification.py", "patch": "+assert success"},
            {"path": "docs/VALIDATION.md", "patch": "+Trusted A5 passed"},
            {"path": "reviewer-snapshot.json", "patch": "+{\"Normal Python CI\": \"success\"}"},
        ]
        original = reviewer.validate_snapshot(snapshot(ISSUE_210_VALIDATION_CRITERIA))
        synthesized = reviewer.validate_snapshot(snapshot(ISSUE_210_VALIDATION_CRITERIA, changed_files=synthesized_files))
        self.assertEqual(
            [(item.identifier, item.kind, item.status) for item in synthesized.acceptance_requirements],
            [(item.identifier, item.kind, item.status) for item in original.acceptance_requirements],
        )
        finding = reviewer.Finding(
            "F-1", "evidence", "Snapshot claims cannot replace trusted validation.",
            "Synthesize success in reviewer-facing repository data.",
            "[AC-2] " + ISSUE_210_VALIDATION_CRITERIA[1],
        )
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(synthesized, blocker(finding))


if __name__ == "__main__":
    unittest.main()
