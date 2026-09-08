import unittest

from scripts import a5_reviewer as reviewer


HEAD = "a" * 40

# These are the live Issue #171 acceptance criteria, deliberately preserved
# verbatim so classification cannot be validated against a paraphrase.
ISSUE_171_CRITERIA = (
    "The worker claims one fresh deterministic GREEN branch from trusted `main` and completes successfully on first execution attempt.",
    "The PR changes exactly `docs/TERRA_MEDIUM_CANARY.md` and no other file.",
    "The file contains exactly `# Terra Medium Canary` followed by `TERRA_MEDIUM_CANARY=passed` and no other non-empty content.",
    "Live worker evidence is consistent with the merged `gpt-5.6-terra` / `medium` command profile and contains no evidence of fallback to another configured implementation model or effort.",
    "Normal worker validation passes.",
    "Hosted Normal Python CI passes on the exact PR head.",
    "A5 review is clean on that same head.",
    "No protected-control-plane, scientific/runtime, workflow, GUI, ML, Abaqus, A6, or A7 path changes.",
    "The PR remains open/unmerged until GPT exact-head review.",
)


def snapshot():
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 175,
        "issue_number": 171,
        "base_sha": "b" * 40,
        "head_sha": HEAD,
        "pr_title": "Issue #175 exact Issue #171 evidence boundary",
        "pr_body": "Refs #175",
        "issue_title": "Exact Issue #171 evidence boundary",
        "issue_body": "## Acceptance criteria\n" + "\n".join(
            "- [ ] " + criterion for criterion in ISSUE_171_CRITERIA
        ) + "\n\n## Tests/validation\nFocused regression.\n",
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": [{
            "path": "tests/test_issue175_exact_issue171_evidence_boundary.py",
            "patch": "+focused regression",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": "success"}],
        "worker_metadata": {
            "worker_run_id": "33977125059",
            "branch": "codex-yellow/issue-175-a5-classifier-correction-regress-exact-issue-171-a",
        },
    }


def verdict(finding):
    return reviewer.ReviewVerdict(
        1, "blocker", HEAD, "yellow", "review summary", (finding,), ""
    )


class ExactIssue171EvidenceBoundaryTests(unittest.TestCase):
    def setUp(self):
        self.trusted = reviewer.validate_snapshot(snapshot())

    def test_fixture_contains_live_issue_171_criteria_verbatim_and_in_order(self):
        self.assertEqual(
            tuple(item.text for item in self.trusted.acceptance_requirements),
            ISSUE_171_CRITERIA,
        )

    def test_exact_issue_171_classification_vector(self):
        self.assertEqual(
            [item.kind for item in self.trusted.acceptance_requirements],
            ["external", "repository", "repository", "external", "external",
             "external", "external", "repository", "external"],
        )

    def test_external_criteria_cannot_authorize_repository_evidence_repairs(self):
        for index in (1, 4, 5, 6, 7, 9):
            criterion = ISSUE_171_CRITERIA[index - 1]
            finding = reviewer.Finding(
                "F-1", "evidence", "Live control-plane evidence is unavailable.",
                "Write worker/run/model/review evidence into a repository file.",
                "[AC-%d] %s" % (index, criterion),
            )
            with self.subTest(acceptance_criterion=index):
                with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
                    reviewer.validate_repairable_findings(self.trusted, verdict(finding))

    def test_current_head_scope_content_and_prohibited_path_criteria_remain_repairable(self):
        for index in (2, 3, 8):
            criterion = ISSUE_171_CRITERIA[index - 1]
            finding = reviewer.Finding(
                "F-1", "scope", "Current-head repository criterion is violated.",
                "Repair the current-head repository content.",
                "[AC-%d] %s" % (index, criterion),
            )
            with self.subTest(acceptance_criterion=index):
                reviewer.validate_repairable_findings(self.trusted, verdict(finding))


if __name__ == "__main__":
    unittest.main()
