import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_reviewer as reviewer
from tests.test_issue75_review_orchestrator import (
    FakeClient,
    automated_evidence,
    automated_yellow_issue,
    automated_yellow_pull_request,
    event,
    protected_blocker_verdict,
)


HEAD = "31b09e0c179f6d577267b690e9197faedb2c3b9e"
EXACT_PRE_INJECTION_CRITERION = (
    "Before controlled defect injection, the file contains exactly "
    "# Terra High Reviewer Blocker Canary followed by "
    "TERRA_HIGH_REVIEWER_BLOCKER_CANARY=expected and no other non-empty content."
)
PRE_INJECTION_CONTRACT = """## Goal
Fixture.
## Necessity Gate
Fixture.
## Required behavior
Fixture.
## Do not change
Fixture.
## Acceptance criteria
- [ ] %s
## Tests/validation
Test.
## Risk classification
Declared risk label: `risk:yellow`
## Dependencies
- none
""" % EXACT_PRE_INJECTION_CRITERION


def snapshot(*criteria):
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 204,
        "issue_number": 203,
        "base_sha": "ba0ee83e680776d57c48780abc7c1a55584b49ce",
        "head_sha": HEAD,
        "pr_title": "Issue #203 fixture",
        "pr_body": "Refs #203",
        "issue_title": "Pre-injection fixture",
        "issue_body": "## Acceptance criteria\n" + "\n".join(
            "- [ ] " + criterion for criterion in criteria
        ) + "\n## Tests/validation\nTest.\n",
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": [{
            "path": "tests/fixtures/terra_high_reviewer_blocker_canary.txt",
            "patch": "+TERRA_HIGH_REVIEWER_BLOCKER_CANARY=defect",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": "success"}],
        "worker_metadata": {
            "worker_run_id": "34310100131",
            "branch": "codex-yellow/issue-203-fixture",
        },
    }


def blocker(requirement):
    finding = reviewer.Finding(
        "F-1", "tests", "The historical clean content is absent.",
        "Append clean-head, diff, checkpoint, and review evidence to the fixture.",
        "[AC-1] " + requirement,
    )
    return reviewer.ReviewVerdict(
        1, "blocker", HEAD, "yellow", "fixture verdict", (finding,), "",
    )


class PreInjectionHistoricalBoundaryTests(unittest.TestCase):
    def test_exact_issue203_pre_injection_file_content_is_external(self):
        trusted = reviewer.validate_snapshot(snapshot(EXACT_PRE_INJECTION_CRITERION))
        self.assertEqual(
            (trusted.acceptance_requirements[0].kind, trusted.acceptance_requirements[0].status),
            ("external", "pending/unverified"),
        )

    def test_bounded_pre_transition_phrases_are_external(self):
        phrases = (
            "Before defect injection, the fixture contains the expected content.",
            "Before deliberate defect injection, the file contains the expected content.",
            "The pre-defect file content is preserved.",
            "Before controlled test transition, the fixture contains the expected content.",
            "Before controlled setup transition, the file contains the expected content.",
            "Before controlled test/setup transition, the file contains the expected content.",
        )
        for phrase in phrases:
            with self.subTest(phrase=phrase):
                trusted = reviewer.validate_snapshot(snapshot(phrase))
                self.assertEqual(trusted.acceptance_requirements[0].kind, "external")

    def test_current_head_and_future_before_merge_file_requirements_remain_repository(self):
        trusted = reviewer.validate_snapshot(snapshot(
            "The current-head file contains exactly the expected content.",
            "Before merge, the file contains exactly the expected content.",
        ))
        self.assertEqual(
            [item.kind for item in trusted.acceptance_requirements],
            ["repository", "repository"],
        )

    def test_pre_injection_blocker_cannot_authorize_repair(self):
        trusted = reviewer.validate_snapshot(snapshot(EXACT_PRE_INJECTION_CRITERION))
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(
                trusted, blocker(EXACT_PRE_INJECTION_CRITERION),
            )

    def test_pre_injection_blocker_cannot_dispatch_automated_yellow_repair(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(body=PRE_INJECTION_CONTRACT),
            issue_comments=automated_evidence(),
        )
        finding = reviewer.Finding(
            "F-1", "tests", "The historical clean content is absent.",
            "Append evidence to the fixture.",
            "[AC-1] " + EXACT_PRE_INJECTION_CRITERION,
        )
        with mock.patch.object(orchestrator, "_yellow_repair") as yellow_repair:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair boundary"):
                orchestrator.orchestrate(
                    client, event(), ".", lambda *_: protected_blocker_verdict((finding,)),
                )
        yellow_repair.assert_not_called()

    def test_appending_historical_evidence_cannot_repair_pre_injection_criterion(self):
        defect_fixture = "TERRA_HIGH_REVIEWER_BLOCKER_CANARY=defect\n"
        bad_repair = defect_fixture + (
            "Clean head contained TERRA_HIGH_REVIEWER_BLOCKER_CANARY=expected.\n"
            "Defect-head diff, checkpoint, and reviewer evidence recorded.\n"
        )
        self.assertIn("TERRA_HIGH_REVIEWER_BLOCKER_CANARY=defect", bad_repair)
        self.assertIn("Clean head contained", bad_repair)

        trusted = reviewer.validate_snapshot(snapshot(EXACT_PRE_INJECTION_CRITERION))
        self.assertEqual(trusted.acceptance_requirements[0].kind, "external")
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(
                trusted, blocker(EXACT_PRE_INJECTION_CRITERION),
            )


if __name__ == "__main__":
    unittest.main()
