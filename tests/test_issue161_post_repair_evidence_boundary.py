import unittest
from unittest import mock

from scripts import a5_review_orchestrator as orchestrator
from scripts import a5_repair_worker as repair
from scripts import a5_reviewer as reviewer
from tests.test_issue75_review_orchestrator import (
    FakeClient,
    automated_evidence,
    automated_yellow_issue,
    automated_yellow_pull_request,
    event,
    protected_blocker_verdict,
)


HEAD = "b582f851fbc74cde08799841d8d424772b1acf4a"
WORKER_RUN = "34231670460"


def snapshot(issue_body, ci_status="success"):
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 160,
        "issue_number": 159,
        "base_sha": "c213c4ee907d6f1576f601135d28f77a7e5ebb93",
        "head_sha": HEAD,
        "pr_title": "Issue #159 fixture",
        "pr_body": "Refs #159",
        "issue_title": "Post-repair fixture",
        "issue_body": issue_body,
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": [{
            "path": "tests/fixtures/issue159_post_repair.txt",
            "patch": "+line one\n+line two\n+line three",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": ci_status}],
        "worker_metadata": {
            "worker_run_id": WORKER_RUN,
            "branch": "codex-yellow/issue-159-fixture",
        },
    }


def verdict(*findings):
    return reviewer.ReviewVerdict(
        1,
        "clean" if not findings else "blocker",
        HEAD,
        "yellow",
        "review summary",
        tuple(findings),
        "",
    )


POST_REPAIR_CONTRACT = """## Goal
Fixture.
## Necessity Gate
Fixture.
## Required behavior
Fixture.
## Do not change
Fixture.
## Acceptance criteria
- [ ] The current-head fixture file contains exactly the intended final three lines.
- [ ] The initial Phase-1 head fixture file contains exactly the pre-repair three-line content.
- [ ] Persisted pre-repair A5 blocker evidence is present for the exact fixture file content.
- [ ] Automated-YELLOW repair-lane execution history is observed on the same PR and branch.
- [ ] Same-PR and branch repair observations are preserved for the fixture file.
- [ ] The exact repair attempt count is 1.
- [ ] Repair-budget evidence shows no reset and MAX_REPAIR_ATTEMPTS remains unchanged.
## Tests/validation
Test.
## Risk classification
Declared risk label: `risk:yellow`
## Dependencies
- none
"""


class PostRepairEvidenceBoundaryTests(unittest.TestCase):
    def test_current_head_file_content_remains_repository_editable(self):
        trusted = reviewer.validate_snapshot(snapshot(POST_REPAIR_CONTRACT))
        requirement = trusted.acceptance_requirements[0]
        self.assertEqual((requirement.kind, requirement.status), ("repository", "repository"))

        finding = reviewer.Finding(
            "F-1",
            "tests",
            "The current fixture content is wrong.",
            "Restore the current-head fixture content.",
            "[AC-1] The current-head fixture file contains exactly the intended final three lines.",
        )
        reviewer.validate_repairable_findings(trusted, verdict(finding))

    def test_historical_explicit_file_content_is_external(self):
        trusted = reviewer.validate_snapshot(snapshot(POST_REPAIR_CONTRACT))
        requirement = trusted.acceptance_requirements[1]
        self.assertEqual((requirement.kind, requirement.status), ("external", "pending/unverified"))

        finding = reviewer.Finding(
            "F-1",
            "evidence",
            "Historical Phase-1 fixture evidence is missing.",
            "Append Phase-1 evidence to the fixture.",
            "[AC-2] The initial Phase-1 head fixture file contains exactly the pre-repair three-line content.",
        )
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(trusted, verdict(finding))

    def test_post_repair_control_plane_criteria_are_external_even_with_file_words(self):
        trusted = reviewer.validate_snapshot(snapshot(POST_REPAIR_CONTRACT))
        self.assertEqual(
            [item.kind for item in trusted.acceptance_requirements[2:]],
            ["external", "external", "external", "external", "external"],
        )

    def test_issue159_post_repair_pattern_cannot_authorize_second_repository_repair(self):
        trusted = reviewer.validate_snapshot(snapshot(POST_REPAIR_CONTRACT))
        findings = (
            reviewer.Finding(
                "F-1",
                "evidence",
                "Persisted blocker evidence is not visible in the repository file.",
                "Write persisted blocker evidence into the fixture file.",
                "[AC-3] Persisted pre-repair A5 blocker evidence is present for the exact fixture file content.",
            ),
            reviewer.Finding(
                "F-2",
                "evidence",
                "Repair-lane history is not visible in the repository file.",
                "Write repair history into the fixture file.",
                "[AC-4] Automated-YELLOW repair-lane execution history is observed on the same PR and branch.",
            ),
            reviewer.Finding(
                "F-3",
                "evidence",
                "Same-PR repair observations are not visible in the repository file.",
                "Write same-PR repair observations into the fixture file.",
                "[AC-5] Same-PR and branch repair observations are preserved for the fixture file.",
            ),
            reviewer.Finding(
                "F-4",
                "evidence",
                "Repair attempt count is not visible in the repository file.",
                "Write attempt-count evidence into the fixture file.",
                "[AC-6] The exact repair attempt count is 1.",
            ),
            reviewer.Finding(
                "F-5",
                "evidence",
                "Repair-budget/no-reset evidence is not visible in the repository file.",
                "Write repair-budget evidence into the fixture file.",
                "[AC-7] Repair-budget evidence shows no reset and MAX_REPAIR_ATTEMPTS remains unchanged.",
            ),
        )
        reviewer.validate_external_requirements(trusted)
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(trusted, verdict(*findings))

    def test_external_post_repair_finding_cannot_dispatch_automated_yellow_repair(self):
        client = FakeClient(
            pr=automated_yellow_pull_request(),
            linked_issue=automated_yellow_issue(body=POST_REPAIR_CONTRACT),
            issue_comments=automated_evidence(),
        )
        blocked = protected_blocker_verdict((reviewer.Finding(
            "F-1",
            "evidence",
            "Historical Phase-1 fixture evidence is missing.",
            "Write historical evidence into the fixture file.",
            "[AC-2] The initial Phase-1 head fixture file contains exactly the pre-repair three-line content.",
        ),))
        with mock.patch.object(orchestrator, "_yellow_repair") as yellow_repair:
            with self.assertRaisesRegex(orchestrator.OrchestrationError, "repair boundary"):
                orchestrator.orchestrate(client, event(), ".", lambda *_: blocked)
        yellow_repair.assert_not_called()
        self.assertEqual(orchestrator.repair_attempt_count(client.comment_data, 175), 0)

    def test_repair_attempt_bound_is_unchanged(self):
        self.assertEqual(repair.MAX_REPAIR_ATTEMPTS, 2)


if __name__ == "__main__":
    unittest.main()
