import unittest

from scripts import a5_reviewer as reviewer
from scripts import a5_repair_worker as repair


HEAD = "a" * 40
WORKER_RUN = "33977125059"


def snapshot(issue_body, ci_status="success"):
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 103,
        "issue_number": 102,
        "base_sha": "b" * 40,
        "head_sha": HEAD,
        "pr_title": "Controlled fixture",
        "pr_body": "Closes #102",
        "issue_title": "Controlled fixture acceptance",
        "issue_body": issue_body,
        "issue_labels": ["status:review", "risk:green"],
        "declared_risk": "green",
        "trusted_risk_floor": "green",
        "changed_files": [{
            "path": "docs/A5_4B_LIVE_FIXTURE.md",
            "patch": "+# A5.4b Live Fixture\n+CONTROL_VALUE: A5_4B_PASS",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": ci_status}],
        "worker_metadata": {"worker_run_id": WORKER_RUN, "branch": "codex/issue-102-fixture"},
    }


def verdict(finding=None):
    findings = () if finding is None else (finding,)
    return reviewer.ReviewVerdict(
        1, "clean" if finding is None else "blocker", HEAD, "green", "review summary", findings, ""
    )


INITIAL_FIXTURE_CONTRACT = """## Goal
Fixture.
## Acceptance criteria
- [ ] The fixture file contains exactly the required two lines.
- [ ] GREEN worker workflow completion is observed.
- [ ] Issue labels/status are observed on GitHub.
- [ ] Audit comment counts and idempotency are observed.
- [ ] PR merged/open state and workflow SHA / PR head SHA are observed.
## Tests/validation
Test.
"""


ISSUE_102_ACCEPTANCE_CRITERIA = """## Acceptance criteria
- [ ] Exactly one worker branch and one PR are created.
- [ ] Exactly one changed file: `docs/A4_18_COMPLETION_OBSERVER_LIVE_FIXTURE.md`.
- [ ] The file content is exactly the required two nonblank lines.
- [ ] Hosted Normal Python CI passes on the exact PR head.
- [ ] The GREEN worker reaches its normal terminal success state.
- [ ] The A4.18 completion observer runs from that exact worker completion event.
- [ ] Exactly one trusted `a4.18-completion` audit marker is recorded for the exact worker run ID.
- [ ] The audit marker binds the issue number and claimed worker branch, records the trusted workflow `main` execution SHA separately from any PR head SHA, and is bounded/secret-safe.
- [ ] Replayed/idempotent processing does not create a duplicate completion marker.
- [ ] The observer does not overwrite the GREEN worker's issue status.
- [ ] The fixture PR remains open and unmerged.

## Tests/validation
Test.
"""


class ExternalEvidenceBoundaryTests(unittest.TestCase):
    def test_existing_repair_attempt_bound_is_unchanged(self):
        self.assertEqual(repair.MAX_REPAIR_ATTEMPTS, 2)

    def test_initial_two_line_fixture_and_external_acceptance_requirements_need_no_repair(self):
        trusted = reviewer.validate_snapshot(snapshot(INITIAL_FIXTURE_CONTRACT))
        requirements = trusted.acceptance_requirements
        self.assertEqual(requirements[0].kind, "repository")
        self.assertEqual([item.kind for item in requirements[1:]], ["external"] * 4)
        self.assertEqual([item.status for item in requirements[1:]], ["pending/unverified"] * 4)
        reviewer.validate_external_requirements(trusted)
        reviewer.validate_repairable_findings(trusted, verdict())

    def test_full_issue_102_acceptance_criteria_keep_only_fixture_content_repairable(self):
        trusted = reviewer.validate_snapshot(snapshot(ISSUE_102_ACCEPTANCE_CRITERIA))
        requirements = trusted.acceptance_requirements
        self.assertEqual(len(requirements), 11)
        self.assertEqual([item.kind for item in requirements[:3]], [
            "external", "repository", "repository",
        ])
        self.assertEqual([item.kind for item in requirements[3:]], ["external"] * 8)
        self.assertTrue(all(item.kind == "external" for item in requirements if item.identifier not in ("AC-2", "AC-3")))
        reviewer.validate_external_requirements(trusted)
        lifecycle_finding = reviewer.Finding(
            "F-1", "evidence", "Worker branch lifecycle evidence is unavailable.", "Edit the fixture.",
            "[AC-1] Exactly one worker branch and one PR are created.",
        )
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(trusted, verdict(lifecycle_finding))

    def test_pending_and_satisfied_external_evidence_never_authorize_repair(self):
        pending = reviewer.validate_snapshot(snapshot(INITIAL_FIXTURE_CONTRACT))
        reviewer.validate_external_requirements(pending)
        self.assertEqual(pending.acceptance_requirements[1].status, "pending/unverified")

        satisfied_contract = """## Acceptance criteria
- [ ] The fixture file contains exactly the required two lines.
- [ ] Hosted Normal Python CI passes.
"""
        satisfied = reviewer.validate_snapshot(snapshot(satisfied_contract, ci_status="success"))
        self.assertEqual(satisfied.acceptance_requirements[1].status, "verified")
        reviewer.validate_external_requirements(satisfied)

    def test_wrong_trusted_run_identity_fails_closed_without_repository_repair(self):
        contract = """## Acceptance criteria
- [ ] The fixture file contains exactly the required two lines.
- [ ] GREEN worker run ID 33977224615 completed.
"""
        trusted = reviewer.validate_snapshot(snapshot(contract))
        self.assertEqual(trusted.acceptance_requirements[1].status, "contradictory")
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance evidence is contradictory"):
            reviewer.validate_external_requirements(trusted)

    def test_only_explicit_green_worker_run_ids_are_compared_to_worker_metadata(self):
        correct_worker = reviewer.validate_snapshot(snapshot("""## Acceptance criteria
- [ ] GREEN Codex issue worker run ID 33977125059 completed.
"""))
        self.assertNotEqual(correct_worker.acceptance_requirements[0].status, "contradictory")

        ci_run = reviewer.validate_snapshot(snapshot("""## Acceptance criteria
- [ ] Normal Python CI run 33989353676 passes.
"""))
        self.assertEqual(ci_run.acceptance_requirements[0].kind, "external")
        self.assertNotEqual(ci_run.acceptance_requirements[0].status, "contradictory")

        observer_run = reviewer.validate_snapshot(snapshot("""## Acceptance criteria
- [ ] Observer workflow run ID 33989353676 is recorded.
"""))
        self.assertEqual(observer_run.acceptance_requirements[0].kind, "external")
        self.assertNotEqual(observer_run.acceptance_requirements[0].status, "contradictory")

        unrelated_number = reviewer.validate_snapshot(snapshot("""## Acceptance criteria
- [ ] GitHub-side audit comment 33989353676 is observed.
"""))
        self.assertEqual(unrelated_number.acceptance_requirements[0].kind, "external")
        self.assertNotEqual(unrelated_number.acceptance_requirements[0].status, "contradictory")

    def test_real_repository_pass_to_fail_defect_remains_repairable(self):
        trusted = reviewer.validate_snapshot(snapshot(INITIAL_FIXTURE_CONTRACT))
        finding = reviewer.Finding(
            "F-1", "tests", "Fixture changed from PASS to FAIL.", "Restore PASS.",
            "[AC-1] The fixture must contain the required PASS value.",
        )
        reviewer.validate_repairable_findings(trusted, verdict(finding))

    def test_external_only_finding_is_rejected_before_a53(self):
        trusted = reviewer.validate_snapshot(snapshot(INITIAL_FIXTURE_CONTRACT))
        external_finding = reviewer.Finding(
            "F-1", "evidence", "Workflow observation is pending.", "Write a run ID into the fixture.",
            "[AC-2] GitHub-side workflow observation.",
        )
        with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
            reviewer.validate_repairable_findings(trusted, verdict(external_finding))


if __name__ == "__main__":
    unittest.main()
