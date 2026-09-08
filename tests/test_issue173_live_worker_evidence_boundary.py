import unittest

from scripts import a5_reviewer as reviewer


HEAD = "a" * 40


ISSUE_171_ACCEPTANCE_CRITERIA = """## Acceptance criteria
- [ ] The worker claims exactly one branch from trusted `main` and completes successfully on its first execution attempt.
- [ ] Changed-file scope is exactly one new file: `docs/TERRA_MEDIUM_CANARY.md`.
- [ ] `docs/TERRA_MEDIUM_CANARY.md` contains exactly these two nonblank lines: `# Terra Medium Canary` and `TERRA_MEDIUM_CANARY=passed`.
- [ ] Live worker execution evidence proves the Terra model, medium reasoning effort, and no-fallback profile.
- [ ] Normal worker validation passes.
- [ ] Hosted Normal Python CI passes on the exact protected PR head.
- [ ] A5 review is clean on that exact head.
- [ ] No prohibited path changes are present on the current head.
- [ ] The PR remains open and unmerged.

## Tests/validation
Test.
"""


def snapshot():
    return {
        "schema_version": 1,
        "repository": "RayZhang2024/ML-AMstress",
        "pull_request_number": 172,
        "issue_number": 171,
        "base_sha": "b" * 40,
        "head_sha": HEAD,
        "pr_title": "Issue #171 fixture",
        "pr_body": "Closes #171",
        "issue_title": "Live worker acceptance fixture",
        "issue_body": ISSUE_171_ACCEPTANCE_CRITERIA,
        "issue_labels": ["status:review", "risk:yellow"],
        "declared_risk": "yellow",
        "trusted_risk_floor": "yellow",
        "changed_files": [{
            "path": "docs/TERRA_MEDIUM_CANARY.md",
            "patch": "+# Terra Medium Canary\n+TERRA_MEDIUM_CANARY=passed",
        }],
        "ci_checks": [{"name": "Normal Python CI", "status": "success"}],
        "worker_metadata": {
            "worker_run_id": "34244150554",
            "branch": "codex/issue-171-terra-medium-canary",
        },
    }


def blocker(*findings):
    return reviewer.ReviewVerdict(1, "blocker", HEAD, "yellow", "summary", findings, "")


class LiveWorkerEvidenceBoundaryTests(unittest.TestCase):
    def setUp(self):
        self.trusted = reviewer.validate_snapshot(snapshot())

    def test_exact_issue_171_criteria_split_control_plane_from_repository_deliverables(self):
        self.assertEqual(
            [item.kind for item in self.trusted.acceptance_requirements],
            ["external", "repository", "repository", "external", "external", "external",
             "external", "repository", "external"],
        )

    def test_external_issue_171_findings_cannot_authorize_canary_evidence_repairs(self):
        external_identifiers = ("AC-1", "AC-4", "AC-5", "AC-6", "AC-7", "AC-9")
        for identifier in external_identifiers:
            requirement = next(item for item in self.trusted.acceptance_requirements
                               if item.identifier == identifier)
            finding = reviewer.Finding(
                "F-1", "evidence", "Live control-plane evidence is unavailable.",
                "Write worker/run/model/review evidence into docs/TERRA_MEDIUM_CANARY.md.",
                "[{}] {}".format(identifier, requirement.text),
            )
            with self.subTest(identifier=identifier):
                with self.assertRaisesRegex(reviewer.ReviewError, "external acceptance requirement"):
                    reviewer.validate_repairable_findings(self.trusted, blocker(finding))

    def test_current_head_scope_and_content_findings_remain_repair_authorizable(self):
        findings = []
        for identifier in ("AC-2", "AC-3", "AC-8"):
            requirement = next(item for item in self.trusted.acceptance_requirements
                               if item.identifier == identifier)
            findings.append(reviewer.Finding(
                "F-{}".format(len(findings) + 1), "scope",
                "Current-head repository deliverable is incorrect.",
                "Restore the authorized canary file scope or exact content.",
                "[{}] {}".format(identifier, requirement.text),
            ))
        reviewer.validate_repairable_findings(self.trusted, blocker(*findings))


if __name__ == "__main__":
    unittest.main()
