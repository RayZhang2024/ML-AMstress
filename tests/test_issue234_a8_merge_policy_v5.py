import re
import unittest
from pathlib import Path

from scripts import a8_merge_policy as policy

HEAD, NEXT_HEAD = "a" * 40, "b" * 40
ROOT = Path(__file__).resolve().parents[1]
HEADINGS = ("## Work-unit rules", "## Repository risk model", "### GREEN", "### YELLOW", "### RED", "## Effective-risk escalation", "## Merge authority", "## Credential and safety boundaries", "## Mandatory stop and escalation conditions")
LITERALS = ("One issue per branch and pull request", "Necessity Gate", "duplicate concurrent implementations", "required validation cannot be run", "least-privilege GitHub permissions", "Never place unrestricted secrets", "Destructive operations", "this document does not", "does not enable merge", "never auto-merged", "issue requirements conflict", "scientific intent is ambiguous", "another implementation is already active")


def snapshot(**changes):
    value = {"schema_version": 1, "issue": {"number": 234, "labels": ["status:review", "risk:yellow"], "dependencies_satisfied": True, "duplicate_or_conflicting_work": False}, "pr": {"number": 1, "issue_number": 234, "head_sha": HEAD, "head_count": 1, "open": True, "merged": False, "draft": False, "mergeable": True, "same_repository": True, "base": "main", "identity_current": True}, "contract": {"complete": True, "issue_number": 234, "declared_risk": "yellow", "dependencies": (), "sections": policy.REQUIRED_CONTRACT_SECTIONS, "required_repository_validations": ("focused",), "required_runtime_evidence": (), "controlled_runtime_required": False}, "scope": {"effective_risk": "yellow", "changed_files": ("scripts/a8_merge_policy.py",), "authorized_paths": ("scripts/a8_merge_policy.py",), "fully_enumerated": True}, "validations": {"repository": ({"name": "focused", "status": "success", "head_sha": HEAD},), "ci": ({"name": "Normal Python CI", "status": "success", "head_sha": HEAD}, {"name": "git diff --check origin/main...HEAD", "status": "success", "head_sha": HEAD})}, "a5": {"state": "review:clean", "head_sha": HEAD, "unresolved_findings": 0, "unresolved_threads": 0, "blocking_submissions": 0}, "runtime_evidence": (), "authorizations": {"implementation": {"scope": "implementation", "head_sha": HEAD, "granted": True}, "controlled_runtime": {"scope": "controlled_runtime", "head_sha": HEAD, "granted": True}, "merge": {"scope": "gpt_managed_merge", "head_sha": HEAD, "granted": True}}}
    value.update(changes)
    return value


class A8Tests(unittest.TestCase):
    def test_clean_green_and_yellow(self):
        self.assertTrue(policy.evaluate(snapshot())["merge_ready"])
        green = snapshot(issue={"number": 234, "labels": ["status:review", "risk:green"], "dependencies_satisfied": True, "duplicate_or_conflicting_work": False}, contract=dict(snapshot()["contract"], declared_risk="green"), scope={"effective_risk": "green", "changed_files": ("docs/x.md",), "authorized_paths": ("docs/x.md",), "fully_enumerated": True})
        self.assertTrue(policy.evaluate(green)["merge_ready"])

    def test_all_gate_classes_fail_closed(self):
        cases = ({"issue": {"number": 234, "labels": ["status:ready", "status:review", "risk:yellow"], "dependencies_satisfied": True, "duplicate_or_conflicting_work": False}}, {"issue": {"number": 234, "labels": ["status:review", "risk:yellow"], "dependencies_satisfied": False, "duplicate_or_conflicting_work": True}}, {"contract": {}}, {"pr": dict(snapshot()["pr"], draft=True)}, {"pr": dict(snapshot()["pr"], open=False)}, {"pr": dict(snapshot()["pr"], mergeable=False)}, {"scope": {"effective_risk": "yellow", "changed_files": (), "authorized_paths": ("x",), "fully_enumerated": False}}, {"scope": {"effective_risk": "yellow", "changed_files": ("other/x",), "authorized_paths": ("docs/x",), "fully_enumerated": True}}, {"scope": {"effective_risk": "red", "changed_files": ("x",), "authorized_paths": ("x",), "fully_enumerated": True}}, {"a5": dict(snapshot()["a5"], state="review:blocker")}, {"a5": dict(snapshot()["a5"], unresolved_findings=1)}, {"a5": dict(snapshot()["a5"], unresolved_threads=1)}, {"a5": dict(snapshot()["a5"], blocking_submissions=1)}, {"validations": {"repository": (), "ci": ()}})
        for changed in cases:
            with self.subTest(changed=changed): self.assertFalse(policy.evaluate(snapshot(**changed))["merge_ready"])

    def test_head_runtime_red_and_determinism(self):
        stale = snapshot(pr=dict(snapshot()["pr"], head_sha=NEXT_HEAD))
        self.assertIn("missing_or_stale_normal_ci", policy.evaluate(stale)["reason_codes"])
        runtime = snapshot(contract=dict(snapshot()["contract"], required_runtime_evidence=("solver",)))
        self.assertIn("missing_or_stale_runtime_evidence", policy.evaluate(runtime)["reason_codes"])
        red = snapshot(issue={"number": 234, "labels": ["status:review", "risk:red"], "dependencies_satisfied": True, "duplicate_or_conflicting_work": False}, contract=dict(snapshot()["contract"], declared_risk="red"), scope={"effective_risk": "red", "changed_files": ("x",), "authorized_paths": ("x",), "fully_enumerated": True}, authorizations=dict(snapshot()["authorizations"], merge={"scope": "red_user_or_domain_owner_merge", "head_sha": HEAD, "granted": True}))
        self.assertTrue(policy.evaluate(red)["merge_ready"])
        self.assertFalse(policy.evaluate(snapshot(issue=red["issue"], contract=red["contract"], scope=red["scope"]))["merge_ready"])
        self.assertFalse(policy.evaluate(snapshot(issue=red["issue"], contract=red["contract"], scope=red["scope"], authorizations=dict(snapshot()["authorizations"], merge={"scope": "red_user_or_domain_owner_merge", "head_sha": NEXT_HEAD, "granted": True})))["merge_ready"])
        self.assertEqual(policy.evaluate({}), policy.evaluate({}))


class GovernanceTests(unittest.TestCase):
    def test_full_issue22_raw_contract(self):
        text = (ROOT / "docs" / "AUTONOMOUS_DEVELOPMENT.md").read_text(encoding="utf-8")
        for required in HEADINGS + LITERALS: self.assertIn(required, text)

    def test_new_policy_prose_is_whitespace_robust(self):
        text = re.sub(r"\s+", " ", (ROOT / "docs" / "AUTONOMOUS_DEVELOPMENT.md").read_text(encoding="utf-8"))
        for required in ("GPT-managed GREEN/YELLOW merge policy", "exact-head pre-merge recheck", "RED requires explicit current user/domain-owner authorization", "separate authorization scopes"):
            self.assertIn(required, text)

    def test_no_actor_merge_route_is_introduced(self):
        for path in (ROOT / "scripts" / "a8_merge_policy.py", ROOT / "scripts" / "codex_issue_worker.py", ROOT / "scripts" / "a5_review_orchestrator.py", ROOT / "scripts" / "a5_repair_worker.py", ROOT / ".github" / "workflows" / "a5-review-loop.yml"):
            text = path.read_text(encoding="utf-8")
            self.assertNotIn("enable_auto_merge", text)
            self.assertNotIn("merge_pull", text)

    def test_existing_model_and_repair_pins_remain_exact(self):
        worker = (ROOT / "scripts" / "codex_issue_worker.py").read_text(encoding="utf-8")
        reviewer = (ROOT / "scripts" / "a5_reviewer.py").read_text(encoding="utf-8")
        repair = (ROOT / "scripts" / "a5_repair_worker.py").read_text(encoding="utf-8")
        for text, expected in ((worker, 'CODEX_WORKER_MODEL = "gpt-5.6-terra"'), (worker, 'CODEX_WORKER_REASONING_EFFORT = "medium"'), (reviewer, 'REVIEWER_MODEL = "gpt-5.6-terra"'), (reviewer, 'REVIEWER_REASONING_EFFORT = "high"'), (repair, 'CODEX_REPAIR_MODEL = "gpt-5.5"'), (repair, "MAX_REPAIR_ATTEMPTS = 2")):
            self.assertIn(expected, text)


if __name__ == "__main__": unittest.main()
