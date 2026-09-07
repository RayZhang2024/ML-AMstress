import json
import unittest

from scripts import yellow_lane_policy as policy
from tests.test_issue28_worker import GREEN_BODY

SHA, OTHER_SHA = "a" * 40, "b" * 40
YELLOW_BODY = GREEN_BODY.replace("risk:green", "risk:yellow")


def prestart(**changes):
    value = {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 147,
             "trusted_base_sha": SHA, "declared_risk": "yellow", "effective_risk": "yellow",
             "authorized_paths": ["docs/example.md", "tests/test_example.py"],
             "scientific_runtime_prohibited": True}
    value.update(changes)
    return value


def claim(**changes):
    value = {"schema_version": 1, "repository": policy.REPOSITORY, "issue_number": 147,
             "trusted_base_sha": SHA, "branch": policy.yellow_branch(147, "YELLOW router policy"),
             "lane": policy.AUTOMATED_YELLOW_LANE}
    value.update(changes)
    return value


def snapshot(**changes):
    value = {"event_action": "labeled", "added_label": "agent:codex", "fresh_label_add": True,
             "labels": ["status:ready", "risk:green", "agent:codex"], "contract": GREEN_BODY,
             "dependencies_satisfied": True, "duplicate_claim": False, "effective_risk": "green",
             "scientific_scope": False, "repository": policy.REPOSITORY, "issue_number": 147,
             "trusted_base_sha": SHA, "prestart_evidence": None}
    value.update(changes)
    return value


class RoutingTests(unittest.TestCase):
    def test_green_and_authorized_yellow(self):
        self.assertEqual(policy.route(snapshot()), policy.GREEN)
        self.assertEqual(policy.route(snapshot(labels=["status:ready", "risk:yellow", "agent:codex"],
                                               contract=YELLOW_BODY, effective_risk="yellow",
                                               prestart_evidence=policy.serialize_prestart(prestart()))), policy.YELLOW)

    def test_yellow_requires_valid_authorization(self):
        base = {"labels": ["status:ready", "risk:yellow", "agent:codex"],
                "contract": YELLOW_BODY, "effective_risk": "yellow"}
        self.assertEqual(policy.route(snapshot(**base)), policy.REJECT)
        self.assertEqual(policy.route(snapshot(**dict(base, prestart_evidence="{bad"))), policy.REJECT)

    def test_all_routing_rejections(self):
        cases = ({"labels": ["status:ready", "risk:red"]}, {"labels": ["status:ready"]},
                 {"labels": ["status:ready", "risk:blue"]},
                 {"labels": ["status:ready", "risk:green", "risk:yellow"]}, {"labels": ["risk:green"]},
                 {"labels": ["status:ready", "status:review", "risk:green"]},
                 {"labels": ["status:review", "risk:green"]}, {"contract": "bad"},
                 {"contract": YELLOW_BODY}, {"event_action": "edited"}, {"added_label": "risk:green"},
                 {"fresh_label_add": False}, {"dependencies_satisfied": False}, {"duplicate_claim": True},
                 {"effective_risk": "uncertain"}, {"effective_risk": "red"}, {"scientific_scope": True})
        for changed in cases:
            with self.subTest(changed=changed):
                self.assertEqual(policy.route(snapshot(**changed)), policy.REJECT)
        missing_event = snapshot(); del missing_event["event_action"]
        self.assertEqual(policy.route(missing_event), policy.REJECT)


class BranchTests(unittest.TestCase):
    def test_normalization_unicode_fallback_and_bounds(self):
        self.assertEqual(policy.yellow_branch(147, "YELLOW  router---policy!"),
                         "codex-yellow/issue-147-yellow-router-policy")
        self.assertEqual(policy.yellow_branch(2, "Café 数据 Test"), "codex-yellow/issue-2-caf-test")
        self.assertEqual(policy.yellow_branch(3, "数据测试"), "codex-yellow/issue-3-yellow-work")
        self.assertEqual(policy.validate_yellow_branch(policy.yellow_branch(147, "title"), 147),
                         "codex-yellow/issue-147-title")
        with self.assertRaises(policy.PolicyError): policy.yellow_branch(1, "word " * 100)

    def test_malformed_inputs(self):
        for number in (0, -1, True, 1.5, "1"):
            with self.subTest(number=number), self.assertRaises(policy.PolicyError): policy.yellow_branch(number, "x")
        for title in ("../x", "C:\\temp", "a/b", "bad\x00title"):
            with self.subTest(title=title), self.assertRaises(policy.PolicyError): policy.yellow_branch(1, title)
        branch = policy.yellow_branch(147, "title")
        for bad, issue in ((branch, 148), ("codex-yellow/issue-147-a--b", 147),
                           ("../issue-147-title", 147), ("x" * 129, 147)):
            with self.subTest(bad=bad), self.assertRaises(policy.PolicyError): policy.validate_yellow_branch(bad, issue)


class PreStartTests(unittest.TestCase):
    def test_round_trip_and_determinism(self):
        canonical = policy.serialize_prestart(prestart())
        reordered = policy.serialize_prestart(prestart(authorized_paths=["tests/test_example.py", "docs/example.md"]))
        self.assertEqual(canonical, reordered)
        self.assertEqual(policy.serialize_prestart(policy.parse_prestart(canonical, expected_issue=147,
                                                                        expected_base=SHA)), canonical)

    def test_json_identity_type_and_risk_rejections(self):
        for raw in ('{"schema_version":1,"schema_version":1}', "{bad"):
            with self.subTest(raw=raw), self.assertRaises(policy.PolicyError): policy.parse_prestart(raw)
        cases = ({"unknown": 1}, {"schema_version": True}, {"repository": "other/repo"},
                 {"issue_number": 0}, {"issue_number": True}, {"trusted_base_sha": "bad"},
                 {"declared_risk": "green"}, {"effective_risk": "red"},
                 {"scientific_runtime_prohibited": False}, {"authorized_paths": "docs/x"})
        for changed in cases:
            item = prestart(); item.update(changed)
            with self.subTest(changed=changed), self.assertRaises(policy.PolicyError): policy.serialize_prestart(item)
        valid = policy.serialize_prestart(prestart())
        with self.assertRaises(policy.PolicyError): policy.parse_prestart(valid, expected_issue=148)
        with self.assertRaises(policy.PolicyError): policy.parse_prestart(valid, expected_base=OTHER_SHA)

    def test_path_and_size_rejections(self):
        paths = ([], ["docs/x"] * 2, ["x"] * (policy.MAX_PATHS + 1),
                 ["x" * (policy.MAX_PATH + 1)], ["../x"], ["/tmp/x"], ["C:/tmp/x"],
                 ["docs/*.md"], ["docs/a?b"], [{"not": "hashable"}])
        for value in paths:
            with self.subTest(value=value), self.assertRaises(policy.PolicyError):
                policy.serialize_prestart(prestart(authorized_paths=value))
        with self.assertRaises(policy.PolicyError): policy.parse_prestart(" " * 8193)


class ClaimAndReplayTests(unittest.TestCase):
    def test_claim_round_trip_and_determinism(self):
        canonical = policy.serialize_claim(claim())
        self.assertEqual(policy.serialize_claim(policy.parse_claim(json.dumps(claim()))), canonical)
        self.assertEqual(policy.resolve_claim_replay([], canonical, expected_issue=147, expected_base=SHA), "new")
        self.assertEqual(policy.resolve_claim_replay([json.dumps(claim())], canonical,
                                                     expected_issue=147, expected_base=SHA), "idempotent")

    def test_claim_and_replay_rejections(self):
        for raw in ('{"schema_version":1,"schema_version":1}', "{bad"):
            with self.subTest(raw=raw), self.assertRaises(policy.PolicyError): policy.parse_claim(raw)
        cases = ({"unknown": 1}, {"schema_version": True}, {"repository": "other/repo"},
                 {"issue_number": 0}, {"trusted_base_sha": "bad"}, {"branch": "bad"},
                 {"branch": policy.yellow_branch(148, "other")}, {"lane": "green"})
        for changed in cases:
            item = claim(); item.update(changed)
            with self.subTest(changed=changed), self.assertRaises(policy.PolicyError): policy.serialize_claim(item)
        valid = policy.serialize_claim(claim())
        expected_cases = ({"expected_repository": "other/repo"}, {"expected_issue": 148},
                          {"expected_base": OTHER_SHA}, {"expected_branch": policy.yellow_branch(147, "other")})
        for expected in expected_cases:
            with self.subTest(expected=expected), self.assertRaises(policy.PolicyError): policy.parse_claim(valid, **expected)
        conflict = policy.serialize_claim(claim(branch=policy.yellow_branch(147, "other")))
        for records in ([conflict], [valid, valid], ["{bad"]):
            with self.subTest(records=records), self.assertRaises(policy.PolicyError):
                policy.resolve_claim_replay(records, valid, expected_issue=147, expected_base=SHA)
        stale_record = policy.serialize_claim(claim(trusted_base_sha=OTHER_SHA))
        with self.assertRaises(policy.PolicyError):
            policy.resolve_claim_replay([stale_record], valid, expected_issue=147, expected_base=SHA)
        with self.assertRaises(policy.PolicyError):
            policy.resolve_claim_replay([valid], valid, expected_issue=147, expected_base=OTHER_SHA)


class NonActivationTests(unittest.TestCase):
    def test_module_has_no_live_side_effect_imports(self):
        with open(policy.__file__, encoding="utf-8") as stream:
            source = stream.read()
        for forbidden in ("urllib", "subprocess", "GitHubClient", "run_codex", "create_pr", "set_issue_labels"):
            self.assertNotIn(forbidden, source)
