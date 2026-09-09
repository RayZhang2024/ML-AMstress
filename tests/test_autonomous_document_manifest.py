import copy
import json
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json"
REQUIRED_FIELDS = {
    "path", "kind", "domain", "status", "authority", "policy_input",
    "protected", "risk_floor", "topics", "protecting_tests",
}
KINDS = {
    "policy", "orchestration-contract", "scientific-contract",
    "operational-runbook", "subsystem-contract", "developer-guide",
    "architecture-reference", "roadmap", "historical-record",
    "fixture-evidence",
}
STATUSES = {"current", "historical", "retained-evidence", "deprecated"}
AUTHORITIES = {"primary", "subordinate", "reference", "none"}
RISK_FLOORS = {"green", "yellow", "red"}
TOPIC_RE = re.compile(r"^[a-z][a-z0-9-]{0,31}$")


def expected_document_paths():
    return sorted(
        ["AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"]
        + [
            "docs/" + path.name
            for path in (ROOT / "docs").glob("*.md")
            # The D1 index is deliberately descriptive metadata, not a
            # trusted-base document that the D1 manifest must classify.
            if path.name != "AUTONOMOUS_GOVERNANCE_INDEX.md"
        ]
    )


def validate_manifest(manifest):
    if not isinstance(manifest, dict) or set(manifest) != {"schema_version", "entries"}:
        raise ValueError("manifest structure is invalid")
    if manifest["schema_version"] != 1 or not isinstance(manifest["entries"], list):
        raise ValueError("manifest version or entries is invalid")

    paths = []
    for entry in manifest["entries"]:
        if not isinstance(entry, dict) or set(entry) != REQUIRED_FIELDS:
            raise ValueError("entry fields are invalid")
        if not isinstance(entry["path"], str) or not entry["path"]:
            raise ValueError("entry path is invalid")
        paths.append(entry["path"])
        if (not isinstance(entry["kind"], str) or entry["kind"] not in KINDS
                or not isinstance(entry["status"], str) or entry["status"] not in STATUSES):
            raise ValueError("entry enum is invalid")
        if not isinstance(entry["authority"], str) or entry["authority"] not in AUTHORITIES:
            raise ValueError("entry authority is invalid")
        if not isinstance(entry["domain"], str) or not TOPIC_RE.fullmatch(entry["domain"]):
            raise ValueError("entry domain is invalid")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool):
            raise ValueError("entry flags are invalid")
        if (entry["risk_floor"] is not None
                and (not isinstance(entry["risk_floor"], str) or entry["risk_floor"] not in RISK_FLOORS)):
            raise ValueError("entry risk floor is invalid")
        for list_name in ("topics", "protecting_tests"):
            values = entry[list_name]
            if not isinstance(values, list) or len(values) > 8 or len(values) != len(set(values)):
                raise ValueError("entry list is invalid")
        if not entry["topics"] or any(not isinstance(topic, str) or not TOPIC_RE.fullmatch(topic) for topic in entry["topics"]):
            raise ValueError("entry topics are invalid")
        for relative_test in entry["protecting_tests"]:
            if not isinstance(relative_test, str):
                raise ValueError("protecting test reference is invalid")
            candidate = ROOT / relative_test
            if not relative_test.startswith("tests/") or not candidate.is_file():
                raise ValueError("protecting test reference is invalid")
        if not (ROOT / entry["path"]).is_file():
            raise ValueError("document reference is invalid")
        if entry["status"] in {"historical", "deprecated"} or entry["kind"] in {"historical-record", "fixture-evidence"}:
            if entry["policy_input"] or entry["authority"] in {"primary", "subordinate"}:
                raise ValueError("non-current material has authority")
        if entry["kind"] == "fixture-evidence" and entry["authority"] != "none":
            raise ValueError("fixture authority is invalid")
        if entry["kind"] == "historical-record" and entry["policy_input"]:
            raise ValueError("historical policy input is invalid")

    if paths != sorted(paths) or len(paths) != len(set(paths)):
        raise ValueError("entry paths are not unique and ordered")
    if paths != expected_document_paths():
        raise ValueError("document coverage is invalid")


class AutonomousDocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with MANIFEST_PATH.open(encoding="utf-8") as manifest_file:
            cls.manifest = json.load(manifest_file)

    def test_schema_and_document_coverage(self):
        validate_manifest(self.manifest)

    def test_anchor_classifications(self):
        entries = {entry["path"]: entry for entry in self.manifest["entries"]}
        expected = {
            "docs/AUTONOMOUS_DEVELOPMENT.md": ("policy", "current", "primary", True),
            "docs/AUTONOMOUS_ORCHESTRATION.md": ("orchestration-contract", "current", "subordinate", True),
            "docs/AUTONOMOUS_WORKER_RUNBOOK.md": ("operational-runbook", "current", "reference", False),
            "docs/AUTONOMOUS_TROUBLESHOOTING.md": ("operational-runbook", "current", "reference", False),
            "docs/CODEX_PROMPT_GUIDE.md": ("developer-guide", "current", "reference", False),
            "docs/ABAQUS_MODEL_CONTRACT.md": ("scientific-contract", "current", "primary", True),
            "docs/A5_3_REPAIR_WORKER_ISSUE92.md": ("historical-record", "historical", "none", False),
            "docs/A5_3_REPAIR_GIT_IDENTITY_ISSUE94.md": ("historical-record", "historical", "none", False),
        }
        for path, classification in expected.items():
            entry = entries[path]
            self.assertEqual(classification, (entry["kind"], entry["status"], entry["authority"], entry["policy_input"]))
        for path in ("docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md", "docs/TERRA_MEDIUM_CANARY_V2.md", "docs/YELLOW_LANE_REPAIR_FIXTURE_V2.md"):
            entry = entries[path]
            self.assertEqual(("fixture-evidence", "retained-evidence", "none", False), (entry["kind"], entry["status"], entry["authority"], entry["policy_input"]))

    def test_invalid_metadata_is_rejected(self):
        cases = []
        duplicate = copy.deepcopy(self.manifest)
        duplicate["entries"].append(copy.deepcopy(duplicate["entries"][0]))
        cases.append(duplicate)
        missing = copy.deepcopy(self.manifest)
        missing["entries"].pop()
        cases.append(missing)
        bad_test = copy.deepcopy(self.manifest)
        bad_test["entries"][0]["protecting_tests"] = ["tests/not-present.py"]
        cases.append(bad_test)
        bad_topic = copy.deepcopy(self.manifest)
        bad_topic["entries"][0]["topics"] = ["Not stable"]
        cases.append(bad_topic)
        duplicate_topic = copy.deepcopy(self.manifest)
        duplicate_topic["entries"][0]["topics"] = ["issue-contract", "issue-contract"]
        cases.append(duplicate_topic)
        duplicate_test = copy.deepcopy(self.manifest)
        duplicate_test["entries"][0]["protecting_tests"] = [
            "tests/test_autonomous_document_manifest.py",
            "tests/test_autonomous_document_manifest.py",
        ]
        cases.append(duplicate_test)
        unknown_kind = copy.deepcopy(self.manifest)
        unknown_kind["entries"][0]["kind"] = "unknown"
        cases.append(unknown_kind)
        bad_risk = copy.deepcopy(self.manifest)
        bad_risk["entries"][0]["risk_floor"] = "orange"
        cases.append(bad_risk)
        bad_flag = copy.deepcopy(self.manifest)
        bad_flag["entries"][0]["policy_input"] = "true"
        cases.append(bad_flag)
        bad_fixture = copy.deepcopy(self.manifest)
        next(entry for entry in bad_fixture["entries"] if entry["kind"] == "fixture-evidence")["authority"] = "reference"
        cases.append(bad_fixture)
        bad_historical = copy.deepcopy(self.manifest)
        next(entry for entry in bad_historical["entries"] if entry["kind"] == "historical-record")["policy_input"] = True
        cases.append(bad_historical)
        for manifest in cases:
            with self.assertRaises(ValueError):
                validate_manifest(manifest)

    def test_governance_index_states_scoped_fail_closed_precedence(self):
        index = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8").lower()
        for concept in ("taxonomy", "scientific-model domain", "runbook", "historical", "fixture", "preflight blocker", "requires reconciliation"):
            self.assertIn(concept, index)


if __name__ == "__main__":
    unittest.main()
