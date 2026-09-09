import copy
import json
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json"
KINDS = {"policy", "orchestration-contract", "scientific-contract", "operational-runbook", "subsystem-contract", "developer-guide", "architecture-reference", "roadmap", "historical-record", "fixture-evidence"}
STATUSES = {"current", "historical", "retained-evidence", "deprecated"}
AUTHORITIES = {"primary", "subordinate", "reference", "none"}
RISK_FLOORS = {"green", "yellow", "red"}
FIELDS = {"path", "kind", "domain", "status", "authority", "policy_input", "protected", "risk_floor", "topics", "protecting_tests"}
IDENTIFIER = re.compile(r"^[a-z][a-z0-9-]*$")


def canonical_relative_path(value):
    if not isinstance(value, str) or not value or "\\" in value:
        return False
    if value.startswith(("/", "//")) or re.match(r"^[A-Za-z]:", value):
        return False
    parts = value.split("/")
    return all(part and part not in {".", ".."} for part in parts)


def validate_manifest(data, root=ROOT):
    if not isinstance(data, dict) or set(data) != {"schema_version", "documents"}:
        raise ValueError("invalid root keys")
    if data["schema_version"] != 1 or not isinstance(data["documents"], list):
        raise ValueError("invalid schema")
    expected = {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
    expected.update("docs/" + item.name for item in (root / "docs").glob("*.md"))
    paths = []
    for entry in data["documents"]:
        if not isinstance(entry, dict) or set(entry) != FIELDS:
            raise ValueError("invalid entry fields")
        path = entry["path"]
        if not canonical_relative_path(path):
            raise ValueError("invalid document path")
        if path not in expected or not (root / path).is_file():
            raise ValueError("undeclared document path")
        if entry["kind"] not in KINDS or entry["status"] not in STATUSES or entry["authority"] not in AUTHORITIES:
            raise ValueError("invalid document classification")
        if entry["risk_floor"] is not None and entry["risk_floor"] not in RISK_FLOORS:
            raise ValueError("invalid risk floor")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool):
            raise ValueError("invalid flag")
        for name in ("domain",):
            if (not isinstance(entry[name], str) or len(entry[name]) > 64
                    or not IDENTIFIER.fullmatch(entry[name])):
                raise ValueError("invalid identifier")
        for name in ("topics", "protecting_tests"):
            values = entry[name]
            if (not isinstance(values, list) or not values or len(values) > 16
                    or values != sorted(values) or len(values) != len(set(values))):
                raise ValueError("invalid ordered list")
        if any(not isinstance(topic, str) or len(topic) > 64 or not IDENTIFIER.fullmatch(topic) for topic in entry["topics"]):
            raise ValueError("invalid topic")
        for test_path in entry["protecting_tests"]:
            test_name = Path(test_path).name if isinstance(test_path, str) else ""
            if (not canonical_relative_path(test_path) or not test_path.startswith("tests/")
                    or not test_name.startswith("test_") or not test_name.endswith(".py")
                    or not (root / test_path).is_file()):
                raise ValueError("invalid protecting test path")
        if entry["policy_input"] and (entry["status"] != "current" or entry["authority"] not in {"primary", "subordinate"}):
            raise ValueError("invalid policy input")
        if entry["status"] in {"historical", "retained-evidence", "deprecated"} and entry["authority"] in {"primary", "subordinate"}:
            raise ValueError("invalid noncurrent authority")
        if entry["authority"] in {"reference", "none"} and entry["policy_input"]:
            raise ValueError("invalid reference policy input")
        if entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none":
            raise ValueError("invalid historical authority")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            raise ValueError("invalid fixture status")
        paths.append(path)
    if paths != sorted(paths) or len(paths) != len(set(paths)) or set(paths) != expected:
        raise ValueError("invalid final document coverage")


class AutonomousDocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))

    def test_manifest_is_valid_for_final_head_document_set(self):
        validate_manifest(self.data)

    def test_governance_index_cannot_be_excluded(self):
        altered = copy.deepcopy(self.data)
        altered["documents"] = [entry for entry in altered["documents"] if entry["path"] != "docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]
        with self.assertRaises(ValueError):
            validate_manifest(altered)

    def test_schema_field_enum_and_list_negatives_fail_closed(self):
        cases = []
        altered = copy.deepcopy(self.data)
        altered["unexpected"] = True
        cases.append(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0].pop("kind")
        cases.append(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0]["kind"] = "unknown"
        cases.append(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0]["domain"] = "not_a_domain"
        cases.append(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0]["topics"] = ["same", "same"]
        cases.append(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"].append(copy.deepcopy(altered["documents"][0]))
        cases.append(altered)
        for altered in cases:
            with self.assertRaises(ValueError):
                validate_manifest(altered)

    def test_anchor_classifications(self):
        entries = {entry["path"]: entry for entry in self.data["documents"]}
        self.assertEqual((entries["docs/AUTONOMOUS_DEVELOPMENT.md"]["kind"], entries["docs/AUTONOMOUS_DEVELOPMENT.md"]["authority"], entries["docs/AUTONOMOUS_DEVELOPMENT.md"]["policy_input"]), ("policy", "primary", True))
        self.assertEqual((entries["docs/ABAQUS_MODEL_CONTRACT.md"]["domain"], entries["docs/ABAQUS_MODEL_CONTRACT.md"]["kind"], entries["docs/ABAQUS_MODEL_CONTRACT.md"]["authority"]), ("scientific-model", "scientific-contract", "primary"))
        self.assertEqual((entries["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["authority"], entries["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["policy_input"]), ("reference", False))
        for path in ("docs/AUTONOMOUS_WORKER_RUNBOOK.md", "docs/AUTONOMOUS_TROUBLESHOOTING.md", "docs/CODEX_PROMPT_GUIDE.md"):
            self.assertEqual((entries[path]["authority"], entries[path]["policy_input"]), ("reference", False))
        for path in ("docs/A5_3_REPAIR_WORKER_ISSUE92.md", "docs/A5_3_REPAIR_GIT_IDENTITY_ISSUE94.md", "docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md", "docs/TERRA_MEDIUM_CANARY_V2.md", "docs/YELLOW_LANE_REPAIR_FIXTURE_V2.md"):
            self.assertEqual((entries[path]["authority"], entries[path]["policy_input"]), ("none", False))

    def test_noncanonical_and_non_test_protecting_paths_fail(self):
        invalid = ["tests/../README.md", "tests/./test_autonomous_document_manifest.py", "tests//test_autonomous_document_manifest.py", "tests\\test_autonomous_document_manifest.py", "/tests/x.py", "C:/tests/x.py", "//server/share/x.py", "README.md", "tests/missing.py"]
        for value in invalid:
            altered = copy.deepcopy(self.data)
            altered["documents"][0]["protecting_tests"] = [value]
            with self.subTest(value=value), self.assertRaises(ValueError):
                validate_manifest(altered)

    def test_invalid_authority_combinations_and_document_paths_fail(self):
        for path in ["/absolute", "C:/absolute", "//server/share", "docs\\ARCHITECTURE.md", "docs//ARCHITECTURE.md", "docs/./ARCHITECTURE.md", "docs/../README.md", "docs/missing.md"]:
            altered = copy.deepcopy(self.data)
            altered["documents"][0]["path"] = path
            with self.subTest(path=path), self.assertRaises(ValueError):
                validate_manifest(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0]["authority"] = "reference"
        altered["documents"][0]["policy_input"] = True
        with self.assertRaises(ValueError):
            validate_manifest(altered)
        altered = copy.deepcopy(self.data)
        altered["documents"][0]["status"] = "deprecated"
        with self.assertRaises(ValueError):
            validate_manifest(altered)

    def test_governance_index_semantics_are_whitespace_tolerant(self):
        words = " ".join((ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8").lower().split())
        for phrase in ("domain-scoped", "scientific-model contract", "runbooks are operational references", "historical records, and retained fixture evidence are not authority", "canonical repository-relative posix identity", "preflight blocker", "require reconciliation"):
            self.assertIn(phrase, words)


if __name__ == "__main__":
    unittest.main()
