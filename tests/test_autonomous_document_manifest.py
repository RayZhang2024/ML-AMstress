import json
import subprocess
import unicodedata
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json"
D1_PATHS = {
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
}
FIELDS = {"path", "kind", "domain", "status", "authority", "policy_input", "protected", "risk_floor", "topics", "protecting_tests"}
KINDS = {"policy", "orchestration-contract", "scientific-contract", "operational-runbook", "subsystem-contract", "developer-guide", "architecture-reference", "roadmap", "historical-record", "fixture-evidence"}
STATUSES = {"current", "historical", "retained-evidence", "deprecated"}
AUTHORITIES = {"primary", "subordinate", "reference", "none"}
RISKS = {"green", "yellow", "red"}


class ManifestError(ValueError):
    pass


def parse_z_inventory(raw):
    if not isinstance(raw, bytes) or (raw and not raw.endswith(b"\0")):
        raise ManifestError("invalid-git-z")
    if not raw:
        return []
    try:
        values = raw[:-1].split(b"\0")
        paths = [value.decode("utf-8", "strict") for value in values]
    except UnicodeError:
        raise ManifestError("invalid-git-z")
    if not paths or any(not value for value in paths) or len(paths) != len(set(paths)):
        raise ManifestError("invalid-git-z")
    for path in paths:
        validate_path_syntax(path)
    aliases = [path.casefold() for path in paths]
    normalized = [unicodedata.normalize("NFC", path) for path in paths]
    if len(aliases) != len(set(aliases)) or len(normalized) != len(set(normalized)):
        raise ManifestError("identity-alias")
    return paths


def git_inventory(args):
    result = subprocess.run(["git"] + args, cwd=str(ROOT), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode:
        raise ManifestError("git-acquisition")
    return parse_z_inventory(result.stdout)


def validate_path_syntax(path):
    if not isinstance(path, str) or not path or "\\" in path or path.startswith("/") or path.startswith("//") or ":" in path:
        raise ManifestError("unsafe-path")
    parts = path.split("/")
    if any(not part or part in {".", ".."} for part in parts):
        raise ManifestError("unsafe-path")


def validate_identity(path, tracked, candidates, *, protecting=False, committed=False):
    validate_path_syntax(path)
    if path not in tracked:
        if committed or path not in candidates or path not in D1_PATHS:
            raise ManifestError("missing-identity")
    if protecting:
        if not path.startswith("tests/") or not path.rsplit("/", 1)[-1].startswith("test_") or not path.endswith(".py"):
            raise ManifestError("invalid-protecting-test")
        if path == "tests/test_AUTONOMOUS_document_manifest.py":
            raise ManifestError("invalid-protecting-test")


def validate_phase(tracked, candidates, committed):
    tracked, candidates = set(tracked), set(candidates)
    if not tracked:
        raise ManifestError("empty-tracked-inventory")
    if committed:
        if candidates or not D1_PATHS <= tracked:
            raise ManifestError("post-commit-identity")
    elif candidates != D1_PATHS:
        raise ManifestError("pre-commit-candidates")
    combined = list(tracked | candidates)
    folded = [item.casefold() for item in combined]
    normalized = [unicodedata.normalize("NFC", item) for item in combined]
    if len(folded) != len(set(folded)) or len(normalized) != len(set(normalized)):
        raise ManifestError("identity-alias")


class AutonomousDocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
        cls.entries = cls.manifest["documents"]

    def test_schema_coverage_order_and_anchors(self):
        self.assertEqual(1, self.manifest["schema_version"])
        paths = [entry["path"] for entry in self.entries]
        expected = {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
        expected |= {"docs/" + path.name for path in (ROOT / "docs").glob("*.md")}
        self.assertEqual(expected, set(paths))
        self.assertEqual(paths, sorted(paths))
        anchors = {entry["path"]: entry for entry in self.entries}
        self.assertEqual(("policy", "primary"), (anchors["docs/AUTONOMOUS_DEVELOPMENT.md"]["kind"], anchors["docs/AUTONOMOUS_DEVELOPMENT.md"]["authority"]))
        self.assertEqual(("scientific-contract", "primary"), (anchors["docs/ABAQUS_MODEL_CONTRACT.md"]["kind"], anchors["docs/ABAQUS_MODEL_CONTRACT.md"]["authority"]))
        self.assertEqual(["canary", "terra"], anchors["docs/TERRA_MEDIUM_CANARY_V2.md"]["topics"])

    def test_schema_and_authority_invariants(self):
        self.assertEqual(KINDS, {entry["kind"] for entry in self.entries})
        for entry in self.entries:
            self.assertEqual(FIELDS, set(entry))
            self.assertIn(entry["kind"], KINDS)
            self.assertIn(entry["status"], STATUSES)
            self.assertIn(entry["authority"], AUTHORITIES)
            self.assertIn(entry["risk_floor"], RISKS)
            self.assertRegex(entry["domain"], r"^[a-z][a-z0-9-]*$")
            for key in ("topics", "protecting_tests"):
                self.assertEqual(entry[key], sorted(set(entry[key])))
            for topic in entry["topics"]:
                self.assertRegex(topic, r"^[a-z][a-z0-9-]*$")
            if entry["policy_input"]:
                self.assertEqual("current", entry["status"])
                self.assertIn(entry["authority"], {"primary", "subordinate"})
            if entry["kind"] in {"historical-record", "fixture-evidence"}:
                self.assertEqual("none", entry["authority"])
                self.assertFalse(entry["policy_input"])
            if entry["kind"] == "fixture-evidence":
                self.assertEqual("retained-evidence", entry["status"])

    def test_live_pre_commit_identity_and_protecting_tests(self):
        tracked = git_inventory(["ls-files", "-z"])
        candidates = git_inventory(["ls-files", "--others", "--exclude-standard", "-z"])
        validate_phase(tracked, candidates, committed=False)
        for entry in self.entries:
            validate_identity(entry["path"], tracked, candidates)
            for test in entry["protecting_tests"]:
                validate_identity(test, tracked, candidates, protecting=True)

    def test_identity_parser_rejects_malformed_and_alias_evidence(self):
        for raw in (b"docs/a.md", b"docs/a.md\0\0", b"docs/a.md\0docs/a.md\0", b"../a\0", b"docs\\a\0", b"\xff\0", b"tests/a\0tests/A\0", "docs/e\u0301.md\0docs/é.md\0".encode("utf-8")):
            with self.assertRaises(ManifestError):
                parse_z_inventory(raw)

    def test_phase_and_protecting_identity_regressions(self):
        tracked = {"AGENTS.md", "tests/test_issue22_governance_docs.py"}
        with self.assertRaises(ManifestError): validate_phase(set(), D1_PATHS, False)
        with self.assertRaises(ManifestError): validate_phase(tracked, set(), False)
        with self.assertRaises(ManifestError): validate_phase(tracked, set(D1_PATHS) - {"docs/AUTONOMOUS_GOVERNANCE_INDEX.md"}, False)
        with self.assertRaises(ManifestError): validate_phase(tracked, D1_PATHS | {"docs/extra.md"}, False)
        validate_phase(tracked | D1_PATHS, set(), True)
        for bad in ("tests/test_AUTONOMOUS_document_manifest.py", "tests/missing.py", "docs/test_x.py", "tests/x.txt", "tests/helper.py", "../tests/test_x.py", "tests\\test_x.py"):
            with self.assertRaises(ManifestError): validate_identity(bad, tracked, set(), protecting=True)
        with self.assertRaises(ManifestError): validate_identity("tests/test_AUTONOMOUS_document_manifest.py", tracked, D1_PATHS, protecting=True)

    def test_git_acquisition_failure_is_bounded(self):
        class FailedResult:
            returncode = 1
            stdout = b""
            stderr = b"untrusted diagnostic"
        with patch("subprocess.run", return_value=FailedResult()):
            with self.assertRaisesRegex(ManifestError, "git-acquisition"):
                git_inventory(["ls-files", "-z"])

    def test_governance_index_binds_phase_semantics_and_reconciliation(self):
        text = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8").lower()
        for phrase in ("before commit", "before the worker commits", "pre-commit", "only the three exact authorized d1 candidate additions", "after commit", "once committed", "post-commit", "all three d1 paths must be ordinary exact tracked identities", "candidate allowance is active", "taxonomy", "historical records", "roadmaps", "fixture evidence", "explicit reconciliation or escalation"):
            self.assertIn(phrase, text)
        self.assertLess(text.index("before commit"), text.index("only the three exact authorized d1 candidate additions"))
        self.assertLess(text.index("after commit"), text.index("all three d1 paths must be ordinary exact tracked identities"))


if __name__ == "__main__":
    unittest.main()
