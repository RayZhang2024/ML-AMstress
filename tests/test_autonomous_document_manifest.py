"""Descriptive D1 manifest validation; this module is not a runtime consumer."""

import json
import re
import subprocess
import unicodedata
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json"
GOVERNANCE_INDEX = ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md"
AUTHORIZED_CANDIDATES = (
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
)
PHASES = ("pre-commit", "post-commit")
ENTRY_FIELDS = {
    "path", "kind", "domain", "status", "authority", "policy_input",
    "protected", "risk_floor", "topics", "protecting_tests",
}
KINDS = {"policy", "orchestration-contract", "scientific-contract",
         "operational-runbook", "subsystem-contract", "developer-guide",
         "architecture-reference", "roadmap", "historical-record", "fixture-evidence"}
STATUSES = {"current", "historical", "retained-evidence", "deprecated"}
AUTHORITIES = {"primary", "subordinate", "reference", "none"}
RISKS = {"green", "yellow", "red"}
IDENTIFIER = re.compile(r"^[a-z][a-z0-9-]*$")


class ManifestError(ValueError):
    """A bounded failure in the descriptive manifest validation."""


def _git(command):
    result = subprocess.run(
        ["git", *command], cwd=ROOT, stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL, check=False,
    )
    return result.returncode, result.stdout


def parse_git_inventory(result, allow_empty=False):
    """Validate Git's NUL-delimited output before any identity processing."""
    code, raw = result
    if code != 0 or not isinstance(raw, bytes):
        raise ManifestError("Git inventory acquisition failed")
    try:
        text = raw.decode("utf-8", "strict")
    except UnicodeDecodeError as exc:
        raise ManifestError("Git inventory is not UTF-8") from exc
    records = text.split("\0")
    if records[-1] == "":
        records.pop()  # A single format-required final delimiter is permitted.
    if not records and not allow_empty:
        raise ManifestError("Git inventory is empty")
    if any(not item for item in records):
        raise ManifestError("Git inventory has an empty record")
    if len(records) != len(set(records)):
        raise ManifestError("Git inventory has duplicate identities")
    for item in records:
        validate_path(item)
    return tuple(records)


def validate_path(path):
    if not isinstance(path, str) or not path or "\\" in path or path.startswith(("/", "//")):
        raise ManifestError("unsafe repository path")
    if re.match(r"^[A-Za-z]:", path) or "//" in path:
        raise ManifestError("unsafe repository path")
    pieces = path.split("/")
    if any(piece in ("", ".", "..") for piece in pieces):
        raise ManifestError("noncanonical repository path")
    if unicodedata.normalize("NFC", path) != path:
        raise ManifestError("noncanonical Unicode path")


def git_inventories(git_call=_git):
    """Acquire tracked and non-ignored inventories separately, fail closed."""
    tracked = parse_git_inventory(git_call(["ls-files", "-z"]))
    untracked = parse_git_inventory(
        git_call(["ls-files", "-z", "--others", "--exclude-standard"]),
        allow_empty=True,
    )
    return tracked, untracked


def final_identity_inventory(phase, tracked, untracked):
    if phase not in PHASES:
        raise ManifestError("unknown validation phase")
    tracked = parse_git_inventory((0, "\0".join(tracked).encode() + b"\0"))
    # Empty non-ignored output is valid at this boundary, unlike tracked output.
    raw_untracked = b"\0".join(item.encode() for item in untracked)
    if raw_untracked:
        raw_untracked += b"\0"
        untracked = parse_git_inventory((0, raw_untracked))
    else:
        untracked = ()
    if set(tracked) & set(untracked):
        raise ManifestError("tracked and untracked identities overlap")
    if phase == "pre-commit":
        extras = [item for item in untracked if item not in AUTHORIZED_CANDIDATES]
        if extras:
            raise ManifestError("unexpected untracked identity")
        return tuple(sorted((*tracked, *untracked)))
    if untracked:
        raise ManifestError("post-commit identity cannot be untracked")
    if any(item not in tracked for item in AUTHORIZED_CANDIDATES):
        raise ManifestError("post-commit D1 identity is not tracked")
    return tracked


def semantic_two_phase_prose(text):
    normalized = re.sub(r"\s+", " ", text.casefold())
    before = any(term in normalized for term in ("before a worker commits", "before commit", "prior to commit"))
    after = any(term in normalized for term in ("once the implementation is committed", "after commit", "final head"))
    candidates = "candidate" in normalized and ("git-reported" in normalized or "git reported" in normalized)
    tracked = "tracked" in normalized and ("inactive" in normalized or "allowance" in normalized)
    return before and after and candidates and tracked


def validate_manifest(data, inventory):
    if set(data) != {"schema_version", "documents"} or data["schema_version"] != 1:
        raise ManifestError("invalid manifest schema")
    entries = data["documents"]
    if not isinstance(entries, list) or not entries:
        raise ManifestError("documents must be a nonempty list")
    paths = []
    for entry in entries:
        if set(entry) != ENTRY_FIELDS:
            raise ManifestError("invalid entry fields")
        path = entry["path"]
        validate_path(path)
        paths.append(path)
        if entry["kind"] not in KINDS or entry["status"] not in STATUSES or entry["authority"] not in AUTHORITIES:
            raise ManifestError("invalid manifest enum")
        if entry["risk_floor"] is not None and entry["risk_floor"] not in RISKS:
            raise ManifestError("invalid risk floor")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool):
            raise ManifestError("invalid manifest boolean")
        for field in ("domain",):
            if not isinstance(entry[field], str) or not IDENTIFIER.fullmatch(entry[field]):
                raise ManifestError("invalid stable identifier")
        for field in ("topics", "protecting_tests"):
            values = entry[field]
            if not isinstance(values, list) or not values or values != sorted(values) or len(values) != len(set(values)):
                raise ManifestError("non-deterministic list")
            for value in values:
                if field == "topics":
                    if not isinstance(value, str) or not IDENTIFIER.fullmatch(value):
                        raise ManifestError("invalid topic")
                else:
                    validate_path(value)
                    if (not value.startswith("tests/") or not value.endswith(".py")
                            or value != "tests/test_autonomous_document_manifest.py"
                            or value not in inventory):
                        raise ManifestError("invalid protecting test")
        policy_allowed = entry["status"] == "current" and entry["authority"] in {"primary", "subordinate"}
        if entry["policy_input"] != policy_allowed:
            raise ManifestError("inconsistent policy input")
        if entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none":
            raise ManifestError("non-authoritative evidence required")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            raise ManifestError("fixture evidence must be retained")
        if path not in inventory:
            raise ManifestError("manifest path lacks exact Git identity")
    if paths != sorted(paths) or len(paths) != len(set(paths)):
        raise ManifestError("manifest paths are not deterministic")


class AutonomousDocumentManifestTests(unittest.TestCase):
    def setUp(self):
        self.data = json.loads(MANIFEST.read_text(encoding="utf-8"))
        self.tracked, self.untracked = git_inventories()

    def test_pre_commit_manifest_and_coverage(self):
        inventory = final_identity_inventory("pre-commit", self.tracked, self.untracked)
        validate_manifest(self.data, inventory)
        expected = {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
        expected |= {"docs/" + item.name for item in (ROOT / "docs").glob("*.md")}
        self.assertEqual(expected, {entry["path"] for entry in self.data["documents"]})

    def test_anchor_classifications(self):
        by_path = {entry["path"]: entry for entry in self.data["documents"]}
        self.assertEqual(("primary", True), (by_path["docs/AUTONOMOUS_DEVELOPMENT.md"]["authority"], by_path["docs/AUTONOMOUS_DEVELOPMENT.md"]["policy_input"]))
        self.assertEqual(("scientific-model", "primary", True), (by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["domain"], by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["authority"], by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["policy_input"]))
        self.assertEqual(("reference", False), (by_path["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["authority"], by_path["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["policy_input"]))

    def test_raw_inventory_failures_are_not_repaired(self):
        for result in ((1, b"docs/a.md\0"), (0, b"docs/a.md\0docs/a.md\0"), (0, b"docs/a.md\0\0docs/b.md\0"), (0, b"../docs/a.md\0")):
            with self.assertRaises(ManifestError):
                parse_git_inventory(result)
        self.assertEqual(("docs/a.md",), parse_git_inventory((0, b"docs/a.md\0")))

    def test_acquisition_failure_reaches_the_production_path(self):
        def failed_git_call(_command):
            return 1, b""
        with self.assertRaises(ManifestError):
            git_inventories(failed_git_call)

        calls = []
        def malformed_untracked(command):
            calls.append(command)
            return (0, b"AGENTS.md\0") if len(calls) == 1 else (0, b"bad\\path\0")
        with self.assertRaises(ManifestError):
            git_inventories(malformed_untracked)

    def test_candidate_and_post_commit_rules(self):
        tracked = ("AGENTS.md", "README.md")
        self.assertIn("tests/test_autonomous_document_manifest.py", final_identity_inventory("pre-commit", tracked, AUTHORIZED_CANDIDATES))
        with self.assertRaises(ManifestError):
            final_identity_inventory("pre-commit", tracked, ("tests/test_AUTONOMOUS_document_manifest.py",))
        with self.assertRaises(ManifestError):
            final_identity_inventory("pre-commit", tracked, ("notes.md",))
        with self.assertRaises(ManifestError):
            final_identity_inventory("post-commit", (*tracked, *AUTHORIZED_CANDIDATES), (AUTHORIZED_CANDIDATES[0],))
        self.assertEqual((*tracked, *AUTHORIZED_CANDIDATES), final_identity_inventory("post-commit", (*tracked, *AUTHORIZED_CANDIDATES), ()))

    def test_semantic_governance_prose(self):
        self.assertTrue(semantic_two_phase_prose(GOVERNANCE_INDEX.read_text(encoding="utf-8")))
        variant = "Before commit Git-reported candidates are allowed. After commit, tracked identity is required and the candidate allowance is inactive."
        self.assertTrue(semantic_two_phase_prose(variant))
        self.assertFalse(semantic_two_phase_prose("Candidates are tracked."))


if __name__ == "__main__":
    unittest.main()
