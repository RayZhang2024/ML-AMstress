import json
import re
import subprocess
import unicodedata
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json"
AUTHORIZED_CANDIDATES = frozenset((
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
))
FIELDS = frozenset((
    "path", "kind", "domain", "status", "authority", "policy_input",
    "protected", "risk_floor", "topics", "protecting_tests",
))
ENUMS = {
    "kind": {"policy", "contract", "reference", "historical"},
    "domain": {"autonomy", "project", "scientific-model"},
    "status": {"current", "retained-evidence"},
    "authority": {"primary", "subordinate", "reference", "none"},
    "risk_floor": {"green", "yellow", "red"},
}


class ManifestError(ValueError):
    pass


def _exact_path(value):
    if not isinstance(value, str) or not value or value.startswith(("/", "\\")):
        raise ManifestError("invalid repository identity")
    if "\\" in value or ":" in value or value.startswith("//"):
        raise ManifestError("invalid repository identity")
    parts = value.split("/")
    if any(part in ("", ".", "..") for part in parts):
        raise ManifestError("invalid repository identity")
    if unicodedata.normalize("NFC", value) != value:
        raise ManifestError("noncanonical repository identity")
    return value


def _collision_key(path):
    return unicodedata.normalize("NFC", path).casefold()


def parse_git_z_inventory(raw, inventory, allow_empty=False):
    if not isinstance(raw, bytes):
        raise ManifestError("malformed Git inventory")
    if not raw:
        if allow_empty:
            return ()
        raise ManifestError("empty Git inventory")
    if not raw.endswith(b"\0"):
        raise ManifestError("unterminated Git inventory")
    records = raw[:-1].split(b"\0")
    if any(not record for record in records):
        raise ManifestError("interior empty Git record")
    try:
        paths = tuple(record.decode("utf-8", "strict") for record in records)
    except UnicodeDecodeError as error:
        raise ManifestError("undecodable Git inventory") from error
    if len(paths) != len(set(paths)):
        raise ManifestError("duplicate Git identity")
    for path in paths:
        _exact_path(path)
    keys = [_collision_key(path) for path in paths]
    if len(keys) != len(set(keys)):
        raise ManifestError("aliased Git identities")
    return paths


def acquire_git_z_inventory(command, inventory, allow_empty=False, runner=subprocess.run):
    try:
        result = runner(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        raw = result.stdout
    except (OSError, subprocess.SubprocessError, AttributeError, TypeError) as error:
        raise ManifestError("Git inventory acquisition failed") from error
    return parse_git_z_inventory(raw, inventory, allow_empty)


def validate_two_phase_identities(tracked, untracked, phase):
    tracked = parse_git_z_inventory(tracked, "tracked")
    untracked = parse_git_z_inventory(untracked, "untracked", allow_empty=True)
    combined = tracked + untracked
    keys = [_collision_key(path) for path in combined]
    if len(keys) != len(set(keys)):
        raise ManifestError("combined aliased Git identities")
    if phase == "before commit":
        if any(path not in AUTHORIZED_CANDIDATES for path in untracked):
            raise ManifestError("unauthorized candidate identity")
    elif phase == "after commit":
        if untracked or any(path not in tracked for path in AUTHORIZED_CANDIDATES):
            raise ManifestError("candidate allowance inactive")
    else:
        raise ManifestError("unknown identity phase")
    return frozenset(combined)


def validate_manifest_identity_references(manifest_paths, protecting_tests, inventory):
    """Require exact Git identities; this intentionally does no path resolution."""
    for path in tuple(manifest_paths) + tuple(protecting_tests):
        _exact_path(path)
        if path not in inventory:
            raise ManifestError("missing exact Git identity")


def governance_has_two_phase_semantics(prose):
    text = " ".join(prose.lower().split())
    before = bool(re.search(r"\bpre[ -]?commit\b|\b(before|prior to)\b.{0,40}\bcommit(?:s|ted|ting)?\b", text))
    after = bool(re.search(r"\bpost[ -]?commit\b|\b(after|once)\b.{0,40}\bcommit(?:s|ted|ting)?\b", text))
    candidates = bool(re.search(r"\bcandidates?\b", text)) and bool(re.search(r"\b(exact|authorized)\b", text)) and bool(re.search(r"\bgit[ -]?reported\b", text))
    tracked = bool(re.search(r"\btracked\b", text)) and bool(re.search(r"\b(candidate )?allowance\b.{0,40}\binactive\b|\binactive\b.{0,40}\b(candidate )?allowance\b", text))
    return before and after and candidates and tracked


class AutonomousDocumentManifestTests(unittest.TestCase):
    def setUp(self):
        self.manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
        self.entries = self.manifest["documents"]
        self.by_path = {entry["path"]: entry for entry in self.entries}

    def test_schema_coverage_order_and_tracked_identity(self):
        self.assertEqual(1, self.manifest["schema_version"])
        paths = [entry["path"] for entry in self.entries]
        self.assertEqual(sorted(paths), paths)
        self.assertEqual(len(paths), len(set(paths)))
        expected = set(ACCEPTED_DOCS()) | {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
        self.assertEqual(expected, set(paths))
        for entry in self.entries:
            self.assertEqual(FIELDS, frozenset(entry))
            for field, values in ENUMS.items():
                self.assertIn(entry[field], values)
            self.assertIsInstance(entry["policy_input"], bool)
            self.assertIsInstance(entry["protected"], bool)
            if entry["authority"] == "none":
                self.assertEqual("retained-evidence", entry["status"])
                self.assertFalse(entry["policy_input"])
            else:
                self.assertEqual("current", entry["status"])
                self.assertEqual(
                    entry["authority"] in {"primary", "subordinate"},
                    entry["policy_input"],
                )
            for field in ("topics", "protecting_tests"):
                self.assertEqual(sorted(entry[field]), entry[field])
                self.assertEqual(len(entry[field]), len(set(entry[field])))
                for path in entry[field]:
                    _exact_path(path)
        self.assertEqual(["canary", "terra"], self.by_path["docs/TERRA_MEDIUM_CANARY_V2.md"]["topics"])

    def test_authority_anchors_and_domain_scoping(self):
        expected = {
            "docs/AUTONOMOUS_DEVELOPMENT.md": "primary",
            "docs/AUTONOMOUS_ORCHESTRATION.md": "subordinate",
            ".github/ISSUE_TEMPLATE/autonomous-work.md": "subordinate",
            "docs/AUTONOMOUS_WORKER_RUNBOOK.md": "reference",
            "docs/AUTONOMOUS_TROUBLESHOOTING.md": "reference",
            "docs/CODEX_PROMPT_GUIDE.md": "reference",
            "docs/ABAQUS_MODEL_CONTRACT.md": "primary",
            "docs/AUTONOMOUS_GOVERNANCE_INDEX.md": "reference",
        }
        for path, authority in expected.items():
            self.assertEqual(authority, self.by_path[path]["authority"])
        self.assertEqual("scientific-model", self.by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["domain"])
        for path, entry in self.by_path.items():
            if entry["status"] == "retained-evidence":
                self.assertEqual("none", entry["authority"], path)
                self.assertFalse(entry["policy_input"], path)

    def test_git_inventory_is_fail_closed(self):
        for inventory in ("tracked", "untracked"):
            self.assertEqual(("docs/A.md",), parse_git_z_inventory(b"docs/A.md\0", inventory))
            for raw in (b"docs/A.md", b"docs/A.md\0docs/A.md\0", b"docs/A.md\0\0"):
                with self.assertRaises(ManifestError):
                    parse_git_z_inventory(raw, inventory)
        with self.assertRaises(ManifestError):
            acquire_git_z_inventory(("git",), "tracked", runner=lambda *a, **k: (_ for _ in ()).throw(OSError()))

    def test_alias_collisions_fail_in_each_inventory_and_combined_inventory(self):
        for raw in (b"docs/A.md\0docs/a.md\0", "docs/caf\u00e9.md\0docs/cafe\u0301.md\0".encode("utf-8")):
            with self.assertRaises(ManifestError):
                parse_git_z_inventory(raw, "tracked")
            with self.assertRaises(ManifestError):
                parse_git_z_inventory(raw, "untracked", allow_empty=True)
        with self.assertRaises(ManifestError):
            validate_two_phase_identities(b"docs/A.md\0", b"docs/a.md\0", "before commit")

    def test_candidate_allowance_is_exact_and_deactivates_after_commit(self):
        tracked = b"AGENTS.md\0"
        candidates = b"docs/AUTONOMOUS_DOCUMENT_MANIFEST.json\0docs/AUTONOMOUS_GOVERNANCE_INDEX.md\0tests/test_autonomous_document_manifest.py\0"
        validate_two_phase_identities(tracked, candidates, "before commit")
        with self.assertRaises(ManifestError):
            validate_two_phase_identities(tracked, b"tests/test_AUTONOMOUS_document_manifest.py\0", "before commit")
        with self.assertRaises(ManifestError):
            validate_two_phase_identities(tracked, candidates, "after commit")
        all_tracked = tracked + candidates
        validate_two_phase_identities(all_tracked, b"", "after commit")
        with self.assertRaises(ManifestError):
            validate_manifest_identity_references(
                ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",),
                ("tests/test_AUTONOMOUS_document_manifest.py",),
                frozenset(AUTHORIZED_CANDIDATES),
            )

    def test_governance_prose_has_semantic_two_phase_contract(self):
        prose = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8")
        self.assertTrue(governance_has_two_phase_semantics(prose))
        accepted = (
            "Before commit authorized candidate identities are exact Git-reported values; after commit they are tracked and the allowance is inactive.",
            "Before the worker commits, an authorized candidate is exact Git-reported; once committed it is tracked and the candidate allowance is inactive.",
            "In the pre-commit phase an authorized candidate is exact Git-reported; in post-commit it is tracked and the candidate allowance is inactive.",
        )
        for text in accepted:
            self.assertTrue(governance_has_two_phase_semantics(text))
        self.assertFalse(governance_has_two_phase_semantics("Before commit candidates are allowed."))
        self.assertFalse(governance_has_two_phase_semantics(
            "Before commit and after commit are phases, but no candidate or tracked identity is described."
        ))
        self.assertFalse(governance_has_two_phase_semantics(
            "Before commit an exact Git-reported candidate is allowed; after commit it remains a candidate."
        ))


def ACCEPTED_DOCS():
    result = subprocess.run(("git", "ls-files", "docs/*.md"), cwd=ROOT, check=True, stdout=subprocess.PIPE, text=True, encoding="utf-8")
    return tuple(line for line in result.stdout.splitlines() if line) + ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",)


if __name__ == "__main__":
    unittest.main()
