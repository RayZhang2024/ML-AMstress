"""Descriptive D1 manifest validation; it deliberately changes no runtime policy."""

import json
import re
import subprocess
import unicodedata
import unittest
from pathlib import Path, PurePosixPath
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
D1_PATHS = frozenset((
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
))
FIELDS = frozenset((
    "path", "kind", "domain", "status", "authority", "policy_input",
    "protected", "risk_floor", "topics", "protecting_tests",
))
KINDS = frozenset((
    "policy", "orchestration-contract", "scientific-contract",
    "operational-runbook", "subsystem-contract", "developer-guide",
    "architecture-reference", "roadmap", "historical-record", "fixture-evidence",
))
STATUSES = frozenset(("current", "historical", "retained-evidence", "deprecated"))
AUTHORITIES = frozenset(("primary", "subordinate", "reference", "none"))
RISK_FLOORS = frozenset(("green", "yellow", "red"))
IDENTIFIER = re.compile(r"^[a-z][a-z0-9-]*$")


class ManifestError(ValueError):
    """A deliberately bounded diagnostic for untrusted repository evidence."""


def fail(code):
    raise ManifestError(code)


def exact_path(value):
    if not isinstance(value, str) or not value or "\\" in value:
        fail("invalid Git identity")
    if value.startswith(("/", "//")) or re.match(r"^[A-Za-z]:", value):
        fail("invalid Git identity")
    parts = value.split("/")
    if any(not part or part in (".", "..") for part in parts):
        fail("invalid Git identity")
    # PurePosixPath is only a second structural check; it never supplies identity.
    if str(PurePosixPath(value)) != value:
        fail("invalid Git identity")
    return value


def comparison_key(value):
    return unicodedata.normalize("NFC", value).casefold()


def parse_git_z(raw, inventory, allow_empty):
    if not isinstance(raw, bytes):
        fail("malformed Git inventory")
    if not raw:
        if allow_empty:
            return ()
        fail("empty tracked inventory")
    if not raw.endswith(b"\0"):
        fail("unterminated Git inventory")
    records = raw[:-1].split(b"\0")
    if any(not record for record in records):
        fail("empty Git inventory record")
    try:
        paths = tuple(record.decode("utf-8", "strict") for record in records)
    except UnicodeDecodeError:
        fail("undecodable Git inventory")
    if len(set(paths)) != len(paths):
        fail("duplicate Git identity")
    for path in paths:
        exact_path(path)
    keys = [comparison_key(path) for path in paths]
    if len(set(keys)) != len(keys):
        fail("aliased Git identity")
    return paths


def git_inventory(command, inventory, allow_empty):
    try:
        result = subprocess.run(
            command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            check=False,
        )
    except OSError:
        fail("Git inventory acquisition failed")
    if result.returncode:
        fail("Git inventory acquisition failed")
    return parse_git_z(result.stdout, inventory, allow_empty)


def live_inventories():
    tracked = git_inventory(("git", "ls-files", "-z"), "tracked", False)
    untracked = git_inventory(
        ("git", "ls-files", "--others", "--exclude-standard", "-z"),
        "untracked", True,
    )
    ignored = git_inventory(
        ("git", "ls-files", "--others", "--ignored", "--exclude-standard", "-z"),
        "ignored", True,
    )
    tracked, untracked = validate_inventory_pair(tracked, untracked)
    if any(comparison_key(path) in {comparison_key(candidate) for candidate in D1_PATHS} for path in ignored):
        fail("ignored D1 identity")
    return tracked, untracked


def validate_inventory_pair(tracked, untracked):
    """Check raw accepted identities again across the two inventories."""
    all_paths = tuple(tracked) + tuple(untracked)
    keys = [comparison_key(path) for path in all_paths]
    if len(set(keys)) != len(keys):
        fail("aliased combined Git identity")
    if set(tracked).intersection(untracked):
        fail("mixed Git identity")
    return frozenset(tracked), frozenset(untracked)


def determine_phase(tracked, untracked, ignored=frozenset()):
    if set(ignored).intersection(D1_PATHS):
        fail("ignored D1 identity")
    tracked_d1 = tracked.intersection(D1_PATHS)
    untracked_d1 = untracked.intersection(D1_PATHS)
    if not tracked_d1 and untracked == D1_PATHS:
        return "pre-commit"
    if tracked_d1 == D1_PATHS and not untracked_d1:
        return "post-commit"
    fail("invalid D1 phase inventory")


def validate_identity(path, available):
    exact_path(path)
    if path not in available:
        fail("identity absent from Git inventory")


def validate_manifest(data, tracked, untracked):
    if set(data) != {"schema_version", "documents"} or data["schema_version"] != 1:
        fail("invalid manifest schema")
    phase = determine_phase(tracked, untracked)
    available = tracked | untracked
    documents = data["documents"]
    if not isinstance(documents, list) or not documents:
        fail("invalid manifest documents")
    paths = [entry.get("path") for entry in documents if isinstance(entry, dict)]
    if len(paths) != len(documents) or paths != sorted(paths) or len(set(paths)) != len(paths):
        fail("manifest path ordering")
    expected = {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
    # Coverage is derived from Git identities, never filesystem discovery.
    expected.update(
        path for path in available
        if path.startswith("docs/") and path.count("/") == 1 and path.endswith(".md")
    )
    if set(paths) != expected:
        fail("manifest coverage")
    for entry in documents:
        if set(entry) != FIELDS:
            fail("manifest field set")
        validate_identity(entry["path"], available)
        if entry["kind"] not in KINDS or entry["status"] not in STATUSES:
            fail("manifest taxonomy")
        if entry["authority"] not in AUTHORITIES or entry["risk_floor"] not in RISK_FLOORS:
            fail("manifest authority")
        if not isinstance(entry["protected"], bool) or not isinstance(entry["policy_input"], bool):
            fail("manifest booleans")
        if not isinstance(entry["domain"], str) or not IDENTIFIER.fullmatch(entry["domain"]):
            fail("manifest domain")
        for key in ("topics", "protecting_tests"):
            values = entry[key]
            if not isinstance(values, list) or not values or values != sorted(values) or len(values) != len(set(values)):
                fail("manifest list ordering")
        if any(not isinstance(topic, str) or not IDENTIFIER.fullmatch(topic) for topic in entry["topics"]):
            fail("manifest topics")
        authoritative = entry["status"] == "current" and entry["authority"] in {"primary", "subordinate"}
        if entry["policy_input"] != authoritative:
            fail("policy-input consistency")
        if entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none":
            fail("non-authority record")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            fail("fixture status")
        for test_path in entry["protecting_tests"]:
            validate_protecting_test(test_path, tracked, untracked, phase)
    return phase


def validate_protecting_test(path, tracked, untracked, phase):
    exact_path(path)
    if not path.startswith("tests/") or not path.rsplit("/", 1)[-1].startswith("test_") or not path.endswith(".py"):
        fail("invalid protecting test")
    if path == "tests/test_AUTONOMOUS_document_manifest.py":
        fail("aliased protecting test")
    if path == "tests/test_autonomous_document_manifest.py":
        if phase == "pre-commit" and path in untracked:
            return
        if phase == "post-commit" and path in tracked:
            return
        fail("D1 protecting-test phase")
    if path not in tracked or path in untracked:
        fail("protecting test not tracked")


def paragraphs(text):
    return [re.sub(r"\s+", " ", item.lower()) for item in re.split(r"\n\s*\n", text) if item.strip()]


def contains_any(text, expressions):
    return any(re.search(expression, text) for expression in expressions)


def validate_governance_prose(text):
    blocks = paragraphs(text)
    pre = [block for block in blocks if contains_any(block, (r"before(?: the worker)? commit", r"pre[- ]commit"))]
    post = [block for block in blocks if contains_any(block, (r"after commit", r"once committed", r"post[- ]commit"))]
    if not any("git" in block and "candidate" in block and "exact" in block and
               contains_any(block, (r"only", r"exclusive", r"complete")) for block in pre):
        fail("missing pre-commit candidate meaning")
    if not any("tracked" in block and "candidate" in block and
               contains_any(block, (r"no .*allowance", r"allowance .*active", r"inactive")) for block in post):
        fail("missing post-commit tracked meaning")
    joined = " ".join(blocks)
    required = (
        ("authority", "domain"), ("scientific", "contract"),
        ("reference", "runbook", "history", "roadmap", "fixture"),
        ("case-sensitive", "git"), ("conflict", "escalate"),
    )
    if any(not all(word in joined for word in words) for words in required):
        fail("missing governance role meaning")


class AutonomousDocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = json.loads((ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json").read_text(encoding="utf-8"))
        cls.governance = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8")

    def test_live_phase_and_manifest(self):
        tracked, untracked = live_inventories()
        self.assertIn(validate_manifest(self.data, tracked, untracked), {"pre-commit", "post-commit"})

    def test_anchor_classifications_and_governance_meaning(self):
        by_path = {entry["path"]: entry for entry in self.data["documents"]}
        self.assertEqual((by_path["docs/AUTONOMOUS_DEVELOPMENT.md"]["kind"], by_path["docs/AUTONOMOUS_DEVELOPMENT.md"]["authority"]), ("policy", "primary"))
        self.assertEqual(by_path["docs/AUTONOMOUS_ORCHESTRATION.md"]["authority"], "subordinate")
        self.assertEqual(by_path[".github/ISSUE_TEMPLATE/autonomous-work.md"]["authority"], "subordinate")
        self.assertEqual(by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["risk_floor"], "red")
        self.assertEqual(by_path["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["authority"], "reference")
        self.assertEqual(by_path["docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md"]["topics"], ["canary", "terra"])
        validate_governance_prose(self.governance)

    def test_phase_states_and_invalid_states(self):
        base = frozenset(("AGENTS.md", "README.md"))
        self.assertEqual(determine_phase(base, D1_PATHS), "pre-commit")
        self.assertEqual(determine_phase(base | D1_PATHS, frozenset()), "post-commit")
        for tracked, untracked in ((base, frozenset()), (base | {next(iter(D1_PATHS))}, D1_PATHS - {next(iter(D1_PATHS))}), (base, D1_PATHS | {"extra"})):
            with self.assertRaises(ManifestError):
                determine_phase(tracked, untracked)
        with self.assertRaises(ManifestError):
            determine_phase(base, D1_PATHS, {"docs/AUTONOMOUS_GOVERNANCE_INDEX.md"})

    def test_git_z_is_fail_closed_and_alias_safe(self):
        self.assertEqual(parse_git_z(b"a\0b\0", "x", False), ("a", "b"))
        bad = (b"a\0b", b"a\0\0b\0", b"a\0a\0", b"../a\0", b"a\\b\0", b"\xff\0")
        for raw in bad:
            with self.assertRaises(ManifestError):
                parse_git_z(raw, "x", False)
        for pair in (("A", "a"), ("caf\u00e9", "cafe\u0301"), ("CAF\u00c9", "cafe\u0301")):
            with self.assertRaises(ManifestError):
                parse_git_z((pair[0] + "\0" + pair[1] + "\0").encode(), "x", False)
        with self.assertRaises(ManifestError):
            validate_inventory_pair(("CAF\u00c9",), ("cafe\u0301",))

    def test_git_acquisition_failure_is_bounded(self):
        with mock.patch("subprocess.run", side_effect=OSError):
            with self.assertRaises(ManifestError):
                git_inventory(("git", "ls-files", "-z"), "tracked", False)
        failed = mock.Mock(returncode=1, stdout=b"", stderr=b"untrusted detail")
        with mock.patch("subprocess.run", return_value=failed):
            with self.assertRaises(ManifestError):
                git_inventory(("git", "ls-files", "-z"), "tracked", False)

    def test_semantic_equivalents_and_inversions(self):
        variants = (
            "Before commit Git permits only exact authorized D1 candidates.\n\nAfter commit each exact path is tracked and no candidate allowance is active.",
            "Before the worker commits Git permits only exact authorized candidates.\n\nOnce committed each exact identity is tracked; candidate allowance is inactive.",
            "Pre-commit Git permits only exact authorized candidates.\n\nPost-commit exact identities are tracked and no candidate allowance remains active.",
        )
        suffix = "\n\nAuthority is domain scoped. The scientific contract is separate. Reference runbook history roadmap fixture roles are non-authority. Git identity is case-sensitive. On conflict, escalate."
        for text in variants:
            validate_governance_prose(text + suffix)
        for text in (
            "Before commit exact paths are tracked and no candidate allowance is active.",
            "After commit Git permits only exact authorized candidates.",
        ):
            with self.assertRaises(ManifestError):
                validate_governance_prose(text + suffix)


if __name__ == "__main__":
    unittest.main()
