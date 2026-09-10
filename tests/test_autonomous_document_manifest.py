"""Deterministic, descriptive D1 document-manifest validation."""

import json
import subprocess
import unicodedata
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
D1_PATHS = (
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
)
ENTRY_FIELDS = {
    "path", "kind", "domain", "status", "authority", "policy_input",
    "protected", "risk_floor", "topics", "protecting_tests",
}
KINDS = {
    "policy", "orchestration-contract", "scientific-contract",
    "operational-runbook", "subsystem-contract", "developer-guide",
    "architecture-reference", "roadmap", "historical-record", "fixture-evidence",
}


def alias_key(path):
    """Comparison-only key: aliases never become accepted Git identities."""
    return unicodedata.normalize("NFC", path).casefold()


def safe_identity(path):
    if not isinstance(path, str) or not path or "\\" in path or path.startswith("/"):
        return False
    if unicodedata.normalize("NFC", path) != path or any(ord(char) < 32 for char in path):
        return False
    return all(part not in ("", ".", "..") for part in path.split("/"))


def parse_git_z(output, require_nonempty=False):
    """Reject malformed Git protocol and unsafe paths before D1 reduction."""
    if not isinstance(output, bytes) or (output and not output.endswith(b"\0")):
        raise ValueError("invalid Git NUL-delimited evidence")
    raw = output[:-1].split(b"\0") if output else []
    if require_nonempty and not raw:
        raise ValueError("empty Git inventory")
    if any(not record for record in raw):
        raise ValueError("empty Git record")
    try:
        paths = tuple(record.decode("utf-8", "strict") for record in raw)
    except UnicodeDecodeError as error:
        raise ValueError("undecodable Git identity") from error
    if len(paths) != len(set(paths)):
        raise ValueError("duplicate Git identity")
    if any(not safe_identity(path) for path in paths):
        raise ValueError("unsafe Git identity")
    return paths


def git_identities(args, require_nonempty=False):
    result = subprocess.run(
        ["git"] + list(args) + ["-z"], cwd=str(ROOT), stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL, check=False,
    )
    if result.returncode:
        raise ValueError("Git identity acquisition failed")
    return parse_git_z(result.stdout, require_nonempty=require_nonempty)


def d1_relevant(inventory):
    """Keep only exact D1 paths or comparison-key aliases of those paths."""
    target_keys = {alias_key(path) for path in D1_PATHS}
    return tuple(path for path in inventory if alias_key(path) in target_keys)


def d1_phase(tracked, candidates, ignored):
    """Classify only bounded D1 evidence after global Git parsing has succeeded."""
    tracked, candidates, ignored = (
        d1_relevant(inventory) for inventory in (tracked, candidates, ignored)
    )
    relevant = tracked + candidates + ignored
    if (any(not safe_identity(path) for path in relevant)
            or len(relevant) != len(set(relevant))):
        raise ValueError("ambiguous D1 identity evidence")
    for target in D1_PATHS:
        aliases = [path for path in relevant if alias_key(path) == alias_key(target)]
        if any(path != target for path in aliases):
            raise ValueError("D1 alias identity evidence")

    targets = set(D1_PATHS)
    tracked_d1, candidate_d1, ignored_d1 = (
        set(inventory) for inventory in (tracked, candidates, ignored)
    )
    if ignored_d1:
        raise ValueError("ignored D1 identity")
    if not tracked_d1 and candidate_d1 == targets:
        return "PRE-COMMIT"
    if tracked_d1 == targets and not candidate_d1:
        return "POST-COMMIT"
    raise ValueError("invalid D1 phase")


def is_test_identity(path):
    return (safe_identity(path) and path.startswith("tests/") and path.endswith(".py")
            and path.rsplit("/", 1)[-1].startswith("test_"))


def validate_protecting_target(path, tracked, candidates, phase):
    if not is_test_identity(path):
        raise ValueError("invalid protecting-test identity")
    if path == D1_PATHS[2] and phase == "PRE-COMMIT" and path in candidates:
        return
    if path not in tracked:
        raise ValueError("protecting test is not an exact tracked identity")


class DocumentManifestTests(unittest.TestCase):
    def live_evidence(self):
        return (
            git_identities(["ls-files"], require_nonempty=True),
            git_identities(["ls-files", "--others", "--exclude-standard"]),
            git_identities(["ls-files", "--others", "--ignored", "--exclude-standard"]),
        )

    def test_git_z_parser_fails_closed_before_d1_reduction(self):
        self.assertEqual(parse_git_z(b"a\0b\0"), ("a", "b"))
        for value in (
                b"a", b"a\0\0", b"a\0a\0", b"a\\b\0", b"../a\0",
                b"docs/cafe\xcc\x81.md\0", b"\xff\0"):
            with self.assertRaises(ValueError):
                parse_git_z(value)
        with self.assertRaises(ValueError):
            parse_git_z(b"", require_nonempty=True)

    def test_git_acquisition_failure_is_bounded_and_fail_closed(self):
        with self.assertRaises(ValueError):
            git_identities(["not-a-git-subcommand"])

    def test_phase_truth_table(self):
        self.assertEqual(d1_phase((), D1_PATHS, ()), "PRE-COMMIT")
        self.assertEqual(d1_phase(D1_PATHS, (), ()), "POST-COMMIT")
        for missing in D1_PATHS:
            with self.assertRaises(ValueError):
                d1_phase((), tuple(path for path in D1_PATHS if path != missing), ())
            with self.assertRaises(ValueError):
                d1_phase(tuple(path for path in D1_PATHS if path != missing), (), ())
        invalid = (
            ((D1_PATHS[0],), (D1_PATHS[1], D1_PATHS[2]), ()),
            ((), D1_PATHS, (D1_PATHS[0],)),
            (D1_PATHS, (D1_PATHS[0],), ()),
            (D1_PATHS + (D1_PATHS[0],), (), ()),
            (D1_PATHS, (), ("docs/AUTONOMOUS_GOVERNANCE_INDEX.MD",)),
            (D1_PATHS, (), ("te\u017fts/test_autonomous_document_manifest.py",)),
            (D1_PATHS, (), ("TE\u017fTS/test_autonomous_document_manifest.py",)),
        )
        for row in invalid:
            with self.assertRaises(ValueError):
                d1_phase(*row)

    def test_unrelated_valid_evidence_does_not_change_d1_phase(self):
        unrelated = ("notes/a.txt", "notes/A.TXT")
        self.assertEqual(d1_phase((), D1_PATHS + unrelated, unrelated), "PRE-COMMIT")
        self.assertEqual(d1_phase(D1_PATHS + unrelated, unrelated, unrelated), "POST-COMMIT")

    def test_live_phase_and_exact_identity_rules(self):
        tracked, candidates, ignored = self.live_evidence()
        phase = d1_phase(tracked, candidates, ignored)
        self.assertIn(phase, ("PRE-COMMIT", "POST-COMMIT"))
        exact = candidates if phase == "PRE-COMMIT" else tracked
        self.assertEqual(set(D1_PATHS).intersection(exact), set(D1_PATHS))

    def test_manifest_schema_taxonomy_and_coverage(self):
        manifest = json.loads((ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json").read_text(encoding="utf-8"))
        self.assertEqual(set(manifest), {"schema_version", "documents"})
        self.assertEqual(manifest["schema_version"], 1)
        entries = manifest["documents"]
        paths = [entry["path"] for entry in entries]
        self.assertEqual(paths, sorted(paths))
        self.assertEqual(len(paths), len(set(paths)))
        expected = {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"}
        expected.update("docs/" + item.name for item in (ROOT / "docs").glob("*.md"))
        self.assertEqual(set(paths), expected)
        for entry in entries:
            self.assertEqual(set(entry), ENTRY_FIELDS)
            self.assertIn(entry["kind"], KINDS)
            self.assertIn(entry["status"], {"current", "historical", "retained-evidence", "deprecated"})
            self.assertIn(entry["authority"], {"primary", "subordinate", "reference", "none"})
            self.assertIn(entry["risk_floor"], {"green", "yellow", "red"})
            self.assertTrue(entry["domain"].islower() and entry["domain"].replace("-", "").isidentifier())
            self.assertIsInstance(entry["policy_input"], bool)
            self.assertIsInstance(entry["protected"], bool)
            self.assertEqual(entry["topics"], sorted(set(entry["topics"])))
            self.assertEqual(entry["protecting_tests"], sorted(set(entry["protecting_tests"])))
            self.assertTrue(all(topic.islower() and topic.replace("-", "").isidentifier() for topic in entry["topics"]))
            self.assertEqual(entry["policy_input"], entry["status"] == "current" and entry["authority"] in {"primary", "subordinate"})
            if entry["kind"] in {"historical-record", "fixture-evidence"}:
                self.assertEqual(entry["authority"], "none")
        by_path = {entry["path"]: entry for entry in entries}
        self.assertEqual(by_path["docs/AUTONOMOUS_DEVELOPMENT.md"]["authority"], "primary")
        self.assertEqual(by_path["docs/ABAQUS_MODEL_CONTRACT.md"]["authority"], "primary")
        self.assertEqual(by_path["docs/AUTONOMOUS_GOVERNANCE_INDEX.md"]["authority"], "reference")

    def test_manifest_paths_and_protecting_tests_have_exact_phase_identity(self):
        tracked, candidates, ignored = self.live_evidence()
        phase = d1_phase(tracked, candidates, ignored)
        available = set(tracked) | set(candidates)
        manifest = json.loads((ROOT / "docs" / "AUTONOMOUS_DOCUMENT_MANIFEST.json").read_text(encoding="utf-8"))
        for entry in manifest["documents"]:
            self.assertIn(entry["path"], available)
            for target in entry["protecting_tests"]:
                validate_protecting_target(target, tracked, candidates, phase)
        for bad in ("tests/test_AUTONOMOUS_document_manifest.py", "tests/nope.py", "../tests/test_x.py", "docs/test_x.py", "tests/helper.py"):
            with self.assertRaises(ValueError):
                validate_protecting_target(bad, D1_PATHS, (), "POST-COMMIT")

    def test_structured_governance_metadata_and_headings(self):
        text = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8")
        start, end = "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->"
        self.assertEqual(text.count(start), 1)
        self.assertEqual(text.count(end), 1)
        metadata = json.loads(text.split(start, 1)[1].split(end, 1)[0].strip())
        self.assertEqual(metadata, {"schema_version": 1, "authority_scope": "domain-scoped", "authority_precedence": "primary-then-subordinate", "scientific_contract": "separate", "non_authority_roles": ["fixture-evidence", "history", "reference", "roadmap", "runbook"], "pre_commit_identity": "exact-authorized-d1-candidates", "post_commit_identity": "exact-tracked-d1-identities", "post_commit_candidate_allowance": "inactive", "conflict_handling": "fail-closed-escalate"})
        for heading in ("# Autonomous governance index", "## Purpose and scope", "## Taxonomy and authority", "## Exact Git identity and D1 phases", "## Conflict handling"):
            self.assertIn(heading, text)

    def test_governance_structure_ignores_ordinary_prose_rewrapping(self):
        text = (ROOT / "docs" / "AUTONOMOUS_GOVERNANCE_INDEX.md").read_text(encoding="utf-8")
        rewritten = text.replace("This descriptive index records", "This index records\n")
        start, end = "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->"
        self.assertEqual(text.split(start, 1)[1].split(end, 1)[0], rewritten.split(start, 1)[1].split(end, 1)[0])


if __name__ == "__main__":
    unittest.main()
