import json
import re
import subprocess
import unicodedata
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
D1 = frozenset((
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
))
GOVERNANCE_INDEX = "docs/AUTONOMOUS_GOVERNANCE_INDEX.md"
MANIFEST_TEST = "tests/test_autonomous_document_manifest.py"
REQUIRED_DOCUMENTS = frozenset((
    "AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md",
))
FIELDS = frozenset(("path", "kind", "domain", "status", "authority",
                    "policy_input", "protected", "risk_floor", "topics",
                    "protecting_tests"))
IDENTIFIER = re.compile(r"^[a-z][a-z0-9-]{0,63}$")
METADATA = {
    "schema_version": 1,
    "authority_scope": "domain-scoped",
    "authority_precedence": "primary-then-subordinate",
    "scientific_contract": "separate",
    "non_authority_roles": ["fixture-evidence", "history", "reference", "roadmap", "runbook"],
    "pre_commit_identity": "exact-authorized-d1-candidates",
    "post_commit_identity": "exact-tracked-d1-identities",
    "post_commit_candidate_allowance": "inactive",
    "conflict_handling": "fail-closed-escalate",
}


def key(path):
    return unicodedata.normalize("NFC", path).casefold()


def valid_path(path):
    return (isinstance(path, str) and path and "\\" not in path and
            not path.startswith("/") and not path.startswith("../") and
            "/../" not in path and not path.endswith("/") and
            all(part not in ("", ".", "..") for part in path.split("/")))


def parse_z(raw, tracked=False):
    """Parse Git's NUL protocol before any D1-specific reduction."""
    if not isinstance(raw, bytes) or (raw and not raw.endswith(b"\0")):
        raise ValueError("invalid Git NUL evidence")
    if tracked and not raw:
        raise ValueError("empty tracked inventory")
    records = raw[:-1].split(b"\0") if raw else []
    if any(not record for record in records):
        raise ValueError("empty Git identity")
    try:
        paths = [record.decode("utf-8", "strict") for record in records]
    except UnicodeDecodeError as exc:
        raise ValueError("undecodable Git identity") from exc
    if len(paths) != len(set(paths)) or any(not valid_path(path) for path in paths):
        raise ValueError("invalid Git identity")
    return frozenset(paths)


def git_inventory(args, tracked=False):
    completed = subprocess.run(["git"] + args, cwd=str(ROOT), stdout=subprocess.PIPE,
                               stderr=subprocess.DEVNULL, check=False)
    if completed.returncode:
        raise ValueError("Git inventory acquisition failed")
    return parse_z(completed.stdout, tracked=tracked)


def d1_phase(tracked, untracked, ignored):
    """Classify only exact D1 paths and aliases; unrelated aliases are inert."""
    all_paths = tracked | untracked | ignored
    aliases = {path for path in all_paths if key(path) in {key(item) for item in D1}}
    exact_tracked, exact_untracked, exact_ignored = tracked & D1, untracked & D1, ignored & D1
    if any(path not in D1 for path in aliases):
        raise ValueError("D1 alias evidence")
    if exact_ignored or len(aliases) != len(set(aliases)):
        raise ValueError("ambiguous D1 evidence")
    if not exact_tracked and exact_untracked == D1:
        return "pre-commit"
    if exact_tracked == D1 and not exact_untracked:
        return "post-commit"
    raise ValueError("invalid D1 phase")


def top_level_doc(path):
    return (path.startswith("docs/") and path.count("/") == 1 and
            path.endswith(".md"))


def expected_manifest_paths(tracked, phase):
    expected = {path for path in tracked if top_level_doc(path)} | set(REQUIRED_DOCUMENTS)
    if phase == "pre-commit":
        expected.add(GOVERNANCE_INDEX)
    elif GOVERNANCE_INDEX not in tracked:
        raise ValueError("post-commit governance index is not tracked")
    if not expected <= (tracked | ({GOVERNANCE_INDEX} if phase == "pre-commit" else set())):
        raise ValueError("required document is absent")
    return expected


def metadata_from(text):
    start, end = "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->"
    if text.count(start) != 1 or text.count(end) != 1:
        raise ValueError("metadata delimiters")
    first, last = text.find(start), text.find(end)
    if first < 0 or last < 0 or first >= last:
        raise ValueError("metadata delimiter order")
    try:
        return json.loads(text[first + len(start):last].strip())
    except json.JSONDecodeError as exc:
        raise ValueError("metadata JSON") from exc


def validate_manifest(manifest, tracked, untracked, ignored, governance_text):
    if set(manifest) != {"schema_version", "documents"} or manifest["schema_version"] != 1:
        raise ValueError("manifest schema")
    phase = d1_phase(tracked, untracked, ignored)
    valid_identities = tracked | (D1 if phase == "pre-commit" else frozenset())
    documents = manifest["documents"]
    if not isinstance(documents, list):
        raise ValueError("documents list")
    paths = [entry.get("path") for entry in documents if isinstance(entry, dict)]
    if len(paths) != len(documents) or paths != sorted(paths) or len(paths) != len(set(paths)):
        raise ValueError("manifest paths")
    if set(paths) != expected_manifest_paths(tracked, phase):
        raise ValueError("manifest coverage")
    for entry in documents:
        if set(entry) != FIELDS or entry["path"] not in valid_identities:
            raise ValueError("manifest identity")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool):
            raise ValueError("boolean field")
        if entry["kind"] not in {"policy", "orchestration-contract", "scientific-contract", "operational-runbook", "subsystem-contract", "developer-guide", "architecture-reference", "roadmap", "historical-record", "fixture-evidence"}:
            raise ValueError("kind")
        if entry["status"] not in {"current", "historical", "retained-evidence", "deprecated"} or entry["authority"] not in {"primary", "subordinate", "reference", "none"} or entry["risk_floor"] not in {"green", "yellow", "red"}:
            raise ValueError("enum")
        if not IDENTIFIER.fullmatch(entry["domain"]) or not all(IDENTIFIER.fullmatch(x) for x in entry["topics"]):
            raise ValueError("identifier")
        for name in ("topics", "protecting_tests"):
            values = entry[name]
            if not isinstance(values, list) or not values or values != sorted(values) or len(values) != len(set(values)):
                raise ValueError("ordered evidence")
        for target in entry["protecting_tests"]:
            permitted_candidate = phase == "pre-commit" and target == MANIFEST_TEST
            if (target not in tracked and not permitted_candidate) or not valid_path(target) or not target.startswith("tests/") or not target.split("/")[-1].startswith("test_") or not target.endswith(".py"):
                raise ValueError("protecting test")
        non_policy = entry["authority"] in {"reference", "none"} or entry["status"] in {"historical", "deprecated", "retained-evidence"}
        if entry["policy_input"] == non_policy or (entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none") or (entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence"):
            raise ValueError("authority consistency")
    if metadata_from(governance_text) != METADATA:
        raise ValueError("metadata schema")
    return phase


class AutonomousDocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.manifest = json.loads((ROOT / "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json").read_text(encoding="utf-8"))
        cls.governance = (ROOT / GOVERNANCE_INDEX).read_text(encoding="utf-8")
        cls.tracked = git_inventory(["ls-files", "-z"], tracked=True)
        cls.untracked = git_inventory(["ls-files", "--others", "--exclude-standard", "-z"])
        cls.ignored = git_inventory(["ls-files", "--others", "--ignored", "--exclude-standard", "-z"])

    def test_current_repository_manifest_is_valid(self):
        self.assertIn(validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, self.governance), {"pre-commit", "post-commit"})

    def test_expected_coverage_comes_from_git_inventory_only(self):
        tracked = frozenset(("docs/A.md", "docs/nested/B.md", "README.md", "AGENTS.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"))
        self.assertEqual(expected_manifest_paths(tracked, "pre-commit"), {"docs/A.md", GOVERNANCE_INDEX, *REQUIRED_DOCUMENTS})
        self.assertEqual(expected_manifest_paths(tracked | {GOVERNANCE_INDEX}, "post-commit"), {"docs/A.md", GOVERNANCE_INDEX, *REQUIRED_DOCUMENTS})
        self.assertNotIn("docs/a.md", expected_manifest_paths(tracked, "pre-commit"))

    def test_git_nul_parser_fails_closed(self):
        for raw in (b"docs/A.md", b"docs/A.md\0\0", b"docs/../A.md\0", b"docs\\A.md\0", b"x\0x\0", b"\xff\0"):
            with self.assertRaises(ValueError): parse_z(raw, tracked=True)
        with self.assertRaises(ValueError): parse_z(b"", tracked=True)

    def test_phase_rows_and_aliases(self):
        self.assertEqual(d1_phase(frozenset(), D1, frozenset()), "pre-commit")
        self.assertEqual(d1_phase(D1, frozenset(), frozenset()), "post-commit")
        for tracked, untracked, ignored in ((frozenset(), D1 - {GOVERNANCE_INDEX}, frozenset()), ({GOVERNANCE_INDEX}, D1 - {GOVERNANCE_INDEX}, frozenset()), (D1, {GOVERNANCE_INDEX}, frozenset()), (frozenset(), D1, {GOVERNANCE_INDEX})):
            with self.assertRaises(ValueError): d1_phase(frozenset(tracked), frozenset(untracked), frozenset(ignored))
        for alias in ("docs/autonomous_governance_index.md", "docs/AUTONOMOUS_GOVERNANCE_INDEX.MD", "docs/AUTONOMOU\u017f_GOVERNANCE_INDEX.md"):
            with self.assertRaises(ValueError): d1_phase(frozenset(), D1 | {alias}, frozenset())
        self.assertEqual(d1_phase(frozenset(), D1 | {"other/ALIAS"}, frozenset()), "pre-commit")

    def test_identifier_bounds_and_fixture_invariant(self):
        for value in ("a", "a" * 64): self.assertIsNotNone(IDENTIFIER.fullmatch(value))
        for value in ("a" * 65, "A", "a_b", "a b", "1a", "", "é"):
            self.assertIsNone(IDENTIFIER.fullmatch(value))
        fixture = next(x for x in self.manifest["documents"] if x["kind"] == "fixture-evidence")
        bad = dict(fixture); bad["status"] = "historical"
        manifest = dict(self.manifest); manifest["documents"] = [bad if x["path"] == fixture["path"] else x for x in self.manifest["documents"]]
        with self.assertRaises(ValueError): validate_manifest(manifest, self.tracked, self.untracked, self.ignored, self.governance)

    def test_anchor_matrix(self):
        entries = {x["path"]: x for x in self.manifest["documents"]}
        expected = {
            "docs/AUTONOMOUS_DEVELOPMENT.md": ("policy", "autonomy", "current", "primary", True),
            "docs/AUTONOMOUS_ORCHESTRATION.md": ("orchestration-contract", "autonomy", "current", "subordinate", True),
            ".github/ISSUE_TEMPLATE/autonomous-work.md": ("orchestration-contract", "autonomy", "current", "subordinate", True),
            "docs/AUTONOMOUS_WORKER_RUNBOOK.md": ("operational-runbook", "autonomy", "current", "reference", False),
            "docs/AUTONOMOUS_TROUBLESHOOTING.md": ("operational-runbook", "autonomy", "current", "reference", False),
            "docs/CODEX_PROMPT_GUIDE.md": ("developer-guide", "autonomy", "current", "reference", False),
            "docs/ABAQUS_MODEL_CONTRACT.md": ("scientific-contract", "scientific-model", "current", "primary", True),
            GOVERNANCE_INDEX: ("architecture-reference", "autonomy", "current", "reference", False),
        }
        for name in ("A4_18_COMPLETION_OBSERVER", "A5_1_REVIEWER", "A5_2_REVIEW_STATE", "A5_4A_REVIEW_LOOP", "A6_1_ABAQUS_RUNNER_PREFLIGHT", "A6_2_EXACT_PR_VALIDATION", "A7_1_ISOLATED_TARGET_VALIDATION", "A7_2_IMPORT_PARTITION_REGRESSION"):
            expected["docs/" + name + ".md"] = ("subsystem-contract", "autonomy", "current", "subordinate", True)
        for path in entries:
            if path.startswith("docs/A5_3_REPAIR"):
                expected[path] = ("historical-record", "autonomy", "historical", "none", False)
            if path in {"docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md", "docs/TERRA_MEDIUM_CANARY_V2.md", "docs/YELLOW_LANE_REPAIR_FIXTURE_V2.md"}:
                expected[path] = ("fixture-evidence", "autonomy", "retained-evidence", "none", False)
        for path, values in expected.items():
            entry = entries[path]
            self.assertEqual(tuple(entry[x] for x in ("kind", "domain", "status", "authority", "policy_input")), values)

    def test_metadata_delimiters_schema_and_headings(self):
        for heading in ("# Autonomous governance index", "## Purpose and scope", "## Taxonomy and authority", "## Exact Git identity and D1 phases", "## Conflict handling"):
            self.assertIn(heading, self.governance)
        self.assertEqual(metadata_from(self.governance), METADATA)
        for text in (self.governance.replace("<!-- D1-GOVERNANCE-METADATA", "", 1), self.governance + "\nD1-GOVERNANCE-METADATA -->", "D1-GOVERNANCE-METADATA -->\n<!-- D1-GOVERNANCE-METADATA", self.governance.replace('"schema_version":1', 'not-json')):
            with self.assertRaises(ValueError): metadata_from(text)
        changed = self.governance.replace('"domain-scoped"', '"wrong"')
        with self.assertRaises(ValueError): validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, changed)

    def test_coverage_and_structured_metadata_are_exact(self):
        with self.assertRaises(ValueError): validate_manifest(self.manifest, self.tracked | {"docs/extra.md"}, self.untracked, self.ignored, self.governance)
        for field in ("schema_version", "conflict_handling"):
            metadata = dict(METADATA); del metadata[field]
            text = "<!-- D1-GOVERNANCE-METADATA\n" + json.dumps(metadata) + "\nD1-GOVERNANCE-METADATA -->"
            with self.assertRaises(ValueError): validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, text)


if __name__ == "__main__":
    unittest.main()
