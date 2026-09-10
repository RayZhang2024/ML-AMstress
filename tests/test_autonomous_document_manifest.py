"""Deterministic, Git-backed validation for the descriptive D1 document map."""
import copy
import json
import re
import subprocess
import unicodedata
import unittest
from unittest import mock


D1_PATHS = frozenset((
    "docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
    "docs/AUTONOMOUS_GOVERNANCE_INDEX.md",
    "tests/test_autonomous_document_manifest.py",
))
SCHEMA_FIELDS = frozenset(("path", "kind", "domain", "status", "authority",
                           "policy_input", "protected", "risk_floor", "topics",
                           "protecting_tests"))
KINDS = frozenset(("policy", "orchestration-contract", "scientific-contract",
                   "operational-runbook", "subsystem-contract", "developer-guide",
                   "architecture-reference", "roadmap", "historical-record",
                   "fixture-evidence"))
STATUSES = frozenset(("current", "historical", "retained-evidence", "deprecated"))
AUTHORITIES = frozenset(("primary", "subordinate", "reference", "none"))
RISK_FLOORS = frozenset(("green", "yellow", "red"))
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


class ValidationError(ValueError):
    """A deliberately bounded validation failure; never includes Git output."""


def comparison_key(path):
    return unicodedata.normalize("NFC", path).casefold()


def valid_path(path):
    """Accept only already-canonical safe repository-relative POSIX identities."""
    if (not isinstance(path, str) or not path or path.startswith("/") or "\\" in path
            or re.match(r"^[A-Za-z]:/", path)):
        return False
    if unicodedata.normalize("NFC", path) != path or any(ord(char) < 32 for char in path):
        return False
    return all(component not in ("", ".", "..") for component in path.split("/"))


def parse_z(raw, inventory):
    """Parse one successful Git -z inventory before any D1 classification."""
    if not isinstance(raw, bytes) or not raw or not raw.endswith(b"\0"):
        raise ValidationError("invalid Git inventory protocol")
    records = raw[:-1].split(b"\0")
    if any(not record for record in records):
        raise ValidationError("invalid Git inventory record")
    try:
        paths = [record.decode("utf-8", "strict") for record in records]
    except UnicodeDecodeError:
        raise ValidationError("undecodable Git identity")
    if len(paths) != len(set(paths)) or any(not valid_path(path) for path in paths):
        raise ValidationError("unsafe or noncanonical Git identity")
    return tuple(paths)


def acquire_git_inventory(args, inventory):
    result = subprocess.run(["git"] + list(args), stdout=subprocess.PIPE,
                            stderr=subprocess.DEVNULL, check=False)
    if result.returncode:
        raise ValidationError("Git inventory acquisition failed")
    return parse_z(result.stdout, inventory)


def validate_inventories(tracked, untracked, ignored):
    """Global parsing is complete before this function performs D1 reduction."""
    inventories = (tuple(tracked), tuple(untracked), tuple(ignored))
    if not inventories[0]:
        raise ValidationError("empty tracked Git inventory")
    for values in inventories:
        if len(values) != len(set(values)) or any(not valid_path(p) for p in values):
            raise ValidationError("invalid validated Git inventory")
    return inventories


def d1_relevant(path):
    return path in D1_PATHS or comparison_key(path) in {comparison_key(p) for p in D1_PATHS}


def classify_phase(tracked, untracked, ignored):
    tracked, untracked, ignored = validate_inventories(tracked, untracked, ignored)
    relevant = tuple(tuple(path for path in group if d1_relevant(path))
                     for group in (tracked, untracked, ignored))
    if relevant[2]:
        raise ValidationError("D1 ignored evidence")
    for group in relevant:
        for path in group:
            if path not in D1_PATHS:
                raise ValidationError("D1 alias evidence")
    locations = {path: [index for index, group in enumerate(relevant) if path in group]
                 for path in D1_PATHS}
    if any(len(where) > 1 for where in locations.values()):
        raise ValidationError("D1 cross-inventory evidence")
    exact_tracked = {path for path, where in locations.items() if where == [0]}
    exact_untracked = {path for path, where in locations.items() if where == [1]}
    if not exact_tracked and exact_untracked == D1_PATHS:
        return "PRE-COMMIT"
    if exact_tracked == D1_PATHS and not exact_untracked:
        return "POST-COMMIT"
    raise ValidationError("invalid D1 phase")


def expected_document_paths(tracked, phase):
    tracked = tuple(tracked)
    expected = {path for path in tracked if path.startswith("docs/")
                and path.count("/") == 1 and path.endswith(".md")}
    expected.update(("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"))
    if phase == "PRE-COMMIT":
        expected.add("docs/AUTONOMOUS_GOVERNANCE_INDEX.md")
    return expected


def validate_metadata(markdown):
    start = "<!-- D1-GOVERNANCE-METADATA"
    end = "D1-GOVERNANCE-METADATA -->"
    if markdown.count(start) != 1 or markdown.count(end) != 1:
        raise ValidationError("invalid governance metadata delimiters")
    if markdown.index(start) > markdown.index(end):
        raise ValidationError("invalid governance metadata ordering")
    left, tail = markdown.split(start, 1)
    payload, right = tail.split(end, 1)
    if not left or not right:
        raise ValidationError("invalid governance metadata ordering")
    try:
        value = json.loads(payload.strip())
    except (TypeError, ValueError):
        raise ValidationError("malformed governance metadata")
    if value != METADATA:
        raise ValidationError("invalid governance metadata")
    return value


def validate_manifest(manifest, tracked, untracked, ignored, governance_markdown=None):
    phase = classify_phase(tracked, untracked, ignored)
    if not isinstance(manifest, dict) or set(manifest) != {"schema_version", "documents"}:
        raise ValidationError("invalid manifest envelope")
    if manifest["schema_version"] != 1 or not isinstance(manifest["documents"], list):
        raise ValidationError("invalid manifest schema")
    documents = manifest["documents"]
    paths = []
    allowed = set(tracked)
    if phase == "PRE-COMMIT":
        allowed.update(D1_PATHS)
    for entry in documents:
        if not isinstance(entry, dict) or set(entry) != SCHEMA_FIELDS:
            raise ValidationError("invalid manifest entry schema")
        path = entry["path"]
        paths.append(path)
        if (not valid_path(path) or path not in allowed or
                (path in D1_PATHS and phase == "PRE-COMMIT" and path != "docs/AUTONOMOUS_GOVERNANCE_INDEX.md")):
            raise ValidationError("invalid manifest identity")
        if entry["kind"] not in KINDS or entry["status"] not in STATUSES:
            raise ValidationError("invalid manifest enum")
        if entry["authority"] not in AUTHORITIES or entry["risk_floor"] not in RISK_FLOORS:
            raise ValidationError("invalid manifest authority or risk")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool):
            raise ValidationError("invalid manifest boolean")
        if not IDENTIFIER.fullmatch(entry["domain"]):
            raise ValidationError("invalid manifest domain")
        for field in ("topics", "protecting_tests"):
            values = entry[field]
            if not isinstance(values, list) or not values or values != sorted(values) or len(values) != len(set(values)):
                raise ValidationError("invalid manifest list")
        if any(not IDENTIFIER.fullmatch(topic) for topic in entry["topics"]):
            raise ValidationError("invalid manifest topic")
        for target in entry["protecting_tests"]:
            test_allowed = set(tracked)
            if phase == "PRE-COMMIT":
                test_allowed.add("tests/test_autonomous_document_manifest.py")
            if (not valid_path(target) or target not in test_allowed or not target.startswith("tests/")
                    or not target.rsplit("/", 1)[-1].startswith("test_") or not target.endswith(".py")):
                raise ValidationError("invalid protecting test")
        non_policy = entry["authority"] in {"reference", "none"} or entry["status"] in {"historical", "deprecated", "retained-evidence"}
        if entry["policy_input"] != (entry["status"] == "current" and entry["authority"] in {"primary", "subordinate"}):
            raise ValidationError("inconsistent policy input")
        if non_policy and entry["policy_input"]:
            raise ValidationError("non-authority policy input")
        if entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none":
            raise ValidationError("historical or fixture authority")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            raise ValidationError("fixture status")
    if paths != sorted(paths) or len(paths) != len(set(paths)):
        raise ValidationError("unsorted or duplicate manifest paths")
    if set(paths) != expected_document_paths(tracked, phase):
        raise ValidationError("manifest coverage mismatch")
    if governance_markdown is not None:
        validate_metadata(governance_markdown)
        for heading in ("# Autonomous governance index", "## Purpose and scope", "## Taxonomy and authority",
                        "## Exact Git identity and D1 phases", "## Conflict handling"):
            if heading not in governance_markdown:
                raise ValidationError("missing governance heading")
    return phase


class DocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tracked = acquire_git_inventory(("ls-files", "-z"), "tracked")
        cls.untracked = acquire_git_inventory(("ls-files", "--others", "--exclude-standard", "-z"), "untracked")
        cls.ignored = acquire_git_inventory(("ls-files", "--others", "--ignored", "--exclude-standard", "-z"), "ignored")
        with open("docs/AUTONOMOUS_DOCUMENT_MANIFEST.json", encoding="utf-8") as source:
            cls.manifest = json.load(source)
        with open("docs/AUTONOMOUS_GOVERNANCE_INDEX.md", encoding="utf-8") as source:
            cls.index = source.read()

    def assertInvalid(self, callback, *args):
        with self.assertRaises(ValidationError):
            callback(*args)

    def test_current_precommit_contract_and_postcommit_contract(self):
        self.assertEqual(validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, self.index), "PRE-COMMIT")
        post_tracked = tuple(sorted(set(self.tracked) | D1_PATHS))
        post_untracked = tuple(path for path in self.untracked if path not in D1_PATHS)
        self.assertEqual(validate_manifest(self.manifest, post_tracked, post_untracked, self.ignored, self.index), "POST-COMMIT")

    def test_git_parser_rejects_every_unsafe_protocol_case(self):
        valid = b"AGENTS.md\0docs/A.md\0"
        self.assertEqual(parse_z(valid, "test"), ("AGENTS.md", "docs/A.md"))
        bad = (b"", b"AGENTS.md", b"AGENTS.md\0\0docs/A.md\0", b"AGENTS.md\0AGENTS.md\0",
               b"/absolute\0", b"a\\b\0", b"a//b\0", b"./a\0", b"a/../b\0", b"docs/e\xcc\x81.md\0",
               b"docs/a\tb.md\0", b"docs/a\nb.md\0", b"docs/a\x1fb.md\0", b"\xff\0")
        for raw in bad:
            self.assertInvalid(parse_z, raw, "test")
        with mock.patch("subprocess.run", return_value=mock.Mock(returncode=1, stdout=b"")):
            self.assertInvalid(acquire_git_inventory, ("ls-files", "-z"), "tracked")

    def test_phase_matrix_rejects_all_nonexact_d1_evidence(self):
        base = ("AGENTS.md",)
        candidates = tuple(sorted(D1_PATHS))
        self.assertEqual(classify_phase(base, candidates, ("README.md",)), "PRE-COMMIT")
        post = tuple(sorted(set(base) | D1_PATHS))
        self.assertEqual(classify_phase(post, ("README.md",), ("docs/Z.md",)), "POST-COMMIT")
        for missing in D1_PATHS:
            self.assertInvalid(classify_phase, base, tuple(p for p in candidates if p != missing), ("README.md",))
            self.assertInvalid(classify_phase, tuple(p for p in post if p != missing), ("README.md",), ("docs/Z.md",))
        alias = "docs/autonomous_governance_index.md"
        unicode_alias = "docſ/AUTONOMOUS_GOVERNANCE_INDEX.md"
        combined_alias = "DOCſ/AUTONOMOUS_GOVERNANCE_INDEX.MD"
        for bad_groups in ((base, candidates, ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",)),
                           (post, ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",), ("README.md",)),
                           (base, candidates + (alias,), ("README.md",)),
                           (base, candidates + (unicode_alias,), ("README.md",)),
                           (base, candidates + (combined_alias,), ("README.md",))):
            self.assertInvalid(classify_phase, *bad_groups)
        # Unrelated aliases/collisions are inert after globally valid parsing.
        self.assertEqual(classify_phase(base + ("docs/Foo.md",), candidates + ("docs/foo.md",), ("README.md",)), "PRE-COMMIT")

    def test_expected_coverage_uses_only_git_identities(self):
        tracked = ("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md",
                   "docs/A.md", "docs/UPPER.MD", "docs/nested/A.md")
        self.assertEqual(expected_document_paths(tracked, "PRE-COMMIT"),
                         {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md", "docs/A.md", "docs/AUTONOMOUS_GOVERNANCE_INDEX.md"})
        self.assertNotIn("docs/AUTONOMOUS_GOVERNANCE_INDEX.md", expected_document_paths(tracked, "POST-COMMIT"))
        altered = copy.deepcopy(self.manifest)
        altered["documents"].pop()
        self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)

    def test_manifest_mutation_matrix_uses_production_validator(self):
        for field, value in (("kind", "bad"), ("status", "bad"), ("authority", "bad"), ("risk_floor", None),
                             ("domain", "Upper"), ("domain", "a" * 65), ("topics", []),
                             ("topics", ["b", "a"]), ("topics", ["a", "a"]), ("protecting_tests", []),
                             ("protecting_tests", ["tests/test_issue22_governance_docs.py", "tests/test_issue22_governance_docs.py"])):
            altered = copy.deepcopy(self.manifest)
            altered["documents"][0][field] = value
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for value in ("", "1a", "a_b", "a b", "é"):
            altered = copy.deepcopy(self.manifest); altered["documents"][0]["topics"] = [value]
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        altered = copy.deepcopy(self.manifest); altered["documents"][0].pop("kind")
        self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        altered = copy.deepcopy(self.manifest); altered["documents"][0]["extra"] = True
        self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for field in SCHEMA_FIELDS:
            altered = copy.deepcopy(self.manifest); altered["documents"][0].pop(field)
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)

    def test_protecting_test_negative_matrix_uses_production_validator(self):
        bad_targets = ("Tests/test_autonomous_document_manifest.py", "teſts/test_autonomous_document_manifest.py", "tests/AUTONOMOUS_DOCUMENT_MANIFEST.py",
                       "tests/missing.py", "../tests/test_bad.py", "docs/test_bad.py", "tests/check_bad.py", "tests/test_bad.txt")
        for target in bad_targets:
            altered = copy.deepcopy(self.manifest); altered["documents"][0]["protecting_tests"] = [target]
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)

    def test_manifest_coverage_rejects_wrong_case_and_missing_special_paths(self):
        altered = copy.deepcopy(self.manifest)
        altered["documents"][0]["path"] = ".github/issue_template/autonomous-work.md"
        self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for special in ("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"):
            altered = copy.deepcopy(self.manifest)
            altered["documents"] = [entry for entry in altered["documents"] if entry["path"] != special]
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)

    def test_anchor_matrix(self):
        entries = {entry["path"]: entry for entry in self.manifest["documents"]}
        anchors = {
            "docs/AUTONOMOUS_DEVELOPMENT.md": ("policy", "autonomy", "current", "primary", True),
            "docs/AUTONOMOUS_ORCHESTRATION.md": ("orchestration-contract", "autonomy", "current", "subordinate", True),
            ".github/ISSUE_TEMPLATE/autonomous-work.md": ("orchestration-contract", "autonomy", "current", "subordinate", True),
            "docs/AUTONOMOUS_WORKER_RUNBOOK.md": ("operational-runbook", "autonomy", "current", "reference", False),
            "docs/AUTONOMOUS_TROUBLESHOOTING.md": ("operational-runbook", "autonomy", "current", "reference", False),
            "docs/CODEX_PROMPT_GUIDE.md": ("developer-guide", "autonomy", "current", "reference", False),
            "docs/ABAQUS_MODEL_CONTRACT.md": ("scientific-contract", "scientific-model", "current", "primary", True),
            "docs/AUTONOMOUS_GOVERNANCE_INDEX.md": ("architecture-reference", "autonomy", "current", "reference", False),
        }
        for path in ("docs/A4_18_COMPLETION_OBSERVER.md", "docs/A5_1_REVIEWER.md", "docs/A5_2_REVIEW_STATE.md", "docs/A5_4A_REVIEW_LOOP.md", "docs/A6_1_ABAQUS_RUNNER_PREFLIGHT.md", "docs/A6_2_EXACT_PR_VALIDATION.md", "docs/A7_1_ISOLATED_TARGET_VALIDATION.md", "docs/A7_2_IMPORT_PARTITION_REGRESSION.md"):
            anchors[path] = ("subsystem-contract", "autonomy", "current", "subordinate", True)
        for path in ("docs/A5_3_REPAIR_GIT_IDENTITY_ISSUE94.md", "docs/A5_3_REPAIR_WORKER.md", "docs/A5_3_REPAIR_WORKER_ISSUE92.md"):
            anchors[path] = ("historical-record", "autonomy", "historical", "none", False)
        for path in ("docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md", "docs/TERRA_MEDIUM_CANARY_V2.md", "docs/YELLOW_LANE_REPAIR_FIXTURE_V2.md"):
            anchors[path] = ("fixture-evidence", "autonomy", "retained-evidence", "none", False)
        for path, expected in anchors.items():
            entry = entries[path]
            self.assertEqual(tuple(entry[key] for key in ("kind", "domain", "status", "authority", "policy_input")), expected)

    def test_metadata_matrix_and_prose_independence(self):
        self.assertEqual(validate_metadata(self.index), METADATA)
        for replacement in ("", "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->",
                            "<!-- D1-GOVERNANCE-METADATA\n{}\nD1-GOVERNANCE-METADATA -->\n<!-- D1-GOVERNANCE-METADATA\n{}\nD1-GOVERNANCE-METADATA -->",
                            "D1-GOVERNANCE-METADATA -->\n<!-- D1-GOVERNANCE-METADATA\n{}",
                            "<!-- D1-GOVERNANCE-METADATA\nnot-json\nD1-GOVERNANCE-METADATA -->"):
            self.assertInvalid(validate_metadata, replacement)
        for key in METADATA:
            value = copy.deepcopy(METADATA); value.pop(key)
            self.assertInvalid(validate_metadata, "x<!-- D1-GOVERNANCE-METADATA\n" + json.dumps(value) + "\nD1-GOVERNANCE-METADATA -->x")
            value = copy.deepcopy(METADATA); value[key] = "wrong"
            self.assertInvalid(validate_metadata, "x<!-- D1-GOVERNANCE-METADATA\n" + json.dumps(value) + "\nD1-GOVERNANCE-METADATA -->x")
        changed = self.index.replace("This index describes", "Wrapped prose describes").replace("Conflicts fail closed", "Ordinary prose says conflicts fail closed")
        self.assertEqual(validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, changed), "PRE-COMMIT")


if __name__ == "__main__":
    unittest.main()
