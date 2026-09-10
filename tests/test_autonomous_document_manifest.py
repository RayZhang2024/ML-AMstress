"""Deterministic, Git-backed validation for the descriptive D1 document map."""
import copy
import json
import re
import subprocess
import unicodedata
import unittest
from unittest import mock


D1_PATHS = frozenset(("docs/AUTONOMOUS_DOCUMENT_MANIFEST.json", "docs/AUTONOMOUS_GOVERNANCE_INDEX.md", "tests/test_autonomous_document_manifest.py"))
SCHEMA_FIELDS = frozenset(("path", "kind", "domain", "status", "authority", "policy_input", "protected", "risk_floor", "topics", "protecting_tests"))
KINDS = frozenset(("policy", "orchestration-contract", "scientific-contract", "operational-runbook", "subsystem-contract", "developer-guide", "architecture-reference", "roadmap", "historical-record", "fixture-evidence"))
STATUSES = frozenset(("current", "historical", "retained-evidence", "deprecated"))
AUTHORITIES = frozenset(("primary", "subordinate", "reference", "none"))
RISK_FLOORS = frozenset(("green", "yellow", "red"))
IDENTIFIER = re.compile(r"^[a-z][a-z0-9-]{0,63}$")
METADATA = {"schema_version": 1, "authority_scope": "domain-scoped", "authority_precedence": "primary-then-subordinate", "scientific_contract": "separate", "non_authority_roles": ["fixture-evidence", "history", "reference", "roadmap", "runbook"], "pre_commit_identity": "exact-authorized-d1-candidates", "post_commit_identity": "exact-tracked-d1-identities", "post_commit_candidate_allowance": "inactive", "conflict_handling": "fail-closed-escalate"}


class ValidationError(ValueError):
    """Bounded validation failure; diagnostics never contain Git output."""


def comparison_key(path):
    return unicodedata.normalize("NFC", path).casefold()


def valid_path(path):
    if not isinstance(path, str) or not path or path.startswith("/") or "\\" in path or re.match(r"^[A-Za-z]:/", path):
        return False
    if unicodedata.normalize("NFC", path) != path or any(ord(char) < 32 for char in path):
        return False
    return all(part not in ("", ".", "..") for part in path.split("/"))


def parse_z(raw, inventory):
    """Parse successful Git -z output before identity reduction or normalization."""
    if inventory not in {"tracked", "untracked", "ignored"}:
        raise ValidationError("invalid Git inventory kind")
    if not isinstance(raw, bytes):
        raise ValidationError("invalid Git inventory protocol")
    # Empty output is a state, not an unterminated record; only candidate lists
    # and ignored lists can be empty after a successful acquisition.
    if raw == b"":
        if inventory in {"untracked", "ignored"}:
            return ()
        raise ValidationError("invalid Git inventory protocol")
    if not raw.endswith(b"\0"):
        raise ValidationError("invalid Git inventory protocol")
    records = raw[:-1].split(b"\0")
    if any(not record for record in records):
        raise ValidationError("invalid Git inventory record")
    try:
        paths = tuple(record.decode("utf-8", "strict") for record in records)
    except UnicodeDecodeError:
        raise ValidationError("undecodable Git identity")
    if len(paths) != len(set(paths)) or any(not valid_path(path) for path in paths):
        raise ValidationError("unsafe or noncanonical Git identity")
    return paths


def acquire_git_inventory(args, inventory):
    result = subprocess.run(["git"] + list(args), stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=False)
    if result.returncode:
        raise ValidationError("Git inventory acquisition failed")
    return parse_z(result.stdout, inventory)


def validate_inventories(tracked, untracked, ignored):
    inventories = tuple(tuple(values) for values in (tracked, untracked, ignored))
    if not inventories[0]:
        raise ValidationError("empty tracked Git inventory")
    for values in inventories:
        if len(values) != len(set(values)) or any(not valid_path(path) for path in values):
            raise ValidationError("invalid validated Git inventory")
    return inventories


def d1_relevant(path):
    return path in D1_PATHS or comparison_key(path) in {comparison_key(item) for item in D1_PATHS}


def classify_phase(tracked, untracked, ignored):
    tracked, untracked, ignored = validate_inventories(tracked, untracked, ignored)
    relevant = tuple(tuple(path for path in values if d1_relevant(path)) for values in (tracked, untracked, ignored))
    if relevant[2]:
        raise ValidationError("D1 ignored evidence")
    if any(path not in D1_PATHS for group in relevant for path in group):
        raise ValidationError("D1 alias evidence")
    locations = {path: [index for index, group in enumerate(relevant) if path in group] for path in D1_PATHS}
    if any(len(where) > 1 for where in locations.values()):
        raise ValidationError("D1 cross-inventory evidence")
    tracked_exact = {path for path, where in locations.items() if where == [0]}
    untracked_exact = {path for path, where in locations.items() if where == [1]}
    if not tracked_exact and untracked_exact == D1_PATHS:
        return "PRE-COMMIT"
    if tracked_exact == D1_PATHS and not untracked_exact:
        return "POST-COMMIT"
    raise ValidationError("invalid D1 phase")


def expected_document_paths(tracked, phase):
    paths = {path for path in tracked if path.startswith("docs/") and path.count("/") == 1 and path.endswith(".md")}
    paths.update(("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"))
    if phase == "PRE-COMMIT":
        paths.add("docs/AUTONOMOUS_GOVERNANCE_INDEX.md")
    return paths


def validate_metadata(markdown):
    start, end = "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->"
    if markdown.count(start) != 1 or markdown.count(end) != 1 or markdown.index(start) > markdown.index(end):
        raise ValidationError("invalid governance metadata delimiters")
    before, tail = markdown.split(start, 1)
    payload, after = tail.split(end, 1)
    if not before or not after:
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
    if not isinstance(manifest, dict) or set(manifest) != {"schema_version", "documents"} or manifest["schema_version"] != 1 or not isinstance(manifest["documents"], list):
        raise ValidationError("invalid manifest schema")
    allowed = set(tracked) | (D1_PATHS if phase == "PRE-COMMIT" else set())
    paths = []
    for entry in manifest["documents"]:
        if not isinstance(entry, dict) or set(entry) != SCHEMA_FIELDS:
            raise ValidationError("invalid manifest entry schema")
        path = entry["path"]; paths.append(path)
        if not valid_path(path) or path not in allowed or (phase == "PRE-COMMIT" and path in D1_PATHS and path != "docs/AUTONOMOUS_GOVERNANCE_INDEX.md"):
            raise ValidationError("invalid manifest identity")
        if entry["kind"] not in KINDS or entry["status"] not in STATUSES or entry["authority"] not in AUTHORITIES or entry["risk_floor"] not in RISK_FLOORS:
            raise ValidationError("invalid manifest enum")
        if not isinstance(entry["policy_input"], bool) or not isinstance(entry["protected"], bool) or not IDENTIFIER.fullmatch(entry["domain"]):
            raise ValidationError("invalid manifest field")
        for field in ("topics", "protecting_tests"):
            values = entry[field]
            if not isinstance(values, list) or not values or values != sorted(values) or len(values) != len(set(values)):
                raise ValidationError("invalid manifest list")
        if any(not IDENTIFIER.fullmatch(topic) for topic in entry["topics"]):
            raise ValidationError("invalid manifest topic")
        test_allowed = set(tracked) | ({"tests/test_autonomous_document_manifest.py"} if phase == "PRE-COMMIT" else set())
        for target in entry["protecting_tests"]:
            if not valid_path(target) or target not in test_allowed or not target.startswith("tests/") or not target.rsplit("/", 1)[-1].startswith("test_") or not target.endswith(".py"):
                raise ValidationError("invalid protecting test")
        non_policy = entry["authority"] in {"reference", "none"} or entry["status"] in {"historical", "deprecated", "retained-evidence"}
        if entry["policy_input"] != (entry["status"] == "current" and entry["authority"] in {"primary", "subordinate"}) or (non_policy and entry["policy_input"]):
            raise ValidationError("inconsistent policy input")
        if entry["kind"] in {"historical-record", "fixture-evidence"} and entry["authority"] != "none":
            raise ValidationError("historical or fixture authority")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            raise ValidationError("fixture status")
    if paths != sorted(paths) or len(paths) != len(set(paths)) or set(paths) != expected_document_paths(tracked, phase):
        raise ValidationError("manifest coverage mismatch")
    if governance_markdown is not None:
        validate_metadata(governance_markdown)
        for heading in ("# Autonomous governance index", "## Purpose and scope", "## Taxonomy and authority", "## Exact Git identity and D1 phases", "## Conflict handling"):
            if heading not in governance_markdown:
                raise ValidationError("missing governance heading")
    return phase


class DocumentManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tracked = acquire_git_inventory(("ls-files", "-z"), "tracked")
        cls.untracked = acquire_git_inventory(("ls-files", "--others", "--exclude-standard", "-z"), "untracked")
        cls.ignored = acquire_git_inventory(("ls-files", "--others", "--ignored", "--exclude-standard", "-z"), "ignored")
        with open("docs/AUTONOMOUS_DOCUMENT_MANIFEST.json", encoding="utf-8") as source: cls.manifest = json.load(source)
        with open("docs/AUTONOMOUS_GOVERNANCE_INDEX.md", encoding="utf-8") as source: cls.index = source.read()

    def assertInvalid(self, callback, *args):
        with self.assertRaises(ValidationError): callback(*args)

    def test_empty_inventories_are_kind_specific(self):
        self.assertInvalid(parse_z, b"", "tracked")
        self.assertEqual(parse_z(b"", "untracked"), ())
        self.assertEqual(parse_z(b"", "ignored"), ())
        for kind in ("tracked", "untracked", "ignored"):
            with mock.patch("subprocess.run", return_value=mock.Mock(returncode=0, stdout=b"")):
                if kind == "tracked": self.assertInvalid(acquire_git_inventory, ("ls-files", "-z"), kind)
                else: self.assertEqual(acquire_git_inventory(("ls-files", "-z"), kind), ())

    def test_protocol_and_canonical_identity_matrix(self):
        self.assertEqual(parse_z(b"AGENTS.md\0docs/A.md\0", "tracked"), ("AGENTS.md", "docs/A.md"))
        bad = (b"AGENTS.md", b"AGENTS.md\0\0docs/A.md\0", b"AGENTS.md\0AGENTS.md\0", b"/absolute\0", b"C:/drive\0", b"a\\b\0", b"a//b\0", b"./a\0", b"a/../b\0", b"docs/e\xcc\x81.md\0", b"docs/a\tb.md\0", b"docs/a\nb.md\0", b"docs/a\x1fb.md\0", b"\xff\0")
        for raw in bad: self.assertInvalid(parse_z, raw, "tracked")
        with mock.patch("subprocess.run", return_value=mock.Mock(returncode=1, stdout=b"")):
            self.assertInvalid(acquire_git_inventory, ("ls-files", "-z"), "tracked")

    def test_pre_and_post_commit_phase_truth_table(self):
        base, candidates = ("AGENTS.md",), tuple(sorted(D1_PATHS))
        self.assertEqual(classify_phase(base, candidates, ()), "PRE-COMMIT")
        post = tuple(sorted(set(base) | D1_PATHS))
        self.assertEqual(classify_phase(post, (), ()), "POST-COMMIT")
        for missing in D1_PATHS:
            self.assertInvalid(classify_phase, base, tuple(p for p in candidates if p != missing), ())
            self.assertInvalid(classify_phase, tuple(p for p in post if p != missing), (), ())
        for groups in ((base, candidates, ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",)), (post, ("docs/AUTONOMOUS_GOVERNANCE_INDEX.md",), ()), (base, candidates + ("docs/autonomous_governance_index.md",), ()), (base, candidates + ("docſ/AUTONOMOUS_GOVERNANCE_INDEX.md",), ()), (base, candidates + ("DOCſ/AUTONOMOUS_GOVERNANCE_INDEX.MD",), ())):
            self.assertInvalid(classify_phase, *groups)
        self.assertEqual(classify_phase(base + ("docs/Foo.md",), candidates + ("docs/foo.md",), ()), "PRE-COMMIT")

    def test_current_and_synthetic_post_commit_manifest(self):
        self.assertEqual(validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, self.index), "PRE-COMMIT")
        post = tuple(sorted(set(self.tracked) | D1_PATHS))
        self.assertEqual(validate_manifest(self.manifest, post, (), (), self.index), "POST-COMMIT")

    def test_manifest_and_protecting_test_negative_matrix(self):
        for field, value in (("kind", "bad"), ("status", "bad"), ("authority", "bad"), ("risk_floor", None), ("domain", "Upper"), ("domain", "a" * 65), ("topics", []), ("topics", ["b", "a"]), ("topics", ["a", "a"]), ("protecting_tests", [])):
            altered = copy.deepcopy(self.manifest); altered["documents"][0][field] = value
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for target in ("Tests/test_autonomous_document_manifest.py", "teſts/test_autonomous_document_manifest.py", "tests/AUTONOMOUS_DOCUMENT_MANIFEST.py", "tests/missing.py", "../tests/test_bad.py", "docs/test_bad.py", "tests/check_bad.py", "tests/test_bad.txt"):
            altered = copy.deepcopy(self.manifest); altered["documents"][0]["protecting_tests"] = [target]
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for field in SCHEMA_FIELDS:
            altered = copy.deepcopy(self.manifest); altered["documents"][0].pop(field)
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        altered = copy.deepcopy(self.manifest); altered["documents"][0]["extra"] = True
        self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)
        for value in ("", "1a", "a_b", "a b", "é"):
            altered = copy.deepcopy(self.manifest); altered["documents"][0]["topics"] = [value]
            self.assertInvalid(validate_manifest, altered, self.tracked, self.untracked, self.ignored)

    def test_git_backed_coverage_anchor_and_metadata_matrices(self):
        tracked = ("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md", "docs/A.md", "docs/UPPER.MD", "docs/nested/A.md")
        self.assertEqual(expected_document_paths(tracked, "PRE-COMMIT"), {"AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md", "docs/A.md", "docs/AUTONOMOUS_GOVERNANCE_INDEX.md"})
        self.assertNotIn("docs/AUTONOMOUS_GOVERNANCE_INDEX.md", expected_document_paths(tracked, "POST-COMMIT"))
        entries = {entry["path"]: entry for entry in self.manifest["documents"]}
        anchors = {"docs/AUTONOMOUS_DEVELOPMENT.md": ("policy", "autonomy", "current", "primary", True), "docs/AUTONOMOUS_ORCHESTRATION.md": ("orchestration-contract", "autonomy", "current", "subordinate", True), ".github/ISSUE_TEMPLATE/autonomous-work.md": ("orchestration-contract", "autonomy", "current", "subordinate", True), "docs/AUTONOMOUS_WORKER_RUNBOOK.md": ("operational-runbook", "autonomy", "current", "reference", False), "docs/AUTONOMOUS_TROUBLESHOOTING.md": ("operational-runbook", "autonomy", "current", "reference", False), "docs/CODEX_PROMPT_GUIDE.md": ("developer-guide", "autonomy", "current", "reference", False), "docs/ABAQUS_MODEL_CONTRACT.md": ("scientific-contract", "scientific-model", "current", "primary", True), "docs/AUTONOMOUS_GOVERNANCE_INDEX.md": ("architecture-reference", "autonomy", "current", "reference", False)}
        for path in ("docs/A4_18_COMPLETION_OBSERVER.md", "docs/A5_1_REVIEWER.md", "docs/A5_2_REVIEW_STATE.md", "docs/A5_4A_REVIEW_LOOP.md", "docs/A6_1_ABAQUS_RUNNER_PREFLIGHT.md", "docs/A6_2_EXACT_PR_VALIDATION.md", "docs/A7_1_ISOLATED_TARGET_VALIDATION.md", "docs/A7_2_IMPORT_PARTITION_REGRESSION.md"):
            anchors[path] = ("subsystem-contract", "autonomy", "current", "subordinate", True)
        for path in ("docs/A5_3_REPAIR_GIT_IDENTITY_ISSUE94.md", "docs/A5_3_REPAIR_WORKER.md", "docs/A5_3_REPAIR_WORKER_ISSUE92.md"):
            anchors[path] = ("historical-record", "autonomy", "historical", "none", False)
        for path in ("docs/TERRA_HIGH_REVIEWER_CLEAN_CANARY_V3.md", "docs/TERRA_MEDIUM_CANARY_V2.md", "docs/YELLOW_LANE_REPAIR_FIXTURE_V2.md"):
            anchors[path] = ("fixture-evidence", "autonomy", "retained-evidence", "none", False)
        for path, expected in anchors.items():
            self.assertEqual(tuple(entries[path][key] for key in ("kind", "domain", "status", "authority", "policy_input")), expected)
        self.assertEqual(validate_metadata(self.index), METADATA)
        for value in ("", "<!-- D1-GOVERNANCE-METADATA", "D1-GOVERNANCE-METADATA -->", "<!-- D1-GOVERNANCE-METADATA\nnot-json\nD1-GOVERNANCE-METADATA -->"):
            self.assertInvalid(validate_metadata, value)
        for key in METADATA:
            altered = copy.deepcopy(METADATA); altered.pop(key)
            self.assertInvalid(validate_metadata, "x<!-- D1-GOVERNANCE-METADATA\n" + json.dumps(altered) + "\nD1-GOVERNANCE-METADATA -->x")
            altered = copy.deepcopy(METADATA); altered[key] = "wrong"
            self.assertInvalid(validate_metadata, "x<!-- D1-GOVERNANCE-METADATA\n" + json.dumps(altered) + "\nD1-GOVERNANCE-METADATA -->x")
        changed = self.index.replace("This index describes", "Other prose describes")
        self.assertEqual(validate_manifest(self.manifest, self.tracked, self.untracked, self.ignored, changed), "PRE-COMMIT")


if __name__ == "__main__":
    unittest.main()
