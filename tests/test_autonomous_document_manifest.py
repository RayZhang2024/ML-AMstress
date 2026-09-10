"""Protect the descriptive D1 document-authority manifest.

This test module deliberately contains the validation model rather than an
executable consumer.  Its Git helpers use raw, exact Git identities so tests
cannot manufacture identity evidence through filesystem resolution.
"""

from __future__ import print_function

import json
import os
import re
import subprocess
import unicodedata
from unittest import mock


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MANIFEST_PATH = os.path.join(ROOT, "docs", "AUTONOMOUS_DOCUMENT_MANIFEST.json")
INDEX_PATH = os.path.join(ROOT, "docs", "AUTONOMOUS_GOVERNANCE_INDEX.md")
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
    "architecture-reference", "roadmap", "historical-record",
    "fixture-evidence",
))
STATUS = frozenset(("current", "historical", "retained-evidence", "deprecated"))
AUTHORITIES = frozenset(("primary", "subordinate", "reference", "none"))
RISK_FLOORS = frozenset(("green", "yellow", "red"))


class ManifestError(ValueError):
    """A deliberately bounded manifest or Git-inventory diagnostic."""


def _fail(code):
    raise ManifestError("manifest-error:" + code)


def parse_git_z(raw, inventory):
    """Parse one exact Git -z inventory before any normalization or dedupe."""
    if not isinstance(raw, bytes):
        _fail("malformed-" + inventory)
    if raw and not raw.endswith(b"\0"):
        _fail("unterminated-" + inventory)
    if not raw:
        return []
    records = raw[:-1].split(b"\0")
    if not records or any(not item for item in records):
        _fail("empty-record-" + inventory)
    try:
        paths = [item.decode("utf-8", "strict") for item in records]
    except UnicodeDecodeError:
        _fail("undecodable-" + inventory)
    if len(paths) != len(set(paths)):
        _fail("duplicate-" + inventory)
    for path in paths:
        if (not path or path.startswith("/") or path.startswith("\\") or
                "\\" in path or re.match(r"^[A-Za-z]:", path) or
                path.startswith("//") or "/./" in path or path.startswith("./") or
                path.endswith("/.") or "/../" in path or path.startswith("../") or
                path.endswith("/..") or "//" in path):
            _fail("unsafe-path-" + inventory)
    return paths


def git_z_inventory(arguments, inventory):
    try:
        result = subprocess.run(
            ["git"] + list(arguments), cwd=ROOT, stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL, check=False)
    except (OSError, TypeError):
        _fail("acquisition-" + inventory)
    if result.returncode != 0:
        _fail("acquisition-" + inventory)
    return parse_git_z(result.stdout, inventory)


def exact_tracked_inventory():
    paths = git_z_inventory(["ls-files", "-z"], "tracked")
    if not paths:
        _fail("empty-tracked")
    return paths


def exact_untracked_inventory():
    return git_z_inventory(["ls-files", "--others", "--exclude-standard", "-z"], "untracked")


def reject_aliases(paths, inventory):
    seen = set()
    for path in paths:
        key = unicodedata.normalize("NFC", path).casefold()
        if key in seen:
            _fail("alias-" + inventory)
        seen.add(key)


def validate_pre_commit(tracked, untracked):
    reject_aliases(tracked, "tracked")
    reject_aliases(untracked, "untracked")
    combined = list(tracked) + list(untracked)
    reject_aliases(combined, "combined")
    if set(tracked) & set(untracked):
        _fail("duplicate-combined")
    if set(untracked) - D1_PATHS:
        _fail("unauthorized-candidate")
    return set(tracked) | set(untracked)


def validate_post_commit(tracked, untracked):
    validate_pre_commit(tracked, untracked)
    if untracked:
        _fail("candidate-active")
    if not D1_PATHS <= set(tracked):
        _fail("missing-tracked-d1")


def validate_phase_semantics(text):
    """Require phase-bound clauses, not a global bag of governance words."""
    paragraphs = [part.lower() for part in re.split(r"\n\s*\n", text) if part.strip()]
    before = re.compile(r"\b(before commit|before the worker commits|pre-commit)\b")
    after = re.compile(r"\b(after commit|once committed|post-commit|final head)\b")
    pre_ok = any(before.search(part) and "candidate" in part and
                 "authorized" in part and re.search(r"\b(permitted|allowed)\b", part)
                 for part in paragraphs)
    post_ok = any(after.search(part) and "exact tracked" in part and
                  re.search(r"candidate allowance (is )?inactive", part)
                  for part in paragraphs)
    if not (pre_ok and post_ok):
        _fail("phase-semantics")


def load_manifest():
    with open(MANIFEST_PATH, "r", encoding="utf-8") as handle:
        return json.load(handle)


def validate_manifest(manifest, identities):
    if set(manifest) != {"schema_version", "documents"} or manifest["schema_version"] != 1:
        _fail("schema")
    entries = manifest["documents"]
    if not isinstance(entries, list):
        _fail("documents")
    expected = {path for path in identities if path.startswith("docs/") and path.endswith(".md")}
    expected.update(("AGENTS.md", "README.md", ".github/ISSUE_TEMPLATE/autonomous-work.md"))
    paths = []
    for entry in entries:
        if set(entry) != FIELDS:
            _fail("fields")
        path = entry["path"]
        paths.append(path)
        if entry["kind"] not in KINDS or entry["status"] not in STATUS:
            _fail("taxonomy")
        if entry["authority"] not in AUTHORITIES or entry["risk_floor"] not in RISK_FLOORS:
            _fail("authority")
        for key in ("topics", "protecting_tests"):
            value = entry[key]
            if not isinstance(value, list) or value != sorted(set(value)):
                _fail("noncanonical-" + key)
        if entry["policy_input"] and not (entry["status"] == "current" and
                                            entry["authority"] in ("primary", "subordinate")):
            _fail("policy-input")
        if entry["kind"] in ("historical-record", "fixture-evidence") and entry["authority"] != "none":
            _fail("non-authority-history")
        if entry["kind"] == "fixture-evidence" and entry["status"] != "retained-evidence":
            _fail("fixture-status")
    if len(paths) != len(set(paths)) or set(paths) != expected:
        _fail("coverage")


def _entry(manifest, path):
    return next(item for item in manifest["documents"] if item["path"] == path)


def test_manifest_schema_coverage_taxonomy_and_anchors():
    manifest = load_manifest()
    identities = validate_pre_commit(exact_tracked_inventory(), exact_untracked_inventory())
    validate_manifest(manifest, identities)
    assert KINDS == {"policy", "orchestration-contract", "scientific-contract", "operational-runbook", "subsystem-contract", "developer-guide", "architecture-reference", "roadmap", "historical-record", "fixture-evidence"}
    assert not {"contract", "reference", "historical"} & KINDS
    anchors = {
        "docs/AUTONOMOUS_DEVELOPMENT.md": ("policy", "current", "primary", True),
        "docs/AUTONOMOUS_ORCHESTRATION.md": ("orchestration-contract", "current", "subordinate", True),
        "docs/A4_18_COMPLETION_OBSERVER.md": ("subsystem-contract", "current", "subordinate", True),
        "docs/ABAQUS_MODEL_CONTRACT.md": ("scientific-contract", "current", "primary", True),
        "docs/AUTONOMOUS_WORKER_RUNBOOK.md": ("operational-runbook", "current", "reference", False),
        "docs/A5_3_REPAIR_WORKER_ISSUE92.md": ("historical-record", "historical", "none", False),
        "docs/TERRA_MEDIUM_CANARY_V2.md": ("fixture-evidence", "retained-evidence", "none", False),
        "docs/AUTONOMOUS_GOVERNANCE_INDEX.md": ("architecture-reference", "current", "reference", False),
    }
    for path, expected in anchors.items():
        item = _entry(manifest, path)
        assert (item["kind"], item["status"], item["authority"], item["policy_input"]) == expected
    assert _entry(manifest, "docs/TERRA_MEDIUM_CANARY_V2.md")["topics"] == ["canary", "terra"]


def test_git_z_parser_and_identity_fail_closed():
    assert parse_git_z(b"a\0b\0", "x") == ["a", "b"]
    for raw in (b"a", b"a\0\0", b"a\0a\0", b"a\\b\0", b"../a\0", b"a//b\0", b"\xff\0"):
        try:
            parse_git_z(raw, "x")
        except ManifestError:
            pass
        else:
            raise AssertionError("unsafe inventory accepted")
    for paths in ((["A", "a"], []),
                  ([], ["docs/AUTONOMOUS_DOCUMENT_MANIFEST.json",
                        "DOCS/autonomous_document_manifest.json"]),
                  (["A"], ["a"]),
                  (["caf\u00e9"], ["cafe\u0301"])):
        try:
            validate_pre_commit(*paths)
        except ManifestError:
            pass
        else:
            raise AssertionError("alias accepted")
    assert validate_pre_commit(["AGENTS.md"], list(D1_PATHS))
    validate_post_commit(["AGENTS.md"] + list(D1_PATHS), [])
    try:
        validate_pre_commit([], ["Docs/AUTONOMOUS_DOCUMENT_MANIFEST.json"])
    except ManifestError:
        pass
    else:
        raise AssertionError("wrong-case candidate accepted")
    try:
        validate_post_commit(["AGENTS.md"] + list(D1_PATHS), ["docs/x.md"])
    except ManifestError:
        pass
    else:
        raise AssertionError("post-commit candidate accepted")
    with mock.patch(__name__ + ".subprocess.run", side_effect=OSError()):
        try:
            git_z_inventory(["ls-files", "-z"], "tracked")
        except ManifestError:
            pass
        else:
            raise AssertionError("acquisition failure accepted")


def test_phase_semantics_are_bound_to_the_correct_phase():
    good = (
        "Before commit, only authorized candidate identities are permitted.\n\n"
        "After commit, exact tracked identity is required and candidate allowance is inactive.")
    worker = (
        "Before the worker commits, authorized candidate identities are allowed.\n\n"
        "Once committed, exact tracked identity is required and candidate allowance is inactive.")
    structured = (
        "pre-commit: authorized candidate identities are permitted.\n\n"
        "post-commit: exact tracked identity is required and candidate allowance is inactive.")
    for text in (good, worker, structured):
        validate_phase_semantics(text)
    inverted = (
        "Pre-commit uses exact tracked identity and candidate allowance is inactive.\n\n"
        "Post-commit permits authorized candidate identities.\n\n"
        "Before commit and after commit are both recorded.")
    try:
        validate_phase_semantics(inverted)
    except ManifestError:
        pass
    else:
        raise AssertionError("inverted phase semantics accepted")


def test_production_governance_index_has_phase_bound_meaning():
    with open(INDEX_PATH, "r", encoding="utf-8") as handle:
        validate_phase_semantics(handle.read())
