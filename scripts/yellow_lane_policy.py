"""Pure policy primitives for the trusted GREEN/YELLOW issue router.

This module performs no I/O. Unicode titles drop non-ASCII characters before
slugging; an empty result uses ``yellow-work``.
"""
from __future__ import annotations

import dataclasses
import json
import re
from typing import Any, Mapping, Sequence

from scripts import codex_issue_worker as green

REPOSITORY = green.REPOSITORY
GREEN, YELLOW, REJECT = "GREEN", "YELLOW", "REJECT"
AUTOMATED_YELLOW_LANE = "automated-yellow"
SCHEMA_VERSION = 1
MAX_TITLE, MAX_SLUG, MAX_BRANCH = 300, 50, 128
MAX_LABELS, MAX_PATHS, MAX_PATH, MAX_SERIALIZED_EVIDENCE = 32, 32, 240, 8192
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
BRANCH_RE = re.compile(r"^codex-yellow/issue-([1-9][0-9]*)-([a-z0-9]+(?:-[a-z0-9]+)*)$")
PRESTART_KEYS = frozenset(("schema_version", "repository", "issue_number", "trusted_base_sha",
                           "declared_risk", "effective_risk", "authorized_paths",
                           "scientific_runtime_prohibited"))
CLAIM_KEYS = frozenset(("schema_version", "repository", "issue_number", "trusted_base_sha", "branch", "lane"))
ROUTE_KEYS = frozenset(("event_action", "added_label", "fresh_label_add", "labels", "contract",
                        "dependencies_satisfied", "duplicate_claim", "effective_risk", "scientific_scope",
                        "repository", "issue_number", "trusted_base_sha", "prestart_evidence"))


class PolicyError(ValueError):
    """A deterministic fail-closed policy validation error."""


@dataclasses.dataclass(frozen=True)
class PreStartEvidence:
    schema_version: int
    repository: str
    issue_number: int
    trusted_base_sha: str
    declared_risk: str
    effective_risk: str
    authorized_paths: tuple[str, ...]
    scientific_runtime_prohibited: bool


@dataclasses.dataclass(frozen=True)
class ClaimEvidence:
    schema_version: int
    repository: str
    issue_number: int
    trusted_base_sha: str
    branch: str
    lane: str


def _positive(value: Any, name: str = "issue number") -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise PolicyError(name + " is invalid")
    return value


def _sha(value: Any, name: str = "trusted base SHA") -> str:
    if not isinstance(value, str) or not SHA_RE.fullmatch(value):
        raise PolicyError(name + " is invalid")
    return value


def _repository(value: Any) -> str:
    if not isinstance(value, str) or not REPOSITORY_RE.fullmatch(value) or value != REPOSITORY:
        raise PolicyError("repository is invalid")
    return value


def _schema(value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value != SCHEMA_VERSION:
        raise PolicyError("schema version is invalid")
    return value


def _safe_path(value: Any) -> str:
    if not isinstance(value, str) or not value or len(value) > MAX_PATH or any(ord(c) < 32 for c in value):
        raise PolicyError("authorized path is invalid")
    if (value.startswith(("/", "\\")) or re.match(r"^[A-Za-z]:[/\\]", value) or "\\" in value
            or "*" in value or "?" in value or value in (".", "..") or "//" in value
            or any(part in ("", ".", "..") for part in value.split("/"))):
        raise PolicyError("authorized path is unsafe or ambiguous")
    return value


def _mapping(value: Any, keys: frozenset[str], name: str) -> Mapping[str, Any]:
    if not isinstance(value, Mapping) or frozenset(value) != keys:
        raise PolicyError(name + " has invalid keys")
    return value


def _strict_json(serialized: Any) -> dict[str, Any]:
    if not isinstance(serialized, str) or not serialized or len(serialized) > MAX_SERIALIZED_EVIDENCE:
        raise PolicyError("serialized evidence is invalid or oversized")
    def pairs(values: list[tuple[str, Any]]) -> dict[str, Any]:
        result = {}
        for key, value in values:
            if key in result:
                raise PolicyError("serialized evidence has duplicate keys")
            result[key] = value
        return result
    try:
        result = json.loads(serialized, object_pairs_hook=pairs)
    except PolicyError:
        raise
    except (TypeError, ValueError):
        raise PolicyError("serialized evidence is malformed") from None
    if not isinstance(result, dict):
        raise PolicyError("serialized evidence must be an object")
    return result


def yellow_branch(issue_number: int, title: str) -> str:
    number = _positive(issue_number)
    if (not isinstance(title, str) or not title or len(title) > MAX_TITLE or any(ord(c) < 32 for c in title)
            or "/" in title or "\\" in title or ".." in title):
        raise PolicyError("title is invalid or path-like")
    ascii_title = title.encode("ascii", "ignore").decode("ascii").lower()
    slug = re.sub(r"[^a-z0-9]+", "-", ascii_title).strip("-")[:MAX_SLUG].rstrip("-") or "yellow-work"
    return validate_yellow_branch("codex-yellow/issue-%d-%s" % (number, slug), number)


def validate_yellow_branch(branch: Any, issue_number: int | None = None) -> str:
    if (not isinstance(branch, str) or not branch or len(branch) > MAX_BRANCH or any(ord(c) < 32 for c in branch)):
        raise PolicyError("YELLOW branch is invalid")
    match = BRANCH_RE.fullmatch(branch)
    if match is None or (issue_number is not None and int(match.group(1)) != _positive(issue_number)):
        raise PolicyError("YELLOW branch does not match the issue")
    return branch


def validate_prestart(value: Any, expected_repository: str = REPOSITORY,
                      expected_issue: int | None = None, expected_base: str | None = None) -> PreStartEvidence:
    item = _mapping(value, PRESTART_KEYS, "pre-start evidence")
    paths = item["authorized_paths"]
    if not isinstance(paths, (list, tuple)) or not 1 <= len(paths) <= MAX_PATHS:
        raise PolicyError("authorized paths are invalid")
    try:
        validated_paths = tuple(sorted(_safe_path(path) for path in paths))
    except TypeError:
        raise PolicyError("authorized paths are invalid") from None
    if len(set(validated_paths)) != len(validated_paths):
        raise PolicyError("authorized paths are duplicated")
    evidence = PreStartEvidence(_schema(item["schema_version"]), _repository(item["repository"]),
                                _positive(item["issue_number"]), _sha(item["trusted_base_sha"]),
                                item["declared_risk"], item["effective_risk"], validated_paths,
                                item["scientific_runtime_prohibited"])
    if evidence.repository != expected_repository:
        raise PolicyError("pre-start repository does not match")
    if expected_issue is not None and evidence.issue_number != _positive(expected_issue):
        raise PolicyError("pre-start issue does not match")
    if expected_base is not None and evidence.trusted_base_sha != _sha(expected_base, "expected base SHA"):
        raise PolicyError("pre-start evidence is stale")
    if evidence.declared_risk != "yellow" or evidence.effective_risk != "yellow":
        raise PolicyError("pre-start evidence is not YELLOW")
    if evidence.scientific_runtime_prohibited is not True:
        raise PolicyError("scientific and runtime execution must be prohibited")
    return evidence


def serialize_prestart(value: Any, **expected: Any) -> str:
    evidence = value if isinstance(value, PreStartEvidence) else validate_prestart(value, **expected)
    evidence = validate_prestart(dataclasses.asdict(evidence), **expected)
    payload = dataclasses.asdict(evidence)
    payload["authorized_paths"] = list(evidence.authorized_paths)
    return json.dumps(payload, sort_keys=True, separators=(",", ":"))


def parse_prestart(serialized: Any, **expected: Any) -> PreStartEvidence:
    return validate_prestart(_strict_json(serialized), **expected)


def validate_claim(value: Any, expected_repository: str = REPOSITORY,
                   expected_issue: int | None = None, expected_base: str | None = None,
                   expected_branch: str | None = None) -> ClaimEvidence:
    item = _mapping(value, CLAIM_KEYS, "claim evidence")
    evidence = ClaimEvidence(_schema(item["schema_version"]), _repository(item["repository"]),
                             _positive(item["issue_number"]), _sha(item["trusted_base_sha"]),
                             validate_yellow_branch(item["branch"], item["issue_number"]), item["lane"])
    if evidence.repository != expected_repository:
        raise PolicyError("claim repository does not match")
    if expected_issue is not None and evidence.issue_number != _positive(expected_issue):
        raise PolicyError("claim issue does not match")
    if expected_base is not None and evidence.trusted_base_sha != _sha(expected_base, "expected base SHA"):
        raise PolicyError("claim evidence is stale")
    if expected_branch is not None and evidence.branch != validate_yellow_branch(expected_branch, evidence.issue_number):
        raise PolicyError("claim branch does not match")
    if evidence.lane != AUTOMATED_YELLOW_LANE:
        raise PolicyError("claim lane is invalid")
    return evidence


def serialize_claim(value: Any, **expected: Any) -> str:
    evidence = value if isinstance(value, ClaimEvidence) else validate_claim(value, **expected)
    evidence = validate_claim(dataclasses.asdict(evidence), **expected)
    return json.dumps(dataclasses.asdict(evidence), sort_keys=True, separators=(",", ":"))


def parse_claim(serialized: Any, **expected: Any) -> ClaimEvidence:
    return validate_claim(_strict_json(serialized), **expected)


def resolve_claim_replay(records: Any, candidate: Any, *, expected_repository: str = REPOSITORY,
                         expected_issue: int, expected_base: str) -> str:
    candidate_evidence = parse_claim(candidate, expected_repository=expected_repository,
                                     expected_issue=expected_issue, expected_base=expected_base)
    canonical_candidate = serialize_claim(candidate_evidence)
    if not isinstance(records, Sequence) or isinstance(records, (str, bytes)):
        raise PolicyError("claim records are invalid")
    if len(records) > 1:
        raise PolicyError("claim evidence is ambiguous")
    if not records:
        return "new"
    existing = parse_claim(records[0], expected_repository=expected_repository, expected_issue=expected_issue,
                           expected_base=expected_base, expected_branch=candidate_evidence.branch)
    if serialize_claim(existing) != canonical_candidate:
        raise PolicyError("claim evidence conflicts")
    return "idempotent"


def route(snapshot: Any) -> str:
    """Return GREEN/YELLOW/REJECT from a fully represented future trigger."""
    try:
        item = _mapping(snapshot, ROUTE_KEYS, "routing snapshot")
        if (item["event_action"] != "labeled" or item["added_label"] != "agent:codex"
                or item["fresh_label_add"] is not True):
            raise PolicyError("event is not a fresh agent:codex label-add")
        labels = item["labels"]
        if (not isinstance(labels, (list, tuple)) or not 1 <= len(labels) <= MAX_LABELS
                or not all(isinstance(label, str) and 0 < len(label) <= 100 for label in labels)
                or len(set(labels)) != len(labels)):
            raise PolicyError("labels are invalid")
        statuses = sorted(label for label in labels if label.startswith("status:"))
        risks = sorted(label for label in labels if label.startswith("risk:"))
        if statuses != ["status:ready"] or len(risks) != 1:
            raise PolicyError("status or risk labels are invalid")
        if (item["dependencies_satisfied"] is not True or item["duplicate_claim"] is not False
                or item["scientific_scope"] is not False):
            raise PolicyError("routing prerequisites are not satisfied")
        contract = green.parse_contract(item["contract"], REPOSITORY)
        if risks[0] == "risk:green" and contract.risk == "risk:green" and item["effective_risk"] == "green":
            if item["prestart_evidence"] is not None:
                raise PolicyError("GREEN routing does not consume YELLOW authorization")
            return GREEN
        if risks[0] == "risk:yellow" and contract.risk == "risk:yellow" and item["effective_risk"] == "yellow":
            parse_prestart(item["prestart_evidence"], expected_repository=_repository(item["repository"]),
                           expected_issue=_positive(item["issue_number"]), expected_base=_sha(item["trusted_base_sha"]))
            return YELLOW
    except (PolicyError, green.WorkerError, TypeError, ValueError, KeyError):
        return REJECT
    return REJECT
