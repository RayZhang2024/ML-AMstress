"""Pure, dormant-by-default validation for the exceptional D0 knowledge bootstrap.

This module has no I/O and cannot authorize implementation, runtime, repair,
or merge activity.  It only recognizes a tightly bounded, owner-authored
pre-start marker for the A5 coordinator to consider after its ordinary
protected-YELLOW checks have already established current PR identity.
"""
from __future__ import annotations

import dataclasses
import json
import re
from typing import Any, Mapping


REPOSITORY = "RayZhang2024/ML-AMstress"
TRUSTED_AUTHOR = "RayZhang2024"
MARKER_PREFIX = "<!-- knowledge-bootstrap-prestart:"
MARKER_SUFFIX = " -->"
SCHEMA_VERSION = 1
MAX_PATHS, MAX_PATH, MAX_BODY = 32, 240, 8192
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
KEYS = frozenset(("schema_version", "repository", "issue_number", "trusted_base_sha",
                  "authorized_paths", "purpose", "a5_mode", "scientific_runtime_prohibited"))
PROTECTED_DOCS = frozenset(("docs/AUTONOMOUS_DEVELOPMENT.md", "docs/AUTONOMOUS_ORCHESTRATION.md",
                            "docs/AUTONOMOUS_KNOWLEDGE_BOOTSTRAP.md", "docs/ABAQUS_MODEL_CONTRACT.md"))


class PolicyError(ValueError):
    """Fail-closed bootstrap evidence error."""


@dataclasses.dataclass(frozen=True)
class Evidence:
    schema_version: int
    repository: str
    issue_number: int
    trusted_base_sha: str
    authorized_paths: tuple[str, ...]
    purpose: str
    a5_mode: str
    scientific_runtime_prohibited: bool


def _safe_path(value: Any) -> str:
    if not isinstance(value, str) or not value or len(value) > MAX_PATH or any(ord(c) < 32 for c in value):
        raise PolicyError("authorized path is invalid")
    if (value.startswith(("/", "\\")) or "\\" in value or "//" in value or "*" in value or "?" in value
            or re.match(r"^[A-Za-z]:", value) or any(part in ("", ".", "..") for part in value.split("/"))):
        raise PolicyError("authorized path is unsafe")
    return value


def is_eligible_path(path: Any) -> bool:
    """Allow only explicitly descriptive repository knowledge identities."""
    try:
        value = _safe_path(path)
    except PolicyError:
        return False
    if value in ("README.md", "AGENTS.md"):
        return True
    if not value.startswith("docs/") or not value.endswith((".md", ".json")):
        return False
    if value in PROTECTED_DOCS or re.fullmatch(r"docs/A[4-7]_.*\.md", value):
        return False
    identities = set(token for token in re.split(r"[^a-z0-9]+", value.lower()) if token)
    return not bool(identities.intersection(("abaqus", "scientific", "model", "runtime", "gui", "ml", "workflow")))


def _strict_json(serialized: str) -> dict[str, Any]:
    def pairs(items: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, value in items:
            if key in result:
                raise PolicyError("duplicate JSON keys")
            result[key] = value
        return result
    try:
        value = json.loads(serialized, object_pairs_hook=pairs)
    except (ValueError, TypeError):
        raise PolicyError("marker JSON is malformed") from None
    if not isinstance(value, dict):
        raise PolicyError("marker JSON is not an object")
    return value


def validate(value: Any, *, expected_issue: int | None = None, expected_base: str | None = None) -> Evidence:
    if not isinstance(value, Mapping) or frozenset(value) != KEYS:
        raise PolicyError("marker schema is invalid")
    paths = value["authorized_paths"]
    if not isinstance(paths, list) or not 1 <= len(paths) <= MAX_PATHS:
        raise PolicyError("authorized paths are invalid")
    validated = tuple(_safe_path(path) for path in paths)
    if tuple(sorted(validated)) != validated or len(set(validated)) != len(validated) or not all(is_eligible_path(path) for path in validated):
        raise PolicyError("authorized paths are not sorted, unique, and knowledge-only")
    if (value["schema_version"] != SCHEMA_VERSION or isinstance(value["issue_number"], bool)
            or not isinstance(value["issue_number"], int) or value["issue_number"] < 1
            or value["repository"] != REPOSITORY or not isinstance(value["trusted_base_sha"], str)
            or not SHA_RE.fullmatch(value["trusted_base_sha"]) or value["purpose"] != "repository-knowledge-reconstruction"
            or value["a5_mode"] != "skip-review-and-repair" or value["scientific_runtime_prohibited"] is not True):
        raise PolicyError("marker required values are invalid")
    evidence = Evidence(SCHEMA_VERSION, REPOSITORY, value["issue_number"], value["trusted_base_sha"], validated,
                        value["purpose"], value["a5_mode"], True)
    if expected_issue is not None and evidence.issue_number != expected_issue:
        raise PolicyError("marker issue does not match")
    if expected_base is not None and evidence.trusted_base_sha != expected_base:
        raise PolicyError("marker base is stale")
    return evidence


def serialize(evidence: Evidence) -> str:
    payload = dataclasses.asdict(evidence)
    payload["authorized_paths"] = list(evidence.authorized_paths)
    return json.dumps(payload, sort_keys=True, separators=(",", ":"))


def parse_marker(body: Any, *, expected_issue: int | None = None, expected_base: str | None = None) -> Evidence:
    if not isinstance(body, str) or len(body) > MAX_BODY or not body.startswith(MARKER_PREFIX) or not body.endswith(MARKER_SUFFIX):
        raise PolicyError("marker is not standalone")
    serialized = body[len(MARKER_PREFIX):-len(MARKER_SUFFIX)]
    evidence = validate(_strict_json(serialized), expected_issue=expected_issue, expected_base=expected_base)
    if serialized != serialize(evidence):
        raise PolicyError("marker is not canonical")
    return evidence
