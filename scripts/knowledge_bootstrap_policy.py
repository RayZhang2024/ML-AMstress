"""Pure, fail-closed policy for the exceptional D0 knowledge bootstrap.

This module deliberately performs no I/O and grants no implementation, runtime,
review, repair, or merge authority.  It only validates the owner-authored
pre-start marker consumed by the A5 coordinator after ordinary YELLOW checks.
"""
from __future__ import annotations

import dataclasses
import json
import re
from typing import Any, Mapping, Sequence

REPOSITORY = "RayZhang2024/ML-AMstress"
SCHEMA_VERSION = 1
MARKER_PREFIX = "<!-- knowledge-bootstrap-prestart:"
MARKER_SUFFIX = " -->"
TRUSTED_AUTHOR = "RayZhang2024"
MAX_SERIALIZED = 8192
MAX_PATHS = 32
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
KEYS = frozenset(("schema_version", "repository", "issue_number", "trusted_base_sha",
                  "authorized_paths", "purpose", "a5_mode", "scientific_runtime_prohibited"))


class PolicyError(ValueError):
    """Bootstrap evidence is missing, ambiguous, or unsafe."""


@dataclasses.dataclass(frozen=True)
class BootstrapEvidence:
    schema_version: int
    repository: str
    issue_number: int
    trusted_base_sha: str
    authorized_paths: tuple[str, ...]
    purpose: str
    a5_mode: str
    scientific_runtime_prohibited: bool


def _object(text: Any) -> dict[str, Any]:
    if not isinstance(text, str) or not text or len(text) > MAX_SERIALIZED:
        raise PolicyError("bootstrap evidence is invalid")
    def no_duplicates(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result = {}
        for key, value in pairs:
            if key in result:
                raise PolicyError("bootstrap evidence has duplicate keys")
            result[key] = value
        return result
    try:
        result = json.loads(text, object_pairs_hook=no_duplicates)
    except (TypeError, ValueError):
        raise PolicyError("bootstrap evidence is malformed") from None
    if not isinstance(result, dict) or frozenset(result) != KEYS:
        raise PolicyError("bootstrap evidence has invalid keys")
    return result


def _path(path: Any) -> str:
    if not isinstance(path, str) or not path or len(path) > 240 or any(ord(c) < 32 for c in path):
        raise PolicyError("bootstrap path is invalid")
    if (path.startswith(("/", "\\")) or "\\" in path or "//" in path or "*" in path or "?" in path
            or any(part in ("", ".", "..") for part in path.split("/"))):
        raise PolicyError("bootstrap path is unsafe")
    return path


def eligible_path(path: Any) -> bool:
    """Allow only descriptive repository-knowledge identities."""
    try:
        path = _path(path)
    except PolicyError:
        return False
    if path in ("README.md", "AGENTS.md"):
        return True
    if not path.startswith("docs/") or not path.endswith((".md", ".json")):
        return False
    blocked = ("docs/AUTONOMOUS_DEVELOPMENT.md", "docs/AUTONOMOUS_ORCHESTRATION.md",
               "docs/AUTONOMOUS_KNOWLEDGE_BOOTSTRAP.md", "docs/ABAQUS_MODEL_CONTRACT.md")
    name = path.rsplit("/", 1)[-1]
    return path not in blocked and not name.startswith(("A4_", "A5_", "A6_", "A7_"))


def validate(value: Any, *, expected_issue: int | None = None,
             expected_base: str | None = None, expected_paths: Sequence[str] | None = None) -> BootstrapEvidence:
    if isinstance(value, BootstrapEvidence):
        item = dataclasses.asdict(value)
        item["authorized_paths"] = list(value.authorized_paths)
    else:
        item = value if isinstance(value, Mapping) else _object(value)
    if not isinstance(item, Mapping) or frozenset(item) != KEYS:
        raise PolicyError("bootstrap evidence has invalid keys")
    paths = item.get("authorized_paths")
    if not isinstance(paths, list) or not 1 <= len(paths) <= MAX_PATHS:
        raise PolicyError("bootstrap paths are invalid")
    parsed = tuple(_path(path) for path in paths)
    if parsed != tuple(sorted(parsed)) or len(set(parsed)) != len(parsed) or not all(eligible_path(path) for path in parsed):
        raise PolicyError("bootstrap paths are not an exact eligible allowlist")
    if (item.get("schema_version") != SCHEMA_VERSION or item.get("repository") != REPOSITORY
            or isinstance(item.get("issue_number"), bool) or not isinstance(item.get("issue_number"), int)
            or item["issue_number"] < 1 or not isinstance(item.get("trusted_base_sha"), str)
            or not SHA_RE.fullmatch(item["trusted_base_sha"]) or item.get("purpose") != "repository-knowledge-reconstruction"
            or item.get("a5_mode") != "skip-review-and-repair" or item.get("scientific_runtime_prohibited") is not True):
        raise PolicyError("bootstrap evidence has invalid values")
    evidence = BootstrapEvidence(SCHEMA_VERSION, REPOSITORY, item["issue_number"], item["trusted_base_sha"], parsed,
                                 item["purpose"], item["a5_mode"], True)
    if expected_issue is not None and evidence.issue_number != expected_issue:
        raise PolicyError("bootstrap issue does not match")
    if expected_base is not None and evidence.trusted_base_sha != expected_base:
        raise PolicyError("bootstrap base is stale")
    if expected_paths is not None and evidence.authorized_paths != tuple(expected_paths):
        raise PolicyError("bootstrap paths do not match YELLOW authorization")
    return evidence


def serialize(value: Any, **expected: Any) -> str:
    evidence = validate(value, **expected)
    return json.dumps({"schema_version": evidence.schema_version, "repository": evidence.repository,
                       "issue_number": evidence.issue_number, "trusted_base_sha": evidence.trusted_base_sha,
                       "authorized_paths": list(evidence.authorized_paths), "purpose": evidence.purpose,
                       "a5_mode": evidence.a5_mode,
                       "scientific_runtime_prohibited": evidence.scientific_runtime_prohibited},
                      sort_keys=True, separators=(",", ":"))


def marker(evidence: Any, **expected: Any) -> str:
    return MARKER_PREFIX + serialize(evidence, **expected) + MARKER_SUFFIX


def authorized_marker(comments: Sequence[Mapping[str, Any]]) -> BootstrapEvidence | None:
    """Return exactly one canonical owner marker; absence is not an error."""
    found = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or not body.startswith(MARKER_PREFIX):
            continue
        author = comment.get("user") if isinstance(comment, Mapping) else None
        if not isinstance(author, Mapping) or author.get("login") != TRUSTED_AUTHOR:
            raise PolicyError("bootstrap marker author is untrusted")
        if not body.endswith(MARKER_SUFFIX) or body != body.strip():
            raise PolicyError("bootstrap marker is not standalone")
        payload = body[len(MARKER_PREFIX):-len(MARKER_SUFFIX)]
        evidence = validate(payload)
        if marker(evidence) != body:
            raise PolicyError("bootstrap marker is not canonical")
        found.append(evidence)
    if not found:
        return None
    if len(found) != 1:
        raise PolicyError("bootstrap marker is ambiguous")
    return found[0]
