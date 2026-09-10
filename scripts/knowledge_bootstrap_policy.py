"""Pure, fail-closed policy for the exceptional D0 knowledge-bootstrap lane."""
from __future__ import annotations

import dataclasses
import json
import re
from typing import Any, Mapping, Sequence

REPOSITORY = "RayZhang2024/ML-AMstress"
TRUSTED_AUTHOR = "RayZhang2024"
SCHEMA_VERSION = 1
MAX_PATHS, MAX_PATH, MAX_SERIALIZED = 32, 240, 8192
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
MARKER_RE = re.compile(r"^<!-- knowledge-bootstrap-prestart:(\{.*\}) -->$")
MARKER_PREFIX = "<!-- knowledge-bootstrap-prestart:"
KEYS = frozenset(("schema_version", "repository", "issue_number", "trusted_base_sha", "authorized_paths", "purpose", "a5_mode", "scientific_runtime_prohibited"))

class PolicyError(ValueError): pass

@dataclasses.dataclass(frozen=True)
class BootstrapEvidence:
    schema_version: int; repository: str; issue_number: int; trusted_base_sha: str
    authorized_paths: tuple[str, ...]; purpose: str; a5_mode: str; scientific_runtime_prohibited: bool

def _positive(value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1: raise PolicyError("issue number is invalid")
    return value
def _sha(value: Any) -> str:
    if not isinstance(value, str) or not SHA_RE.fullmatch(value): raise PolicyError("trusted base SHA is invalid")
    return value
def _path(value: Any) -> str:
    if (not isinstance(value, str) or not value or len(value) > MAX_PATH or "\\" in value or value.startswith("/") or "//" in value or any(part in ("", ".", "..") for part in value.split("/"))): raise PolicyError("authorized path is invalid")
    return value
def bootstrap_path_eligible(path: Any) -> bool:
    try: path = _path(path)
    except PolicyError: return False
    if path in ("README.md", "AGENTS.md"): return True
    if not path.startswith("docs/") or not path.endswith((".md", ".json")): return False
    name = path.rsplit("/", 1)[-1]
    return not (path in ("docs/AUTONOMOUS_DEVELOPMENT.md", "docs/AUTONOMOUS_ORCHESTRATION.md", "docs/AUTONOMOUS_KNOWLEDGE_BOOTSTRAP.md", "docs/ABAQUS_MODEL_CONTRACT.md") or name.startswith(("A4_", "A5_", "A6_", "A7_")))
def _strict_json(serialized: Any) -> dict[str, Any]:
    if not isinstance(serialized, str) or not serialized or len(serialized) > MAX_SERIALIZED: raise PolicyError("bootstrap JSON is invalid")
    def pairs(items: list[tuple[str, Any]]) -> dict[str, Any]:
        result = {}
        for key, value in items:
            if key in result: raise PolicyError("bootstrap JSON has duplicate keys")
            result[key] = value
        return result
    try: value = json.loads(serialized, object_pairs_hook=pairs)
    except (TypeError, ValueError): raise PolicyError("bootstrap JSON is malformed") from None
    if not isinstance(value, dict): raise PolicyError("bootstrap JSON must be an object")
    return value
def validate_evidence(value: Any, *, expected_issue: int | None = None, expected_base: str | None = None) -> BootstrapEvidence:
    if not isinstance(value, Mapping) or frozenset(value) != KEYS: raise PolicyError("bootstrap evidence has invalid keys")
    paths = value["authorized_paths"]
    if not isinstance(paths, list) or not 1 <= len(paths) <= MAX_PATHS: raise PolicyError("authorized paths are invalid")
    checked = tuple(_path(path) for path in paths)
    if checked != tuple(sorted(checked)) or len(checked) != len(set(checked)) or not all(map(bootstrap_path_eligible, checked)): raise PolicyError("authorized paths are not eligible canonical knowledge paths")
    evidence = BootstrapEvidence(value["schema_version"], value["repository"], value["issue_number"], value["trusted_base_sha"], checked, value["purpose"], value["a5_mode"], value["scientific_runtime_prohibited"])
    if (isinstance(evidence.schema_version, bool) or evidence.schema_version != SCHEMA_VERSION or evidence.repository != REPOSITORY or _positive(evidence.issue_number) != evidence.issue_number or _sha(evidence.trusted_base_sha) != evidence.trusted_base_sha or evidence.purpose != "repository-knowledge-reconstruction" or evidence.a5_mode != "skip-review-and-repair" or evidence.scientific_runtime_prohibited is not True): raise PolicyError("bootstrap evidence has unsupported values")
    if expected_issue is not None and evidence.issue_number != _positive(expected_issue): raise PolicyError("bootstrap issue does not match")
    if expected_base is not None and evidence.trusted_base_sha != _sha(expected_base): raise PolicyError("bootstrap base is stale")
    return evidence
def serialize_evidence(value: Any, **expected: Any) -> str:
    raw = dataclasses.asdict(value) if isinstance(value, BootstrapEvidence) else value
    if isinstance(raw, dict) and isinstance(raw.get("authorized_paths"), tuple): raw["authorized_paths"] = list(raw["authorized_paths"])
    evidence = validate_evidence(raw, **expected)
    payload = dataclasses.asdict(evidence); payload["authorized_paths"] = list(evidence.authorized_paths)
    return json.dumps(payload, sort_keys=True, separators=(",", ":"))
def parse_evidence(serialized: Any, **expected: Any) -> BootstrapEvidence:
    evidence = validate_evidence(_strict_json(serialized), **expected)
    if serialize_evidence(evidence) != serialized: raise PolicyError("bootstrap JSON is not canonical")
    return evidence
def extract_evidence(comments: Sequence[Mapping[str, Any]], **expected: Any) -> BootstrapEvidence:
    if not isinstance(comments, Sequence) or isinstance(comments, (str, bytes)): raise PolicyError("bootstrap comments are invalid")
    markers = []
    for comment in comments:
        body = comment.get("body") if isinstance(comment, Mapping) else None
        if not isinstance(body, str) or MARKER_PREFIX not in body: continue
        match, user = MARKER_RE.fullmatch(body), comment.get("user") if isinstance(comment, Mapping) else None
        if match is None or not isinstance(user, Mapping) or user.get("login") != TRUSTED_AUTHOR: raise PolicyError("bootstrap marker is invalid")
        markers.append(match.group(1))
    if len(markers) != 1: raise PolicyError("bootstrap marker is missing or ambiguous")
    return parse_evidence(markers[0], **expected)
def validate_activation(comments: Sequence[Mapping[str, Any]], yellow_prestart: Any, changed_paths: Sequence[str], *, expected_issue: int, expected_base: str) -> BootstrapEvidence:
    evidence = extract_evidence(comments, expected_issue=expected_issue, expected_base=expected_base)
    paths = tuple(changed_paths) if isinstance(changed_paths, Sequence) and not isinstance(changed_paths, (str, bytes)) else ()
    if (not paths or len(paths) != len(set(paths)) or tuple(evidence.authorized_paths) != tuple(getattr(yellow_prestart, "authorized_paths", ())) or set(paths) - set(evidence.authorized_paths)): raise PolicyError("bootstrap paths do not exactly agree with YELLOW authorization")
    return evidence
