"""Pure, bounded, fail-closed A8 merge-readiness policy evaluation.

The caller supplies an already-observed snapshot. This module deliberately
does not perform I/O, invoke tools, or alter GitHub state.
"""
from __future__ import annotations

import re
from typing import Any, Mapping

SHA_RE = re.compile(r"^[0-9a-f]{40}$")
RISKS = ("green", "yellow", "red")
MAX_ITEMS = 128
MAX_PATH_LENGTH = 240
REQUIRED_CONTRACT_SECTIONS = (
    "Goal", "Necessity Gate", "Required behavior", "Do not change",
    "Acceptance criteria", "Tests/validation", "Risk classification", "Dependencies",
)


def _mapping(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def _sha(value: Any) -> bool:
    return isinstance(value, str) and SHA_RE.fullmatch(value) is not None


def _items(value: Any) -> tuple[Any, ...] | None:
    if not isinstance(value, (list, tuple)) or len(value) > MAX_ITEMS:
        return None
    return tuple(value)


def _safe_path(path: Any) -> bool:
    return (isinstance(path, str) and 0 < len(path) <= MAX_PATH_LENGTH
            and "\\" not in path and not path.startswith("/") and not path.startswith("../")
            and "/../" not in path and "//" not in path and "\x00" not in path)


def _names(value: Any, *, allow_empty: bool) -> tuple[str, ...] | None:
    values = _items(value)
    if values is None or (not allow_empty and not values):
        return None
    if (any(not isinstance(name, str) or not name or len(name) > 120 for name in values)
            or len(set(values)) != len(values)):
        return None
    return values


def _head_evidence(records: Any, required: tuple[str, ...], head: str) -> set[str]:
    records = _items(records)
    if records is None:
        return set(required)
    observed, duplicates = {}, set()
    for raw in records:
        record = _mapping(raw)
        name = record.get("name")
        if not isinstance(name, str):
            continue
        if name in observed:
            duplicates.add(name)
        observed[name] = record
    return {name for name in required if name in duplicates or observed.get(name, {}).get("status") != "success"
            or observed.get(name, {}).get("head_sha") != head}


def _authorization(value: Any, head: str, scope: str) -> bool:
    item = _mapping(value)
    return item.get("scope") == scope and item.get("head_sha") == head and item.get("granted") is True


def evaluate(snapshot: Any) -> dict[str, Any]:
    """Return a deterministic data-only decision; malformed input fails closed."""
    reasons: set[str] = set()
    data = _mapping(snapshot)
    if data.get("schema_version") != 1:
        reasons.add("invalid_snapshot")
    issue, pr = _mapping(data.get("issue")), _mapping(data.get("pr"))
    contract, scope = _mapping(data.get("contract")), _mapping(data.get("scope"))
    validations, a5 = _mapping(data.get("validations")), _mapping(data.get("a5"))
    authorizations = _mapping(data.get("authorizations"))
    labels = _items(issue.get("labels"))
    statuses = [] if labels is None else [x for x in labels if isinstance(x, str) and x.startswith("status:")]
    risk_labels = [] if labels is None else [x for x in labels if isinstance(x, str) and x.startswith("risk:")]
    if len(statuses) != 1 or statuses[0] not in ("status:in-progress", "status:review"):
        reasons.add("invalid_lifecycle_status")
    if len(risk_labels) != 1 or risk_labels[0] not in tuple("risk:" + risk for risk in RISKS):
        reasons.add("invalid_risk_label")
    declared = risk_labels[0][5:] if len(risk_labels) == 1 and risk_labels[0].startswith("risk:") else None
    effective = scope.get("effective_risk")
    if effective not in RISKS or declared not in RISKS or RISKS.index(effective) > RISKS.index(declared):
        reasons.add("risk_elevation_or_unknown")
    if issue.get("dependencies_satisfied") is not True:
        reasons.add("unresolved_dependencies")
    if issue.get("duplicate_or_conflicting_work") is not False:
        reasons.add("duplicate_or_conflicting_work")
    if (contract.get("complete") is not True or tuple(contract.get("sections", ())) != REQUIRED_CONTRACT_SECTIONS
            or contract.get("issue_number") != issue.get("number") or contract.get("declared_risk") != declared
            or not isinstance(contract.get("dependencies"), (list, tuple))
            or not isinstance(contract.get("controlled_runtime_required"), bool)):
        reasons.add("malformed_contract")
    head = pr.get("head_sha") if _sha(pr.get("head_sha")) else None
    if not head:
        reasons.add("invalid_current_head")
    if not (pr.get("open") is True and pr.get("merged") is False and pr.get("draft") is False
            and pr.get("mergeable") is True and pr.get("same_repository") is True and pr.get("base") == "main"
            and isinstance(pr.get("number"), int) and pr.get("number") > 0):
        reasons.add("invalid_pr_identity")
    if (pr.get("issue_number") != issue.get("number") or pr.get("identity_current") is not True
            or pr.get("head_count") != 1):
        reasons.add("ambiguous_pr_identity")
    changed, allowed = _items(scope.get("changed_files")), _items(scope.get("authorized_paths"))
    if changed is None or allowed is None or not changed or not allowed or not all(_safe_path(x) for x in changed + allowed):
        reasons.add("incomplete_file_enumeration")
    elif len(set(changed)) != len(changed) or not set(changed).issubset(set(allowed)):
        reasons.add("unauthorized_changed_path")
    if scope.get("fully_enumerated") is not True:
        reasons.add("incomplete_file_enumeration")
    required_validations = _names(contract.get("required_repository_validations"), allow_empty=False)
    if required_validations is None:
        reasons.add("missing_required_validation")
    elif head and _head_evidence(validations.get("repository"), tuple(required_validations), head):
        reasons.add("missing_or_stale_repository_validation")
    if head and _head_evidence(validations.get("ci"), ("Normal Python CI", "git diff --check origin/main...HEAD"), head):
        reasons.add("missing_or_stale_normal_ci")
    if head and not (a5.get("state") == "review:clean" and a5.get("head_sha") == head
                     and a5.get("unresolved_findings") == 0 and a5.get("unresolved_threads") == 0
                     and a5.get("blocking_submissions") == 0):
        reasons.add("a5_not_clean_on_current_head")
    required_runtime = _names(contract.get("required_runtime_evidence"), allow_empty=True)
    if required_runtime is None:
        reasons.add("malformed_contract")
    elif head and _head_evidence(data.get("runtime_evidence"), tuple(required_runtime), head):
        reasons.add("missing_or_stale_runtime_evidence")
    if head and not _authorization(authorizations.get("implementation"), head, "implementation"):
        reasons.add("missing_implementation_authorization")
    if head and contract.get("controlled_runtime_required") is True and not _authorization(authorizations.get("controlled_runtime"), head, "controlled_runtime"):
        reasons.add("missing_runtime_authorization")
    if head and effective in ("green", "yellow") and not _authorization(authorizations.get("merge"), head, "gpt_managed_merge"):
        reasons.add("missing_gpt_merge_authorization")
    if head and effective == "red" and not _authorization(authorizations.get("merge"), head, "red_user_or_domain_owner_merge"):
        reasons.add("missing_red_merge_authorization")
    return {"merge_ready": not reasons, "effective_risk": effective if effective in RISKS else None,
            "head_sha": head, "reason_codes": tuple(sorted(reasons))}
