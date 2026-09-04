"""Pure, fail-closed A5.2 PR review-state transition contract."""
from __future__ import annotations

import dataclasses
import hashlib
import json
import re
from typing import Any

STATE_CONTRACT_VERSION = 1
ISSUE_STATUSES = frozenset(("status:review", "status:in-progress", "status:blocked"))
REVIEW_STATES = frozenset(("review:pending", "review:blocker", "review:clean", "review:escalated"))
VERDICTS = frozenset(("clean", "blocker", "escalate"))
EVENTS = frozenset(("initialize", "verdict", "new_head", "human_release"))
RISKS = frozenset(("green", "yellow", "red"))
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
FINDING_ID_RE = re.compile(r"^F-[1-9][0-9]*$")
MAX_FINDING_ID_LENGTH = 32


class ReviewStateError(Exception):
    """Raised when state is unsafe, inconsistent, or cannot transition."""


@dataclasses.dataclass(frozen=True)
class ValidatedVerdict:
    verdict: str
    reviewed_head_sha: str
    effective_risk: str
    finding_ids: tuple[str, ...] = ()


@dataclasses.dataclass(frozen=True)
class ReviewStateInput:
    schema_version: int
    repository: str
    pull_request_number: int
    issue_number: int
    current_head_sha: str
    current_issue_status: str
    current_pr_review_state: str | None
    review_state_head_sha: str | None
    validated_verdict: ValidatedVerdict | None
    event_kind: str


@dataclasses.dataclass(frozen=True)
class TransitionPlan:
    schema_version: int
    next_issue_status: str
    next_pr_review_state: str
    review_state_head_sha: str
    idempotent_no_op: bool
    decision_key: str


def _positive_int(value: Any, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise ReviewStateError("%s must be a positive integer" % name)


def _sha(value: Any, name: str, optional: bool = False) -> None:
    if optional and value is None:
        return
    if not isinstance(value, str) or not SHA_RE.fullmatch(value):
        raise ReviewStateError("%s must be a lowercase 40-character SHA" % name)


def _single_state(value: Any, allowed: frozenset[str], name: str, optional: bool = False) -> None:
    if optional and value is None:
        return
    if not isinstance(value, str) or value not in allowed:
        raise ReviewStateError("%s must be one recognized mutually exclusive state" % name)


def validate_state_input(state: ReviewStateInput) -> ReviewStateInput:
    """Validate immutable input before deriving any transition."""
    if not isinstance(state, ReviewStateInput):
        raise ReviewStateError("state must be a ReviewStateInput")
    if state.schema_version != STATE_CONTRACT_VERSION:
        raise ReviewStateError("unsupported state contract version")
    if not isinstance(state.repository, str) or not state.repository or len(state.repository) > 200 or "/" not in state.repository:
        raise ReviewStateError("repository must be a bounded owner/repository identifier")
    _positive_int(state.pull_request_number, "pull_request_number")
    _positive_int(state.issue_number, "issue_number")
    _sha(state.current_head_sha, "current_head_sha")
    _single_state(state.current_issue_status, ISSUE_STATUSES, "current_issue_status")
    _single_state(state.current_pr_review_state, REVIEW_STATES, "current_pr_review_state", True)
    _sha(state.review_state_head_sha, "review_state_head_sha", True)
    if (state.current_pr_review_state is None) != (state.review_state_head_sha is None):
        raise ReviewStateError("review state and review-state head must both be set or both be uninitialized")
    if state.event_kind not in EVENTS:
        raise ReviewStateError("event_kind is unrecognized")
    verdict = state.validated_verdict
    if verdict is not None:
        if not isinstance(verdict, ValidatedVerdict):
            raise ReviewStateError("validated_verdict must be a ValidatedVerdict")
        if verdict.verdict not in VERDICTS or verdict.effective_risk not in RISKS:
            raise ReviewStateError("validated verdict contains an unrecognized value")
        _sha(verdict.reviewed_head_sha, "validated verdict reviewed_head_sha")
        if not isinstance(verdict.finding_ids, tuple) or len(verdict.finding_ids) > 50:
            raise ReviewStateError("finding_ids must be a bounded tuple")
        if len(set(verdict.finding_ids)) != len(verdict.finding_ids) or any(
                not isinstance(x, str) or len(x) > MAX_FINDING_ID_LENGTH
                or not FINDING_ID_RE.fullmatch(x) for x in verdict.finding_ids):
            raise ReviewStateError("finding_ids must be unique stable identifiers")
    if state.event_kind == "verdict" and verdict is None:
        raise ReviewStateError("verdict event requires a validated verdict")
    if state.event_kind != "verdict" and verdict is not None:
        raise ReviewStateError("only a verdict event may contain a validated verdict")
    return state


def decision_key(state: ReviewStateInput) -> str:
    """Return a stable identity for one exact trusted transition decision."""
    validate_state_input(state)
    verdict = state.validated_verdict
    identity = {
        "schema_version": STATE_CONTRACT_VERSION,
        "repository": state.repository,
        "pull_request_number": state.pull_request_number,
        "issue_number": state.issue_number,
        "current_head_sha": state.current_head_sha,
        "current_issue_status": state.current_issue_status,
        "current_pr_review_state": state.current_pr_review_state,
        "review_state_head_sha": state.review_state_head_sha,
        "event_kind": state.event_kind,
        "verdict": verdict.verdict if verdict else None,
        "reviewed_head_sha": verdict.reviewed_head_sha if verdict else None,
        "effective_risk": verdict.effective_risk if verdict else None,
        "finding_ids": list(verdict.finding_ids) if verdict else [],
    }
    encoded = json.dumps(identity, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return "a5.2:" + hashlib.sha256(encoded).hexdigest()


def _plan(state: ReviewStateInput, issue: str, review: str, head: str, no_op: bool) -> TransitionPlan:
    return TransitionPlan(STATE_CONTRACT_VERSION, issue, review, head, no_op, decision_key(state))


def _pending_current(state: ReviewStateInput) -> None:
    if state.current_issue_status != "status:review" or state.current_pr_review_state != "review:pending" or state.review_state_head_sha != state.current_head_sha:
        raise ReviewStateError("a verdict is allowed only from pending evidence for the current review head")


def transition(state: ReviewStateInput) -> TransitionPlan:
    """Derive one deterministic plan or raise a fail-closed error."""
    state = validate_state_input(state)
    review, evidence_head = state.current_pr_review_state, state.review_state_head_sha
    if state.event_kind == "initialize":
        if review is not None or state.current_issue_status != "status:review":
            raise ReviewStateError("only uninitialized status:review may initialize review state")
        return _plan(state, "status:review", "review:pending", state.current_head_sha, False)
    if state.event_kind == "verdict":
        verdict = state.validated_verdict
        if verdict.reviewed_head_sha != state.current_head_sha:
            raise ReviewStateError("stale verdict head must not mutate review state")
        terminal = {"review:clean": "clean", "review:blocker": "blocker", "review:escalated": "escalate"}
        if review in terminal:
            expected = {"review:clean": "status:review", "review:blocker": "status:in-progress", "review:escalated": "status:blocked"}[review]
            if evidence_head != state.current_head_sha or state.current_issue_status != expected:
                raise ReviewStateError("terminal review state has an impossible issue/head combination")
            if terminal[review] == verdict.verdict:
                return _plan(state, expected, review, evidence_head, True)
            raise ReviewStateError("conflicting verdict for an accepted terminal review state")
        _pending_current(state)
        issue, target = {"clean": ("status:review", "review:clean"), "blocker": ("status:in-progress", "review:blocker"), "escalate": ("status:blocked", "review:escalated")}[verdict.verdict]
        return _plan(state, issue, target, state.current_head_sha, False)
    if state.event_kind == "new_head":
        if review == "review:escalated":
            if state.current_issue_status != "status:blocked":
                raise ReviewStateError("escalated review must remain status:blocked")
            return _plan(state, "status:blocked", "review:escalated", evidence_head, True)
        if review in ("review:clean", "review:blocker") and evidence_head == state.current_head_sha:
            raise ReviewStateError("new_head event requires a head different from terminal evidence")
        if ((review == "review:clean" and state.current_issue_status == "status:review") or
                (review == "review:blocker" and state.current_issue_status == "status:in-progress") or
                (review == "review:pending" and state.current_issue_status == "status:review")):
            return _plan(state, "status:review", "review:pending", state.current_head_sha,
                         review == "review:pending" and evidence_head == state.current_head_sha)
        raise ReviewStateError("new head is not authorized from this issue/review combination")
    if review != "review:escalated" or state.current_issue_status != "status:blocked":
        raise ReviewStateError("human release is allowed only from escalated status:blocked")
    return _plan(state, "status:review", "review:pending", state.current_head_sha, False)


def serialize_audit(state: ReviewStateInput, plan: TransitionPlan) -> str:
    """Return bounded JSON excluding prompts, model prose, credentials and diff content."""
    state = validate_state_input(state)
    if not isinstance(plan, TransitionPlan) or plan.schema_version != STATE_CONTRACT_VERSION or plan.decision_key != decision_key(state):
        raise ReviewStateError("plan does not belong to this supported state input")
    verdict = state.validated_verdict
    payload = {"schema_version": STATE_CONTRACT_VERSION, "repository": state.repository,
               "pull_request_number": state.pull_request_number, "issue_number": state.issue_number,
               "current_head_sha": state.current_head_sha, "event_kind": state.event_kind,
               "verdict": verdict.verdict if verdict else None, "effective_risk": verdict.effective_risk if verdict else None,
               "finding_ids": list(verdict.finding_ids) if verdict else [], "old_issue_status": state.current_issue_status,
               "old_pr_review_state": state.current_pr_review_state, "old_review_state_head_sha": state.review_state_head_sha,
               "new_issue_status": plan.next_issue_status, "new_pr_review_state": plan.next_pr_review_state,
               "new_review_state_head_sha": plan.review_state_head_sha, "idempotent_no_op": plan.idempotent_no_op,
               "decision_key": plan.decision_key}
    return json.dumps(payload, sort_keys=True, separators=(",", ":"))
