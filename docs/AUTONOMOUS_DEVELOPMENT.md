# Autonomous development policy

This policy defines how GPT, Codex, GitHub, and related agents may work in the
ML-AMstress repository. It is governance and documentation only. It does not
enable autonomous production-code execution, GitHub Actions, automatic merges,
or any other automation.

Deterministic issue labels, dependency syntax, eligibility checks, and state
transitions are defined in [AUTONOMOUS_ORCHESTRATION.md](AUTONOMOUS_ORCHESTRATION.md).

## Work-unit rules

- One issue per branch and pull request. Do not mix unrelated fixes or
  speculative refactors into the work unit.
- Only issues explicitly marked ready by the repository maintainers may be
  started. Draft, blocked, superseded, or otherwise unready work requires
  maintainer direction first.
- Perform the Necessity Gate before any production change: identify the
  observed problem or evidence, the files that must change, why those files
  are necessary, and the intended behavior. Documentation-only work must not
  be used as a pretext for runtime changes.
- Keep implementation and review within the declared issue scope. A newly
  discovered adjacent defect becomes a separate issue unless a maintainer
  explicitly expands the current scope.
- Do not run duplicate concurrent implementations of the same issue. Check
  active branches, worktrees, and pull requests before starting; stop if
  another implementation is already active.
- If required validation cannot be run, report that limitation explicitly and
  distinguish planned/manual validation from checks actually performed. Never
  assume unrun Abaqus, solver, GUI, or scientific evidence.

### Trusted-worker implementation boundary

The readiness, dependency, duplicate-work, and branch/PR checks above apply to
an actor that owns orchestration. The trusted GREEN worker in
`scripts/codex_issue_worker.py` is such an actor: it performs those checks,
re-checks races and dependencies, and makes the deterministic branch claim
before it invokes sandboxed Codex.

After that worker has completed those checks for its immutable issue snapshot
and claimed branch, the sandboxed Codex implementation process must treat the
resulting control-plane facts as authoritative. It must not require GitHub API
access or repeat issue labels/status/risk, dependency, duplicate/open-PR,
branch-claim, or race checks. This exception does not apply to manual agents
or any other actor that has not received a completed trusted-worker claim.

The trusted worker owns branch creation and claim, issue labels/status,
authoritative normal-Python validation, commit/push, PR creation, and the
merge/no-merge policy. Those worker-owned steps are context, not prerequisites
for sandboxed Codex. Sandboxed Codex still owns the local repository Necessity
Gate: determine whether the requested change is already satisfied, identify
the minimal scoped edits, respect Do-not-change constraints, and stop on
effective-risk escalation or scientific ambiguity. It may run focused local
checks when available and must report checks it could not run truthfully;
unavailable optional Python or other tooling alone does not block an otherwise
clear GREEN edit because the trusted worker performs final validation. The
sandboxed process must continue without `GITHUB_TOKEN`, `GH_TOKEN`, or
`OPENAI_API_KEY`.

## Repository risk model

Classify a work unit by the highest-risk behavior it changes, not by the
amount of code or the issue label. A documentation-only change such as this
one is GREEN.

### GREEN — bounded, reversible, behavior-preserving work

Examples include:

- documentation and tests;
- UI/layout changes;
- visualization infrastructure;
- logging and error messages;
- packaging, settings, and path improvements;
- behavior-preserving cleanup.

GREEN work still requires focused checks, review, and truthful reporting of
anything not verified.

### YELLOW — workflow or data behavior requiring stronger evidence

Examples include:

- data extraction or interpolation;
- Abaqus orchestration;
- readiness or validation logic;
- performance changes that can alter workflow behavior.

YELLOW work requires targeted tests plus the relevant runtime or fixture
validation before it can be considered complete.

### RED — scientific or physical behavior

Examples include:

- meshing strategy or element type;
- boundary-condition physical semantics;
- thermal or heat-treatment physics;
- layer activation or removal physics;
- material definitions;
- residual-stress methodology;
- scientific calibration assumptions.

RED work requires explicit human/domain-owner approval and appropriate
Abaqus/solver or scientific validation. It is never eligible for an
automated merge.

## Effective-risk escalation

The effective risk of a pull request is the highest class implied by any
changed file, code path, generated artifact, or resulting behavior. A PR is
blocked when its effective risk is higher than the issue's declared class.
The agent must stop, explain the mismatch in the PR, and request issue
reclassification, scope correction, or explicit human direction before
continuing. Tests or a low-risk wrapper do not make a higher-risk behavior
GREEN. Future automation may enforce this rule by comparing issue metadata,
changed paths, and reviewed behavior; this document does not activate that
automation.

## Merge authority

- GREEN changes are eventually eligible for automated merge only after all
  required CI checks pass and review is clean.
- YELLOW changes are eventually eligible only after the stronger required
  runtime validation for their workflow has passed and review is clean.
- RED changes are never auto-merged. An explicit human/domain-owner approval
  is required in addition to the required checks and scientific validation.

No merge is implied by an agent completing a branch or opening a PR. The
repository's current workflow remains review-first and does not enable merge
automation through this policy.

## Credential and safety boundaries

- Use least-privilege GitHub permissions and credentials for the requested
  operation. Read-only access is preferred; write, issue, branch, and merge
  permissions must not be broader than necessary.
- Never place unrestricted secrets, tokens, private keys, or credentials in
  prompts, issue text, commits, PR descriptions, logs, test fixtures, or tool
  output. Redact accidental exposure and report it through the appropriate
  secure channel.
- Destructive operations (delete, reset, overwrite, force-push, merge, or
  broad cleanup) must be narrowly targeted, clearly authorized, and checked
  against the exact path or ref before execution. Preserve recoverable state
  whenever practical.
- Keep agent actions auditable in GitHub: use issue/PR discussion and commits
  that identify scope, evidence, checks, limitations, and approvals. Do not
  conceal failed checks, skipped validation, or external side effects.

## Mandatory stop and escalation conditions

An agent must stop implementation and report the blocker when any of the
following occurs:

- issue requirements conflict or cannot be reconciled;
- scientific intent is ambiguous;
- required runtime evidence cannot be produced for a high-risk change;
- the proposed implementation would cross the issue's declared scope;
- repository state indicates another implementation is already active.

The report should identify the evidence, preserve local work, and state the
specific maintainer or domain-owner decision needed to proceed. An agent must
not resolve these conditions by silently guessing, broadening permissions, or
merging.

## Required completion record

Every autonomous work unit should leave a concise record of the issue,
branch/PR, effective risk, files changed, checks actually run, manual/runtime
validation still required, review status, and any escalation. This record is
part of the audit trail; it does not replace human approval where this policy
requires it.
