# Autonomous GitHub orchestration conventions

This document defines deterministic metadata for future issue selection and
review. It is subordinate to [AUTONOMOUS_DEVELOPMENT.md](AUTONOMOUS_DEVELOPMENT.md)
and does not activate Codex workers, autonomous production execution, or
auto-merge.

## Label vocabulary

Labels are exact, lower-case strings. An issue may have at most one label from
each mutually exclusive family.

### Status labels

- `status:ready` — the issue contract is complete, the declared risk is
  labeled, dependencies are satisfied, and no active implementation exists.
  This is the only status that makes an issue eligible to start.
- `status:in-progress` — one authorized implementation has claimed the issue
  and has a branch or PR. A second implementation must not start.
- `status:review` — the implementation is complete enough for review; the PR,
  checks, limitations, and any required runtime evidence are recorded.
- `status:blocked` — work cannot safely start or continue because a dependency,
  conflict, missing evidence, ambiguity, scope issue, or duplicate work must
  be resolved. A blocked issue is never eligible to start.

An issue with no status label, multiple status labels, or an unknown status
label is not eligible. Status labels describe orchestration state; they do not
replace GitHub's open/closed state.

### Risk labels

- `risk:green` — the GREEN class in the repository policy: bounded,
  reversible, behavior-preserving work such as documentation/tests, UI/layout,
  visualization infrastructure, logging, packaging/settings/path changes, or
  behavior-preserving cleanup.
- `risk:yellow` — the YELLOW class: data extraction/interpolation, Abaqus
  orchestration, readiness/validation logic, or performance changes that may
  alter workflow behavior.
- `risk:red` — the RED class: meshing or element type, BC physical semantics,
  thermal/heat-treatment physics, layer activation/removal physics, material
  definitions, residual-stress methodology, or scientific calibration
  assumptions.

Exactly one `risk:*` label is required for eligibility. The label is a
declaration, not permission to bypass the effective-risk escalation rules.

### Optional routing labels

- `agent:codex` — optional routing/request metadata for a future Codex worker.
- `agent:gpt-review` — optional routing/request metadata for a future GPT
  review pass.

Routing labels do not authorize work, change risk, satisfy dependencies, or
enable a worker. They may coexist with one status and one risk label, or be
omitted.

## Standard autonomous issue contract

An issue intended for autonomous orchestration must contain these sections in
this order. Human-written additional context may follow them, but must not
change their meaning:

1. `## Goal`
2. `## Necessity Gate`
3. `## Required behavior`
4. `## Do not change`
5. `## Acceptance criteria`
6. `## Tests/validation`
7. `## Risk classification`
8. `## Dependencies`

The repository template at
`.github/ISSUE_TEMPLATE/autonomous-work.md` provides this contract without
applying labels automatically.

The `Risk classification` section must name exactly one declared risk label.
The `Dependencies` section must use the syntax below, including `- none` when
there are no dependencies. Missing or ambiguous contract sections make the
issue ineligible and should result in `status:blocked`, not an inferred
interpretation.

## Machine-readable dependencies

Inside the `## Dependencies` section, write one dependency per line using the
exact lower-case key and issue reference:

```text
- blocked-by: #22
- blocked-by: RayZhang2024/ML-AMstress#123
```

The canonical grammar is:

```text
^\s*-\s+blocked-by:\s+(#[1-9][0-9]*|[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+#[1-9][0-9]*)\s*$
```

`#22` is shorthand for the current repository. Cross-repository references
must include `owner/repository#number`. Do not put prose on a dependency line;
put explanations on the following indented or ordinary paragraph lines. Use
`- none` exactly when the section has no dependencies. The parser must reject
unknown keys, malformed references, duplicate references, and unresolved issue
numbers rather than guessing.

A dependency is satisfied only when its referenced issue is closed or an
explicit maintainer record says the dependency is otherwise released. An open,
missing, ambiguous, or inaccessible dependency is unsatisfied and blocks work.

## Eligibility and duplicate-work prevention

Only an issue with `status:ready` may be started. Future task selection may
consider an issue executable only when all of these checks pass at the same
observation time:

1. The issue is open.
2. It has exactly one `status:ready` label and exactly one recognized
   `risk:*` label.
3. The autonomous issue contract parses successfully.
4. Every declared dependency is satisfied.
5. No active implementation already exists: there is no open PR linked to the
   issue, and no branch/PR claim or `status:in-progress` record for the same
   issue owned by another implementation.
6. The requested agent, if any, is authorized for the declared risk and the
   required runtime evidence is available or explicitly planned.

Before creating a branch, an implementation must re-check the issue labels,
dependencies, and open PRs. It should claim the issue by recording the branch
and PR and changing status to `status:in-progress`; if another claim appears
or the state cannot be determined, it must stop and set/report `status:blocked`
rather than racing or duplicating work. Closing an issue or merging a PR is not
an agent's implicit authority.

## State transitions and evidence

The intended lifecycle is:

```text
planned/unlabeled -> status:ready -> status:in-progress -> status:review -> closed
                              \-> status:blocked
status:blocked -> status:ready       (only after the blocker is resolved)
status:review -> status:in-progress (only when review requests implementation changes)
```

- A maintainer establishes `status:ready`, the risk label, the complete
  contract, and dependency readiness.
- A future authorized worker may claim `status:in-progress` only after the
  eligibility and duplicate-work checks pass, and must record branch/PR
  identity.
- A worker may request `status:review` only after the implementation diff,
  required checks, limitations, and runtime/scientific evidence are recorded.
- A maintainer or explicitly approved future automation may mark the issue
  closed after the PR is merged. No `status:completed` label is required.
- Any actor discovering a blocker may report it, but only an authorized
  maintainer or future policy-enforcing automation should resolve a blocked
  state or reclassify risk.

## Effective-risk and review routing

The effective risk is the highest risk implied by changed files, behavior,
generated artifacts, or required evidence, following the parent policy. If it
exceeds the issue's `risk:*` declaration, the PR is blocked: do not silently
edit labels to make it eligible, merge, or continue implementation. Record the
mismatch, move/report the issue as blocked, and request maintainer
reclassification or scope correction. `agent:gpt-review` may route review but
cannot approve a RED change; RED still needs explicit human/domain-owner
approval.

## Current activation boundary

These labels, transitions, and syntax are documentation conventions for future
orchestration. This repository currently has no Codex worker, issue poller,
label-management automation, autonomous production-code execution, or
auto-merge enabled by this document.
