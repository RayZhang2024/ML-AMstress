# A5.2 deterministic PR review-state contract

A5.2 is a pure-Python interpretation layer between an already validated A5.1 verdict and a future trusted orchestrator. It does not invoke a reviewer, query GitHub, alter labels, repair code, push, create a PR, or merge. Applying a returned plan remains the trusted caller's responsibility.

## Vocabulary and composition

PR review state is exactly one of `review:pending`, `review:blocker`, `review:clean`, or `review:escalated`. It is separate from the existing issue implementation-state family.

| Issue state | PR review state | Meaning |
| --- | --- | --- |
| `status:review` | `review:pending` | The exact current head awaits review. |
| `status:review` | `review:clean` | The exact current head is clean but unmerged. |
| `status:in-progress` | `review:blocker` | Accepted findings authorize later same-branch repair. |
| `status:blocked` | `review:escalated` | Escalation needs explicit trusted human release. |

All other initialized combinations fail closed. Uninitialized review state is allowed only with `status:review` during initialization.

## Immutable input and decisions

`ReviewStateInput` is immutable and versioned (`schema_version: 1`). It contains repository, PR/issue numbers, current head SHA, exactly one issue status, exactly one review state (or uninitialized), the evidence head, an optional already-validated A5.1 verdict, and an event kind. Verdict input is bounded to verdict, reviewed head, effective risk, and stable finding IDs.

`transition` returns a versioned plan with next issue/review states, evidence head, explicit no-op flag, and deterministic decision key. The key is SHA-256 over contract version, repository, PR number, and current head SHA; it never includes timestamps or model prose.

## Transition table

| Current state and event | Result |
| --- | --- |
| uninitialized + `status:review`, initialize | pending on current head; review |
| pending + exact-head clean verdict | clean; review |
| pending + exact-head blocker verdict | blocker; in-progress |
| pending + exact-head escalate verdict | escalated; blocked |
| clean + new head | pending on new head; review |
| blocker/in-progress + same-branch new head | pending on new head; review |
| escalated/blocked + ordinary new head | remain escalated/blocked, retaining old evidence head |
| escalated/blocked + trusted human release | pending on then-current head; review |

## Exact-head, idempotency, and audit rules

Verdicts must name the current head. Stale verdicts and evidence attached to a different head cannot mutate state, and terminal decisions cannot be silently transplanted to another head. Replaying the same accepted terminal verdict on the same head is an explicit idempotent no-op; a conflicting verdict, unknown or multiple states, and impossible combinations fail closed. Ordinary pushes cannot clear escalation.

`serialize_audit` emits deterministic bounded JSON with identity, heads, event, verdict/risk/finding IDs, old/new states, no-op flag, and decision key. It excludes prompts, model streams, credentials, review summaries, and raw issue or diff content.
