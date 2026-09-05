# A5.4a protected PR review and repair loop

`.github/workflows/a5-review-loop.yml` starts only after a completed
`Normal Python CI` `workflow_run`. It checks out trusted `main` with persisted
credentials disabled, then runs the coordinator as
`python -m scripts.a5_review_orchestrator` on the designated self-hosted
Windows runner.

The workflow keeps the built-in `GITHUB_TOKEN` for A5.2 labels/status/audit
state. It mints a repository-scoped, short-lived App installation token with
`actions/create-github-app-token@v3` from `AUTOMATION_APP_CLIENT_ID` and
`AUTOMATION_APP_PRIVATE_KEY`, then passes it as `AUTOMATION_APP_TOKEN` only to
trusted A5.3 same-branch push code. The token is injected only through the
origin-scoped temporary Git extraheader and is removed from A5.1 reviewer,
A5.3 Codex, and validation child environments. That repair push must emit the
next `pull_request:synchronize` event for `Normal Python CI`; no merge or
auto-merge operation is introduced.

The orchestrator resolves exactly one open, in-repository `codex/issue-*` PR
for the CI head, verifies its canonical `Closes #N` link and eligible open
GREEN `agent:codex` issue, and fails closed on stale, forked, ambiguous, or
non-GREEN evidence. It constructs the A5.1 snapshot from trusted API evidence;
A5.1 remains read-only and receives no GitHub credential.

The trusted A5 REST boundary uses a fixed application User-Agent. Every API
call has a stable operation name; an HTTP failure records only that operation
and its numeric status, while transport and malformed-response failures record
only the operation and their fixed category. These diagnostics never include
request URLs, payloads, response bodies, headers, tokens, prompts, diffs, or
local paths.

A5.2 is the sole source of `review:pending`, `review:blocker`,
`review:clean`, and `review:escalated` transitions. Bounded machine-readable
PR comments record exact-head CI observations, state decision keys, and repair
attempts. Before any successful-head state mutation, the trusted coordinator
idempotently verifies or creates the four repository review labels and fails
closed if their evidence or creation is ambiguous. A repair uses the exact
accepted pending-to-blocker A5.2 decision key, can run only for an accepted
GREEN blocker, at most twice,
and only within the PR's already-reviewed GREEN changed-file set. A successful
A5.3 repair must prove its pushed head before A5.2 returns the PR to pending;
the next exact-head CI completion performs any further review.

The loop contains no merge or auto-merge behavior. Ordinary GREEN workers are
also denied all live A5 control-plane scripts and `.github/**` paths.

Before A5.3 is authorized, trusted A5.1 classifies issue acceptance criteria as
repository-editable or external/post-run. A criterion is external only when it
requires observing a live control-plane result or state, such as hosted CI
passing on the exact PR head, worker completion, labels/status observed on
GitHub, audit/idempotency records, PR state, or GitHub-side SHA evidence.
Bare implementation nouns such as workflow, CI, and run remain
repository-editable.
Pending external evidence is presented as pending/unverified and cannot create
a repair finding. Contradictory trusted external evidence fails closed; it is
never copied or invented in a repository repair. Every A5.3 finding must carry
an exact repository acceptance-criterion reference, which trusted code verifies
before any state transition, attempt marker, checkout, or Codex invocation.
