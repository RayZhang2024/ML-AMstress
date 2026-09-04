# A5.4a protected PR review and repair loop

`.github/workflows/a5-review-loop.yml` starts only after a completed
`Normal Python CI` `workflow_run`. It checks out trusted `main` with persisted
credentials disabled, then runs `scripts/a5_review_orchestrator.py` on the
designated self-hosted Windows runner.

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
