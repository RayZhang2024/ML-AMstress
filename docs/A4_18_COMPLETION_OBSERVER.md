# A4.18 GREEN worker completion observer

The GitHub-native observer in
`.github/workflows/codex-green-completion-observer.yml` receives only completed
`workflow_run` events for the exact `GREEN Codex issue worker` workflow. It
checks out trusted `main` with persisted credentials disabled and runs
`python -m scripts.codex_completion_observer`.

The observer resolves an issue only when exactly one trusted
`codex-worker-claim` marker binds the completed GitHub run ID and deterministic
branch to that issue. It then creates one bounded, machine-readable issue
comment containing the repository, run ID, terminal conclusion, GitHub run
timestamps, branch, head SHA, and an optional exact PR identity. The deterministic idempotency key prevents a
replayed delivery from adding another observation. Zero or multiple claim/PR
matches, stale identity, malformed trusted audit, or API ambiguity stop without
mutating an arbitrary issue.

It never runs Codex, retries work, changes issue or review labels, performs
repair, merges, or enables auto-merge. Successful worker completion therefore
preserves the worker's existing `status:review`; terminal non-success preserves
its existing fail-closed state. The workflow has only `actions: read`,
`contents: read`, `issues: write`, and `pull-requests: read` permissions.

GitHub Actions can observe workflow completion immediately through GitHub's
native event delivery. That does not make ChatGPT receive arbitrary GitHub
webhook pushes into an existing chat session; a maintainer still consults the
GitHub issue/PR audit trail or an explicitly configured notification channel.
