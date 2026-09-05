# A5.1 read-only reviewer boundary

`scripts/a5_reviewer.py` accepts a prepared snapshot only; it makes no GitHub
requests and cannot mutate PR or issue state. `validate_snapshot()` freezes a
version-1 snapshot containing exact PR SHAs, linked issue evidence, risk floor,
bounded changed-file patches, CI status evidence, and trusted worker identity.

`review_snapshot()` sends the generated prompt to the configured Codex CLI via
stdin (`codex exec --sandbox read-only -c approval_policy="never"
--output-last-message <temporary-final-message> -`). Codex progress/session
output is never treated as the verdict: only the bounded temporary final-message
file is passed to strict verdict parsing, then its containing directory is
removed. Its child environment excludes `GITHUB_TOKEN`, `GH_TOKEN`,
`OPENAI_API_KEY`, and `AUTOMATION_APP_TOKEN`. It returns
only a strict version-1 `ReviewVerdict` or raises `ReviewError`; it does not
coerce malformed model output to `clean`.

If the reviewer process exits nonzero, the error reports only the stable
`reviewer-process` category, numeric exit code, and a bounded redacted tail.
It prefers stderr and uses stdout only when stderr is empty; reviewer prompts,
snapshots, diffs, credentials, and local paths are excluded.

The parser requires `clean`, `blocker`, or `escalate`, binds the verdict to the
snapshot head SHA, enforces the trusted risk floor, and makes RED risk escalate.
Findings have stable `F-N` IDs plus category, message, required action, and
required evidence for a later, separately authorized repair flow.
