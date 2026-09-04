# A5.1 read-only reviewer boundary

`scripts/a5_reviewer.py` accepts a prepared snapshot only; it makes no GitHub
requests and cannot mutate PR or issue state. `validate_snapshot()` freezes a
version-1 snapshot containing exact PR SHAs, linked issue evidence, risk floor,
bounded changed-file patches, CI status evidence, and trusted worker identity.

`review_snapshot()` sends the generated prompt to the configured Codex CLI via
stdin (`codex exec --sandbox read-only -c approval_policy="never" -`). Its child
environment excludes `GITHUB_TOKEN`, `GH_TOKEN`, and `OPENAI_API_KEY`. It returns
only a strict version-1 `ReviewVerdict` or raises `ReviewError`; it does not
coerce malformed model output to `clean`.

The parser requires `clean`, `blocker`, or `escalate`, binds the verdict to the
snapshot head SHA, enforces the trusted risk floor, and makes RED risk escalate.
Findings have stable `F-N` IDs plus category, message, required action, and
required evidence for a later, separately authorized repair flow.
