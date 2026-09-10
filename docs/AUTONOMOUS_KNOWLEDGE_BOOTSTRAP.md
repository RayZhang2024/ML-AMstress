# D0 knowledge bootstrap

D0 is an exceptional, dormant-by-default, protected-YELLOW mechanism for a
one-time repository-knowledge reconstruction. It is descriptive and
non-enforcing: it never authorizes scientific/runtime activity, implementation,
merge, or a clean A5 verdict. The ordinary protected-YELLOW process remains
the authority for those decisions.

Activation requires exactly one owner-authored standalone issue-comment body:
`<!-- knowledge-bootstrap-prestart:{canonical-json} -->`. Schema version `1`
contains exactly `schema_version`, `repository`, `issue_number`,
`trusted_base_sha`, `authorized_paths`, `purpose`, `a5_mode`, and
`scientific_runtime_prohibited`. It is fail-closed, bound to the exact issue,
base, and canonical YELLOW authorization, and permits only `README.md`,
`AGENTS.md`, or eligible descriptive `docs/**/*.md`/`.json` paths.

Without the exact marker prefix, A5's D0 probe returns not-applicable before
it reads PR, branch, head, base, paths, authorization, CI, audit, or session
state. A valid skip follows successful exact-head Normal Python CI and records
one idempotent exact-head skip audit; it invokes neither review nor repair.
