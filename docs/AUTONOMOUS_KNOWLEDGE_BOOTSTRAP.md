# D0 knowledge bootstrap

D0 is an exceptional, dormant-by-default, protected-YELLOW governance escape hatch for reconstructing descriptive repository knowledge from a trusted base. It cannot authorize scientific work, runtime execution, repair, merge, or a clean review verdict.

Activation requires exactly one owner-authored standalone issue comment: `<!-- knowledge-bootstrap-prestart:{canonical-json} -->`. Schema version 1 has exactly `schema_version`, `repository`, `issue_number`, `trusted_base_sha`, `authorized_paths`, `purpose`, `a5_mode`, and `scientific_runtime_prohibited`.

Authorization is fail-closed: canonical owner evidence, a current automated-YELLOW PR, exact current head/base and changed paths, matching canonical YELLOW pre-start evidence, and successful exact-head Normal Python CI are all required. A replay emits at most one bounded exact-head audit. Invalid evidence leaves ordinary A5 unchanged.

Future eligible identities are only root `README.md`, root `AGENTS.md`, and non-protected descriptive `docs/**` Markdown or JSON. Workflows, scripts, tests, generated/runtime artifacts, and Abaqus/scientific/model/GUI/ML and A4 through A7 control-plane identities are excluded.
