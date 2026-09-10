# Autonomous governance document index

`AUTONOMOUS_DOCUMENT_MANIFEST.json` is a descriptive inventory of the
repository's documented authorities. It does not dispatch work, calculate
risk, route workers, or alter workflow or merge behavior.

## Precedence

Authority is domain-scoped. A current primary document is the principal source
within its domain; a current subordinate document may refine it without
overriding it. Reference material supplies context only. Historical records
and retained fixture evidence have no policy authority.

The autonomy domain is led by `AUTONOMOUS_DEVELOPMENT.md`; orchestration
contracts refine that policy. The scientific-model domain is led by
`ABAQUS_MODEL_CONTRACT.md`.

## Repository identity phases

Before commit, the trusted worker validates separately acquired exact
Git-reported tracked and non-ignored untracked inventories. Only the three
authorized D1 paths may be permitted as untracked candidate identities in
that pre-commit inventory.

After commit, the final head requires each of those paths to be an exact
tracked identity, and candidate allowance is inactive.

Git-reported repository-relative POSIX spelling is the identity proof. Path
resolution, case-insensitive lookup, and normalization are not substitutes.
The same fail-closed treatment applies to malformed `-z` inventory evidence
and case-folding or Unicode-normalization aliases.
