# Autonomous document governance index

This index is a descriptive map of document authority.  It neither executes
policy nor changes worker, routing, review, repair, runtime, or merge behavior.
The adjacent manifest is the bounded inventory for this final repository head.

## Taxonomy and authority

`policy` and `orchestration-contract` documents describe governance;
`subsystem-contract` documents describe bounded A4--A7 control-plane parts.
Operational runbooks, developer guides, and architecture references explain or
support that work but are reference material, not policy input.  Roadmaps,
historical records, and retained fixture evidence preserve context or audit
evidence and have no authority.

Authority is domain-scoped.  The primary autonomy policy is
`AUTONOMOUS_DEVELOPMENT.md`; its subordinate sources are
`AUTONOMOUS_ORCHESTRATION.md` and the autonomous-work issue template.  Current
authoritative autonomy sources that conflict must be reconciled fail closed,
rather than selecting a convenient interpretation.  The primary scientific
contract, `ABAQUS_MODEL_CONTRACT.md`, governs only the `scientific-model`
domain.  It does not override autonomy policy, and autonomy policy does not
override the scientific contract within that scientific domain.

## Exact Git identity and final-head coverage

Paths in the manifest are exact, case-sensitive, repository-relative POSIX
identities reported by Git.  Filesystem existence, path resolution, and
case-insensitive lookup are not identity evidence.  The inventory covers every
top-level `docs/*.md` document plus `AGENTS.md`, `README.md`, and the autonomous
issue template.

Before a worker commits, identity validation may use only Git-reported,
non-ignored candidates for the three named D1 additions.  Existing documents
and protecting tests must already be exact tracked identities.  Once the
implementation is committed and validated as the final head, every D1 path
must be tracked and the candidate allowance is inactive.  Unexpected,
malformed, ambiguous, or conflicting identity evidence fails closed.
