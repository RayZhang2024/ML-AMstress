# Autonomous governance index

## Purpose and scope

This index is a descriptive, Git-backed inventory of the repository's
autonomous-governance documents.  It does not route work, calculate risk, or
enforce policy.  The accompanying manifest records classifications; the
existing governing documents remain the source of their own requirements.

## Taxonomy and authority

Authority is scoped by domain.  Primary authority takes precedence over
subordinate authority within the same domain.  Scientific-model contracts are
separate from autonomy governance.  Reference material, runbooks, roadmaps,
history, and retained fixture evidence are not policy authorities.

<!-- D1-GOVERNANCE-METADATA
{"authority_scope":"domain-scoped","authority_precedence":"primary-then-subordinate","scientific_contract":"separate","non_authority_roles":["fixture-evidence","history","reference","roadmap","runbook"],"pre_commit_identity":"exact-authorized-d1-candidates","post_commit_identity":"exact-tracked-d1-identities","post_commit_candidate_allowance":"inactive","conflict_handling":"fail-closed-escalate","schema_version":1}
D1-GOVERNANCE-METADATA -->

## Exact Git identity and D1 phases

The manifest validation uses exact, case-sensitive repository-relative POSIX
identities reported by Git.  Before these three D1 files are committed, only
their exact authorized candidates may supply the pre-commit transition; after
commit, those identities must be tracked.  Similar-looking names are rejected
rather than normalized into accepted identities.

## Conflict handling

Conflicting classifications or identity evidence fail closed and require
escalation.  This prose explains the structured metadata but is not parsed as
a policy language.
