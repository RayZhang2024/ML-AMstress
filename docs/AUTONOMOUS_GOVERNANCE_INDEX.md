# Autonomous governance index

## Purpose and scope

This index describes the repository's governance documents. It is a reference
map, not a policy consumer or workflow control point.

## Taxonomy and authority

<!-- D1-GOVERNANCE-METADATA
{"schema_version":1,"authority_scope":"domain-scoped","authority_precedence":"primary-then-subordinate","scientific_contract":"separate","non_authority_roles":["fixture-evidence","history","reference","roadmap","runbook"],"pre_commit_identity":"exact-authorized-d1-candidates","post_commit_identity":"exact-tracked-d1-identities","post_commit_candidate_allowance":"inactive","conflict_handling":"fail-closed-escalate"}
D1-GOVERNANCE-METADATA -->

Authority is scoped by domain. Primary sources precede subordinate sources;
reference, history, roadmap, runbook, and fixture evidence do not supply
authority. Scientific-model contracts remain separate from autonomy governance.

## Exact Git identity and D1 phases

The manifest uses exact, canonical Git-reported repository-relative POSIX
identities. Before commit, only the authorized D1 candidates may be untracked;
after commit, those identities must be tracked.

## Conflict handling

Conflicts fail closed and require escalation to the applicable authority.
