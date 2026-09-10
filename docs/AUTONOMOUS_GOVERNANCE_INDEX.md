# Autonomous governance index

<!-- D1-GOVERNANCE-METADATA
{"schema_version":1,"authority_scope":"domain-scoped","authority_precedence":"primary-then-subordinate","scientific_contract":"separate","non_authority_roles":["fixture-evidence","history","reference","roadmap","runbook"],"pre_commit_identity":"exact-authorized-d1-candidates","post_commit_identity":"exact-tracked-d1-identities","post_commit_candidate_allowance":"inactive","conflict_handling":"fail-closed-escalate"}
D1-GOVERNANCE-METADATA -->

## Purpose and scope

This descriptive index records the taxonomy used by the autonomous governance
documents. It does not activate a worker, route policy, calculate risk, or
change runtime behavior.

## Taxonomy and authority

Authority is domain-scoped. Within a domain, a current primary source takes
precedence over a current subordinate source. The Abaqus model contract is a
separate scientific contract. Reference material, runbooks, history, roadmaps,
and retained fixture evidence are not authority sources.

## Exact Git identity and D1 phases

Document identity is established only by exact, case-sensitive,
repository-relative POSIX paths reported by Git. Before commit, the three D1
documents are exact authorized non-ignored candidates. After commit, they are
exact tracked identities; the post-commit candidate allowance is inactive.
Aliases, ambiguous evidence, and mixed states are reconciled by failing closed.

## Conflict handling

When current authoritative sources conflict, stop and escalate for
reconciliation. Do not infer authority from reference, runbook, history,
roadmap, or fixture material.
