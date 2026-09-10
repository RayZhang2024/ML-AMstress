# Autonomous governance index

## Purpose and scope

This is a descriptive, non-enforcing index of the repository's autonomous
governance documents.  It neither starts work nor changes worker, review,
runtime, scientific, or merge behaviour.  The companion manifest records a
bounded classification for each indexed document.

## Authority and roles

Authority is domain-scoped.  Within a domain, current primary authority takes
precedence and a current subordinate contract refines that policy only within
the delegated domain.  The scientific-model contract is separately authoritative
for scientific-model decisions and is not superseded by autonomy documents.
Reference material and operational runbooks explain or support current policy
without being policy input.  Roadmaps describe planned direction; history
records preserve past context; retained fixture evidence preserves canary and
repair evidence.  Those non-authority roles do not create governing rules.

## Exact identity and D1 phases

Repository identity is established only by exact, case-sensitive,
repository-relative POSIX path strings reported by Git.  Filesystem resolution,
case-insensitive lookup, path normalization, and resolved-path equivalence do
not establish identity.

Before commit, Git may report only the exact three authorized D1 candidates:
`docs/AUTONOMOUS_DOCUMENT_MANIFEST.json`,
`docs/AUTONOMOUS_GOVERNANCE_INDEX.md`, and
`tests/test_autonomous_document_manifest.py`.  This candidate allowance is
complete and exclusive.

After commit, each of those exact paths must be a tracked Git identity and no
D1 candidate allowance is active.  The two states are exhaustive: partial,
mixed, aliased, ignored, or extra candidate evidence is not a valid phase.

## Reconciliation

When current authoritative sources conflict within the same domain, do not
silently select an ordering.  Stop, identify the conflict, and escalate it to
the responsible maintainer for an explicit reconciliation.  Cross-domain
questions likewise require the authority appropriate to the affected domain,
including the scientific-model contract for scientific-model decisions.
