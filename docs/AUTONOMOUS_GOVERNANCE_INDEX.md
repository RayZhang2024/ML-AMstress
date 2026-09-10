# Autonomous governance document index

`AUTONOMOUS_DOCUMENT_MANIFEST.json` is a descriptive, version-1 inventory of the repository's document authority. It does not execute policy, route work, calculate risk, or change worker, review, merge, runtime, or model behavior.

## Taxonomy and roles

The manifest uses the stable kinds `policy`, `orchestration-contract`, `scientific-contract`, `operational-runbook`, `subsystem-contract`, `developer-guide`, `architecture-reference`, `roadmap`, `historical-record`, and `fixture-evidence`. A document's domain limits the scope in which its authority can be used. Within that domain, a current primary source governs; a current subordinate source supplies a more specific contract consistently with its primary source. Reference material explains or assists but does not set policy.

`AUTONOMOUS_DEVELOPMENT.md` is the current primary autonomy policy. `AUTONOMOUS_ORCHESTRATION.md` and the autonomous-work issue template are current subordinate autonomy orchestration contracts. The current A4--A7 documents are subordinate autonomy subsystem contracts. The worker runbook, troubleshooting guide, and Codex prompt guide are current reference/runbook material. `ABAQUS_MODEL_CONTRACT.md` is the separate, current primary scientific-model contract; autonomy documents do not override scientific model safety.

Roadmaps describe direction, historical records preserve history, and retained fixture evidence preserves test or audit evidence. Those three roles have no authority to establish current policy. The index itself is a current non-policy reference, not an additional source of precedence.

## Exact identity and phases

Repository identity is proved only by exact case-sensitive, Git-reported, repository-relative POSIX path strings. Filesystem lookup, path resolution, normalization, and case-insensitive equivalence cannot prove identity.

Before commit, validation separately obtains the exact tracked inventory and the exact Git-reported non-ignored untracked inventory. It permits only the three exact authorized D1 candidate additions: the manifest, this index, and `tests/test_autonomous_document_manifest.py`. No partial, empty, extra, or aliased candidate inventory is permitted.

After commit, or once committed, all three D1 paths must be ordinary exact tracked identities and no candidate allowance is active. This two-phase rule also applies to protecting tests: they identify exact intended files beneath `tests/`, with only the new D1 test permitted as a candidate before commit. The equivalent structured labels `pre-commit` and `post-commit`, and the phrases before the worker commits and once committed, keep these same phase assignments; reversing them fails closed.

## Reconciliation

Domain-scoped precedence does not silently choose between conflicting current authoritative sources. If current primary or subordinate guidance conflicts within its defined domain, it requires explicit reconciliation or escalation; arbitrary ordering outside the defined domain rules is prohibited.
