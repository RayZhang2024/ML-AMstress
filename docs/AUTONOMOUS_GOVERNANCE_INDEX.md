# Autonomous documentation governance index

This index is a descriptive guide to the document taxonomy recorded in
`AUTONOMOUS_DOCUMENT_MANIFEST.json`.  It does not execute policy, route work,
calculate risk, or change eligibility, merge, runtime, or scientific behavior.

## Taxonomy and scope

The manifest classifies repository documents as policy, orchestration or
scientific contracts, operational runbooks, subsystem contracts, developer
guides, architecture references, roadmaps, historical records, or retained
fixture evidence.  Its status and authority fields make the scope explicit:
only current primary or subordinate sources in the relevant domain can be
inputs to current policy resolution.

`docs/AUTONOMOUS_DEVELOPMENT.md` is the current primary autonomy policy.
`docs/AUTONOMOUS_ORCHESTRATION.md` is its subordinate orchestration contract.
The worker runbook is an operational runbook, not a policy override;
troubleshooting is recovery/reference guidance; and the Codex prompt guide is
an execution/developer guide.  Subsystem contracts describe bounded A4--A7
control-plane responsibilities within that hierarchy.

`docs/ABAQUS_MODEL_CONTRACT.md` is the primary scientific/model contract in
the separate scientific-model domain.  It governs scientific/model concerns;
autonomy governance does not supersede it, and it does not override autonomy
policy outside that domain.

Roadmaps, historical records, and fixture evidence are context only.  They
never override current policy.  In particular, retained canary and repair
fixtures are evidence rather than authority, and issue-specific historical
notes do not become current policy inputs.

## Precedence and conflicts

Use the manifest path, status, domain, authority, and `policy_input` fields to
identify applicable current authority; do not infer authority from a directory
name or incidental prose.  A current primary source precedes subordinate
sources in the same domain.  Runbooks and reference material provide
instructions or context only within the limits of applicable policy.

A conflict between current authoritative sources is a preflight blocker that
requires reconciliation.  It is not permission to guess, choose by path, or
promote historical, roadmap, or fixture material into authority.
