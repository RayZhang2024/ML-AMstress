# Autonomous governance document index

This index is a human-readable map of the descriptive document-authority
manifest. It is reference material, not policy input, and has no authority to
override a primary or subordinate source.

## Taxonomy and authority

The manifest records a document's kind, domain, lifecycle status, authority,
policy-input role, risk floor, topics, and protecting tests. Primary and
subordinate current documents may be policy inputs in their stated domains.
Reference documents explain or assist; they are not policy inputs. Runbooks
are operational references, not permission to change governing policy.

Authority is domain-scoped. Current autonomy primary and subordinate sources
govern autonomy work in the autonomy domain. The scientific-model contract is
the primary authority for its scientific-model domain; autonomy policy does
not override it there, and that scientific contract does not override
autonomy policy outside that domain.

## Current sources and non-authority

`AUTONOMOUS_DEVELOPMENT.md` is the current primary autonomy policy.
`AUTONOMOUS_ORCHESTRATION.md`, the autonomous-work issue template, and the
current A4-A7 control-plane contracts are subordinate autonomy sources.
The worker runbook, troubleshooting guide, prompt guide, and this index are
reference material. Roadmaps, historical records, and retained fixture
evidence are not authority and cannot become policy input.

## Identity and conflict handling

Each manifest path is one canonical repository-relative POSIX identity. A
document or protecting-test path with an alias, traversal, platform-specific
separator, or other non-canonical spelling is invalid even if it resolves on a
filesystem.

Conflicting current authoritative sources are a preflight blocker. They
require reconciliation by the responsible authority; they do not grant
permission to guess, select a convenient source, or infer a new rule.
