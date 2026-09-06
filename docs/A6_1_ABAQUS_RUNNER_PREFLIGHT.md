# A6.1 trusted Abaqus runner preflight

A6.1 establishes only the trusted Windows Abaqus runtime boundary. It is not
an Abaqus model-generation, solver, GUI, or scientific validation workflow.

## Runner contract

The dedicated runner role is `windows-x64-abaqus`, scheduled only with these
labels:

- `self-hosted`
- `windows`
- `x64`
- `ml-amstress-abaqus`

This role is distinct from the `ml-amstress-codex` runner role, even when a
maintainer initially hosts both labels on one physical workstation. The
designated runner must have non-secret repository variables
`A6_EXPECTED_RUNNER_NAME` and `A6_EXPECTED_WINDOWS_USER`; missing or mismatched
values fail the preflight.

The sole approved launcher is
`C:/SIMULIA/Commands/abq2021.bat`. A6.1 does not search `PATH`, select another
SIMULIA install, or fall back to another Abaqus release. The launcher must
report release `2021`.

## Trusted execution boundary

The [A6.1 workflow](../.github/workflows/a6-abaqus-preflight.yml) is manual
`workflow_dispatch` only. Its job runs only when the repository is
`RayZhang2024/ML-AMstress` and the dispatch ref is `refs/heads/main`; it checks
out the exact trusted dispatch SHA with `persist-credentials: false`. It has only `contents: read`
permission and does not create or modify issues, labels, pull requests,
branches, or merge state. It does not execute PR or fork content.
The executable `scripts/a6_` control-plane prefix is excluded from both the
ordinary GREEN worker and A5 repair edit surfaces.

The workflow invokes `scripts/a6_abaqus_preflight.py`, which runs a bounded
release query and then the separate `scripts/a6_abaqus_probe.py` in a temporary
directory. The probe only prints its fixed success marker. It creates no
model, geometry, mesh, material, boundary condition, thermal step, UTEMP
input, solver job, or project file, and it does not invoke production Abaqus
helpers.

## Outcomes and evidence

The deterministic outcome is one of:

- `passed` — the approved launcher reports release 2021 and the bounded CAE
  noGUI marker returns successfully;
- `failed` — the runner contract, launcher release, probe marker, or timeout
  fails;
- `unavailable` — the approved launcher is missing/unusable or the licensed
  runtime reports unavailable.

Each run emits one bounded JSON evidence line containing only the GitHub run
identity, trusted repository SHA, role and labels, the non-secret command
identity `abq2021.bat`, release, outcome, and a fixed failure category. It
does not emit launcher output, license-server information, environment dumps,
usernames, local paths, credentials, or filesystem inventories.

A6.2 will define bounded exact-PR Abaqus validation. A7 will define scientific
regression fixtures. Neither is enabled by A6.1. The controlled live A6.1 run
is deliberately pending human review and runner setup; it must be run after
this implementation PR is reviewed, before A6.1 is considered complete.
