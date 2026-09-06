# A7.1 isolated Abaqus target-code validation

A7.1 extends A6.2 with one future target-code profile, without enabling any
scientific fixture or changing model behavior. The manual trusted-main A6.2
metadata gate, exact-head binding, bounded pagination, protected-path policy,
and second pre-check remain mandatory.

## Dedicated identity prerequisite

Only `isolated-target-cae-smoke` may schedule the dedicated role
`windows-x64-abaqus-validation` with labels `self-hosted`, `windows`, `x64`,
and `ml-amstress-abaqus-validation`. The generic `ml-amstress-abaqus` label is
not sufficient. The job requires repository variables:

- `A7_EXPECTED_VALIDATION_RUNNER_NAME`
- `A7_EXPECTED_VALIDATION_WINDOWS_USER`
- `A7_VALIDATION_ISOLATION_READY` set to `isolated`

The runtime requires exact Windows/x64, runner-name, Windows-user, and
dedicated-label matches, and rejects either identity if it matches the existing
Codex runner/user variables. Both Codex-reference variables are mandatory and
the configured A7 runner and user must be distinct from them. Evidence never
includes the username.

Repository code cannot prove Windows account or ACL isolation. Before any live
acceptance, a maintainer must provision a separate Windows account/runner with
no Codex configuration or credentials, OpenAI API credentials, personal SSH
credentials, repository-write credentials, or unrelated-user secrets. The same
physical PC is permitted only with this deliberately separate execution
identity. Missing external setup fails closed.

## Fixed inert profile

The profile's only fixed target path is
`tests/fixtures/a7_1_target_cae_smoke.py`. It is intentionally absent from
this implementation: a later fresh harmless fixture PR supplies it after merge
and external setup. No workflow input, PR/issue text, target file, or artifact
can choose commands, paths, arguments, timeouts, environment policy, or
evidence.

On a fresh detached target checkout, the controller invokes the approved
Abaqus/CAE 2021 launcher only for this fixed fixture. The fixture must write
exactly `A7.1_ISOLATED_TARGET_CAE_SMOKE_PASSED` to the fresh controller-provided
sentinel path. Zero exit without that exact single marker, stale/missing/wrong
marker, timeout, unavailable runtime, or identity failure is not a pass.
The target child has no inherited controller environment: it receives only the
allowlisted Windows process values `SYSTEMROOT`, `WINDIR`, `COMSPEC`, `PATH`,
`PATHEXT`, `TEMP`, and `TMP`; explicit Abaqus/SIMULIA/DSLS and license families;
and the controller-provided sentinel path. It has no persisted checkout
credentials or GitHub Actions command-file/controller/identity variables. It
must not call production AM helpers or create models, geometry,
mesh, jobs, materials, BCs, thermal behavior, UTEMP, GUI, or ML results.

## Bounded evidence

The existing `A6_PR_VALIDATION_EVIDENCE=` record adds `isolation_result` and
uses the isolated runner role/labels for this profile. It binds the trusted
controller/run, target PR/issue/head, risk, profile, approved command/release,
isolation result, outcome, and fixed failure category. It excludes target and
Abaqus output, usernames, paths, environment data, licenses, and credentials.

No live target-code acceptance, fixture PR, scientific A7.2 work, merge, or
auto-merge is enabled by this document or implementation.
