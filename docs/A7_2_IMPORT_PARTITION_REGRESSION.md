# A7.2 import/partition layer-set regression

`partition-layer-sets-regression` is the first A7 production-code profile. It
uses the existing A7.1 isolated Windows/Abaqus validation role and retains the
hosted metadata gate, exact PR-head checkout, repeated live metadata check,
positive target-child environment allowlist, bounded timeout, and bounded
evidence record.

The profile has no user-selectable command, path, geometry, or expected-value
input. Its future controlled fixture path is fixed as
`tests/fixtures/a7_2_partition_layer_sets_regression.py`; it must execute the
exact detached target checkout's `import_and_partition.py`. That fixture is
intentionally absent from this implementation PR. After review and merge, a
separate, fresh same-repository fixture PR supplies it for the controlled live
acceptance; this profile fails closed while either fixed target file is absent.

The fixture must create only a deterministic simple solid with build extent
`10.3` and configure the existing production script with `LAYER_THK = 0.5`.
It must inspect the resulting Abaqus in-process model/CAE state, not console
output, and write the controller-provided fresh marker only after proving:

- part sets `BASE` and `BUILD_ALL`;
- assembly `set-0`;
- exactly `set-1` through `set-21` for the 21 build layers;
- `set-22` as the whole-build set; and
- `set-21` as the final partial layer.

The exact marker is `A7.2_PARTITION_LAYER_SETS_REGRESSION_PASSED`. A zero exit
code alone, missing/wrong/stale marker, missing fixed target file, target
failure, unavailable Abaqus runtime/license, timeout, identity failure, stale
head, metadata race, or protected target path cannot pass. The profile creates
no mesh, material, BC, thermal/heat-treatment state, activation/removal state,
job, solver result, GUI action, or ML action.

The existing `A6_PR_VALIDATION_EVIDENCE=` record adds only the bounded
`regression_result` field for this profile. It still excludes model output,
usernames, paths, environment data, license details, and credentials. No live
A7.2 acceptance or merge is enabled by this document.
