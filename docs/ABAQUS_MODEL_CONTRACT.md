# Abaqus model contract

This document separates current implementation behavior from engineering invariants that are not yet fully enforced. The latter are Phase-1 safety targets; changing an invariant requires an explicit issue.

## Sets

At part level, imported models use `BASE` for cells at or below the build-axis zero and `BUILD_ALL` for the build region. At assembly level, `set-0` is the base, `set-1` through `set-N` are consecutive build-layer slabs, and `set-(N+1)` is the aggregate whole-build region. The legacy parametric script uses equivalent capitalized `Set-*` names in places, so callers must preserve the naming convention of the path they invoke.

Current code creates layer sets from consecutive slab edges and the material script infers layer counts. For models containing `ImportedPart`, `create_input.py` now enforces a pre-input gate: it infers the lower-case assembly-set sequence, uses geometry-cell membership to verify the aggregate as the union of build-layer cells (with the one-layer `set-1`/`set-2` equal-geometry case handled explicitly), requires that aggregate to be named `set-(N+1)`, checks `set-0` and contiguous layer numbering, requires direct `set.elements` mesh membership for every required set, and compares the inferred count with GUI-injected `layer_n`. Geometry cells are used for membership and aggregate identification only; assembly-cell `Cell.getElements()` is not required. For per-cell completeness, a dependent assembly cell's `instanceName` and `index` are resolved to the owning `ImportedPart` cell and checked with `ImportedPart.getMeshStats(regions=(part_cell,))`; this is the verified Abaqus/CAE 2021 path because part-cell `getElements()` and assembly-cell `Cell.getElements()` return `None` in the imported model. Each required set therefore reports its geometry-cell count, meshed-cell count, and direct element count, and fails when any geometry cell is unmeshed or cannot be verified. The same gate runs the verified Abaqus/CAE 2021 `rootAssembly.verifyMeshQuality(ANALYSIS_CHECKS)` query: its `numElements`, `failedElements`, `warningElements`, and `naElements` values are reported, analysis-error elements block generation, and warnings are informational only. It raises before any `.inp`, UTEMP `.for`, or `submit.bat` output on failure; a terminal numbered set is never accepted as the aggregate without the geometry-union check. The gate deliberately does not impose the lower-case imported contract on legacy parametric models. Numerical mesh-quality repair and prevention of invalid `ModelChange` creation remain later Phase-1 work.

## Steps and ModelChange interactions

The intended imported-material sequence is:

1. `Step-1` is the base step.
2. `Step-2` through `Step-(N+1)` are the sequential layer steps.
3. `Step-(N+2)` is cooling.
4. `Step-(N+3)` releases/deactivates the base (`set-0`).
5. Optional additional steps deactivate bottom build layers `set-1` onward, as requested.
6. Optional heat treatment is intended to follow those removals. Current scripts have a known mismatch: `apply_materials.py` creates the HT step at `N + 4 + rem_layers`, while `create_input.py` writes the UTEMP HT branch at `KSTEP == layer_n + 4`; Input & UTEMP does not inject the bottom-removal count. When both features are enabled, the scripts therefore do not share the same HT step index.

`Int-1` initially deactivates the whole-build aggregate (`set-(N+1)` where detected). `Int-2` through `Int-(N+1)` activate layer 1 through N in order. The base-removal interaction is created at the base-removal step; optional bottom-layer interactions deactivate the requested lowest layers afterward. The material script can detect an aggregate by set size and correct to `set-(N+1)`, so documentation and future validation must treat detection as an implementation detail, not permission to reorder layers.

The legacy parametric script follows the same conceptual sequence but has older `Set-*` capitalization and an optional half-build set/removal block that is currently not implemented. In the Input & UTEMP tab, `layer_n` is user-entered (the current default is 24), not inferred from or validated against the CAE layer count.

## Meshing

The active imported-CAD mesher globally seeds with `BASE_SEED`, applies build/base directional reseeding, sets `FREE + TET` controls for all cells, and assigns C3D10 by default (or C3D4 when explicitly requested). Earlier sweep/hex C3D8R and mixed strategies in the file are commented-out legacy implementations. The active all-tet strategy is the robust default for arbitrary imported CAD. `create_input.py` now validates element membership in every required imported layer before writing inputs; numerical mesh-quality checks remain out of scope.

## Boundary conditions and temperature

`apply_boundary.py` applies a uniform 25°C initial predefined temperature and a USER_DEFINED temperature field beginning in Step-1 over `TEMP_ALL`. Bottom-node and corner selection is axis-aware, using the selected build axis and geometry. The displacement components are not remapped by axis: BC-1 is fixed global U1, BC-2 is fixed global U2, and BC-3 is fixed global U3. Axis-dependent component remapping, if desired, is a separate behavior change.

## Invariants

- Preserve the Python 3 GUI versus Abaqus runtime boundary.
- Preserve contiguous, correctly numbered base/layer/aggregate sets.
- Do not treat geometry-only sets as meshed element regions.
- Do not create interactions for missing or empty required regions.
- Preserve the current step ordering and optional-removal/heat-treatment ordering.
- Keep the active all-tet `FREE + TET + C3D10` imported meshing policy; any element-type or meshing-strategy change needs an explicit issue.
- Keep failures visible; do not swallow critical generation errors.
