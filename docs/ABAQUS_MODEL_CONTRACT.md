# Abaqus model contract

This is the contract implied by the current model-building scripts. It is descriptive for existing behavior; changing an invariant requires a separate issue.

## Sets

At part level, imported models use `BASE` for cells at or below the build-axis zero and `BUILD_ALL` for the build region. At assembly level, `set-0` is the base, `set-1` through `set-N` are consecutive build-layer slabs, and `set-(N+1)` is the aggregate whole-build region. The legacy parametric script uses equivalent capitalized `Set-*` names in places, so callers must preserve the naming convention of the path they invoke.

Layer sets are created from consecutive slab edges. Their numbering must be contiguous and must cover the intended layers. A geometry set is not sufficient for input generation: after meshing, every required layer set must exist and contain elements. Missing or empty required sets must be diagnosed rather than silently skipped, and required `ModelChange` regions must never be undefined or empty.

## Steps and ModelChange interactions

The intended imported-material sequence is:

1. `Step-1` is the base step.
2. `Step-2` through `Step-(N+1)` are the sequential layer steps.
3. `Step-(N+2)` is cooling.
4. `Step-(N+3)` releases/deactivates the base (`set-0`).
5. Optional additional steps deactivate bottom build layers `set-1` onward, as requested.
6. Optional heat treatment follows those removals when enabled; the current implementation shifts its step index by the number of removed bottom layers.

`Int-1` initially deactivates the whole-build aggregate (`set-(N+1)` where detected). `Int-2` through `Int-(N+1)` activate layer 1 through N in order. The base-removal interaction is created at the base-removal step; optional bottom-layer interactions deactivate the requested lowest layers afterward. The material script can detect an aggregate by set size and correct to `set-(N+1)`, so documentation and future validation must treat detection as an implementation detail, not permission to reorder layers.

The legacy parametric script follows the same conceptual sequence but has older `Set-*` capitalization and an optional half-build set/removal block that is currently not implemented.

## Meshing

The active imported-CAD mesher globally seeds with `BASE_SEED`, applies build/base directional reseeding, and currently requests sweep + HEX with C3D8R by default; if sweep/hex is unsuitable it has mixed/fallback tetrahedral paths using C3D10 (or explicitly requested C3D4). This means the current code is not a universal C3D10-only policy. For arbitrary imported CAD, the robust engineering direction is `FREE + TET + C3D10`; adopting it as runtime default is out of scope here. Never silently introduce mixed C3D8R/C3D10 elements, and always validate element membership in required layer sets before writing inputs.

## Boundary conditions and temperature

`apply_boundary.py` applies a uniform 25°C initial predefined temperature and a USER_DEFINED temperature field beginning in Step-1 over `TEMP_ALL`. It derives axis-aware anti-rigid-body constraints from bottom nodes: BC-1 constrains one displacement component, BC-2 constrains a second component at selected corners, and BC-3 constrains the remaining component/hold location. The exact component mapping follows the selected build axis and must remain axis-aware.

## Invariants

- Preserve the Python 3 GUI versus Abaqus runtime boundary.
- Preserve contiguous, correctly numbered base/layer/aggregate sets.
- Do not treat geometry-only sets as meshed element regions.
- Do not create interactions for missing or empty required regions.
- Preserve the current step ordering and optional-removal/heat-treatment ordering.
- Keep one coherent element-type policy per meshing operation; any C3D10 default change needs an explicit issue.
- Keep failures visible; do not swallow critical generation errors.
