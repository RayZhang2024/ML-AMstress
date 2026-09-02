# Engineering roadmap

## Phase 0 — engineering foundation

This issue: establish agent rules, current-state architecture, the Abaqus model contract, development/validation language, PR review structure, and an explicit roadmap.

## Phase 1 — safety / validation

- Validate expected layer sets and contiguity.
- Detect empty or unmeshed layer sets.
- Add mesh validation and clear diagnostics.
- Prevent invalid input generation.

## Phase 2 — configuration / architecture

- Reduce regex script patching.
- Introduce structured configuration.
- Clean obsolete meshing implementations.
- Improve error handling.

## Phase 3 — model viewer

- Export a neutral model/mesh representation.
- Add a PyVista/PyVistaQt viewer.
- Show geometry, layers, materials, mesh, and element types.

## Phase 4 — modularisation

Progressively split `AM_gui_v7.py` into cohesive modules without a big-bang rewrite.

## Phase 5 — advanced diagnostics/results

Add mesh-quality and boundary-condition visualization, followed eventually by result/ODB-derived visualization.

These are directions, not commitments to implement them in Issue #1.
