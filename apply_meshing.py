# # # # -*- coding: utf-8 -*-
# # # """
# # # Mesh with separate base/build seeding by swapping the order:
# # #   1) Global seed = BASE_SEED
# # #   2) Local re-seed ONLY on BUILD edges (y > Y_ZERO) with BUILD_SEED
# # #      - Y-parallel edges   -> seedEdgeByNumber (count from BUILD_SEED)
# # #      - Non-Y edges        -> seedEdgeBySize (BUILD_SEED)
# # #   3) Try SWEEP + HEX (C3D8R default); fall back to FREE + TET (C3D10) if needed.

# # # Assumes import_and_partition.py already split at y = Y_ZERO.
# # # """

# # # from abaqus import *
# # # from abaqusConstants import *
# # # from caeModules import *
# # # import mesh
# # # import math, traceback
# # # from regionToolset import Region


# # # # ========= (Patched by GUI) =========
# # # CAE_FILE   = r""
# # # MODEL_NAME = "Model-1"
# # # PART_NAME  = "ImportedPart"   # leave "" to auto-pick the only part
# # # BASE_SEED  = 3.0              # now used as GLOBAL seed
# # # BUILD_SEED = 0.5              # applied only to edges in BUILD (coord > AXIS_ZERO)
# # # BUILD_AXIS = "Y"              # "X" | "Y" | "Z"
# # # AXIS_ZERO  = 0.0              # split plane provided by import step (often 0)
# # # TOL        = 1.0e-6
# # # ELEM_CODE  = "C3D8R"          # "C3D10", "C3D4", "C3D8R" (default: C3D8R)
# # # # ====================================

# # # # ---------- helpers ----------
# # # def repo_keys(repo):
# # #     try:
# # #         return list(repo.keys())
# # #     except Exception:
# # #         return [k for k in repo.keys()]

# # # def repo_only(repo, what="item"):
# # #     ks = repo_keys(repo)
# # #     if len(ks) == 1:
# # #         return repo[ks[0]]
# # #     raise RuntimeError("Expected exactly one %s, found %d (%s)" %
# # #                        (what, len(ks), ", ".join(sorted(ks))))

# # # def part_bbox(p):
# # #     xs, ys, zs = [], [], []
# # #     for v in p.vertices:
# # #         x, y, z = v.pointOn[0]
# # #         xs.append(x); ys.append(y); zs.append(z)
# # #     if not xs:
# # #         for e in p.edges:
# # #             x, y, z = e.pointOn[0]
# # #             xs.append(x); ys.append(y); zs.append(z)
# # #     if not xs:
# # #         raise RuntimeError("Cannot compute bounding box (no vertices/edges).")
# # #     return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

# # # def elem_type_from_code(code):
# # #     code = (code or "").upper()
# # #     if code == "C3D4":
# # #         return mesh.ElemType(elemCode=C3D4), TET, FREE
# # #     elif code == "C3D8R":
# # #         return mesh.ElemType(elemCode=C3D8R), HEX, SWEEP
# # #     else:
# # #         return mesh.ElemType(elemCode=C3D10), TET, FREE

# # # def edge_endpoints(edge):
# # #     """Return ((x1,y1,z1),(x2,y2,z2)) for an edge."""
# # #     try:
# # #         va = edge.getVertices()  # VertexArray
# # #         (x1, y1, z1) = va[0].pointOn[0]
# # #         (x2, y2, z2) = va[1].pointOn[0]
# # #         return (x1, y1, z1), (x2, y2, z2)
# # #     except Exception:
# # #         p1 = edge.pointOn[0]
# # #         p2 = edge.pointOn[-1]
# # #         return p1, p2

# # # def _axis_idx(axis):
# # #     axis = (axis or "Y").upper()
# # #     return 0 if axis == "X" else 2 if axis == "Z" else 1

# # # def _bbox_axis(x0,x1,y0,y1,z0,z1, axis, lo, hi, pad=0.0):
# # #     """Axis-aware bounding box tuple for getByBoundingBox()."""
# # #     if axis.upper() == "X":
# # #         return (lo, y0 - pad, z0 - pad, hi, y1 + pad, z1 + pad)
# # #     if axis.upper() == "Z":
# # #         return (x0 - pad, y0 - pad, lo, x1 + pad, y1 + pad, hi)
# # #     # default Y
# # #     return (x0 - pad, lo, z0 - pad, x1 + pad, hi, z1 + pad)

# # # def is_axis_parallel(p1, p2, axis, eps=1e-8):
# # #     """Edge is 'mostly parallel' to given axis if span on that coord dominates."""
# # #     dx = abs(p2[0] - p1[0]); dy = abs(p2[1] - p1[1]); dz = abs(p2[2] - p1[2])
# # #     spans = [dx, dy, dz]
# # #     i = _axis_idx(axis)
# # #     s = spans[i]; others = [spans[(i+1)%3], spans[(i+2)%3]]
# # #     return (s > max(10.0*others[0], 10.0*others[1])) and (s > eps)

# # # def seed_build_edges_after_global(p, base_seed, build_seed, axis_zero, tol, pad, axis="Y"):
# # #     """
# # #     Order:
# # #       (A) Global seed everywhere = BASE_SEED
# # #       (B) Re-seed ONLY edges fully in BUILD (y > Y_ZERO + eps) with BUILD_SEED:
# # #           - Y-parallel edges: by NUMBER (ceil(dy / BUILD_SEED))
# # #           - Non-Y edges    : by SIZE (BUILD_SEED)
# # #     """
# # #     # (A) global base seed for entire part
# # #     p.seedPart(size=float(base_seed), deviationFactor=0.1, minSizeFactor=0.1)

# # #     # (B) build-only edge selection (strictly above AXIS_ZERO)
# # #     x0, x1, y0, y1, z0, z1 = part_bbox(p)
# # #     thresh = float(axis_zero) + max(float(tol), 1.0e-9)
# # #     candidates = p.edges.getByBoundingBox(*_bbox_axis(x0, x1, y0, y1, z0, z1, axis, thresh, 1.0e99, pad))

# # #     axis_edges_build = []
# # #     non_axis_edges_build = []
# # #     for e in candidates:
# # #         p1e, p2e = edge_endpoints(e)
# # #         # keep only edges fully in build region (coord > thresh)
# # #         coord = _axis_idx(axis)
# # #         if (p1e[coord] > thresh) and (p2e[coord] > thresh):
# # #             if is_axis_parallel(p1e, p2e, axis):
# # #                 axis_edges_build.append((e, p1e, p2e))
# # #             else:
# # #                 non_axis_edges_build.append(e)

# # #     # Non-Y build edges: re-seed by size (overrides global)
# # #     if non_axis_edges_build:
# # #         p.seedEdgeBySize(
# # #             edges=non_axis_edges_build,
# # #             size=float(build_seed),
# # #             deviationFactor=0.1,
# # #             minSizeFactor=0.1,
# # #             constraint=FINER,
# # #         )
# # #         print("Build re-seed by SIZE on %d non-axis edges." % len(non_axis_edges_build))

# # #     # Y-parallel build edges: re-seed by number (sweep honors counts)
# # #     if axis_edges_build:
# # #         applied = 0
# # #         for e, p1e, p2e in axis_edges_build:
# # #             # span along chosen axis
# # #             c = _axis_idx(axis)
# # #             dL = abs(p2e[c] - p1e[c])
# # #             n_div = int(max(1, math.ceil(dL / float(build_seed))))
# # #             p.seedEdgeByNumber(edges=(e,), number=n_div, constraint=FINER)
# # #             applied += 1
# # #         print("Build re-seed by NUMBER on %d axis-parallel edges." % applied)

# # # def try_hex_then_mesh(p, elemtype):
# # #     """Try SWEEP+HEX; return True if meshing succeeds."""
# # #     p.setMeshControls(regions=p.cells, technique=SWEEP, elemShape=HEX)
# # #     p.setElementType(regions=(p.cells,), elemTypes=(elemtype,))
# # #     try:
# # #         p.generateMesh()
# # #         return True
# # #     except Exception as e:
# # #         print("Hex sweep meshing failed: %s" % str(e))
# # #         return False

# # # def mesh_free_tet(p, use_c3d4=False):
# # #     """Fallback: FREE+TET."""
# # #     et = mesh.ElemType(elemCode=(C3D4 if use_c3d4 else C3D10))
# # #     p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)
# # #     p.setElementType(regions=(p.cells,), elemTypes=(et,))
# # #     p.generateMesh()
# # #     return et.elemCode.name

# # # def mesh_mixed_hex_tet(p, desired_hex_elemtype, use_c3d4_fallback=False):
# # #     """
# # #     Try to sweep-mesh each cell individually (SWEEP+HEX). If a cell refuses,
# # #     switch that cell to FREE+TET and set a tet element type on it.
# # #     Returns (ok, n_hex, n_tet, used_codes_tuple)
# # #     """
# # #     n_hex = 0
# # #     n_tet = 0
# # #     used_hex = False
# # #     used_tet = False
# # #     tet_type = mesh.ElemType(elemCode=(C3D4 if use_c3d4_fallback else C3D10))

# # #     # Assign mesh controls + element types per cell
# # #     for i in range(len(p.cells)):
# # #         single = p.cells[i:i+1]  # <-- this is a GeomSequence (CellArray)
# # #         try:
# # #             # Try sweep hex on this cell
# # #             p.setMeshControls(regions=single, technique=SWEEP, elemShape=HEX)
# # #             p.setElementType(regions=single, elemTypes=(desired_hex_elemtype,))
# # #             n_hex += 1
# # #             used_hex = True
# # #         except Exception:
# # #             # Fall back to tet on this cell
# # #             try:
# # #                 p.setMeshControls(regions=single, technique=FREE, elemShape=TET)
# # #             except Exception:
# # #                 pass
# # #             p.setElementType(regions=single, elemTypes=(tet_type,))
# # #             n_tet += 1
# # #             used_tet = True

# # #     # Try to generate the mixed mesh
# # #     try:
# # #         p.generateMesh()
# # #         used_codes = []
# # #         if used_hex: used_codes.append(desired_hex_elemtype.elemCode.name)
# # #         if used_tet: used_codes.append(tet_type.elemCode.name)
# # #         if not used_codes:
# # #             used_codes = ("",)
# # #         return True, n_hex, n_tet, tuple(used_codes)
# # #     except Exception as e:
# # #         print("Mixed meshing failed, falling back to all TET. Reason: %s" % str(e))
# # #         # Hard fallback: wipe partial mesh + controls and go all-tet
# # #         try:
# # #             p.deleteMesh()
# # #         except Exception:
# # #             pass
# # #         p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)
# # #         tet_all = mesh.ElemType(elemCode=(C3D4 if use_c3d4_fallback else C3D10))
# # #         p.setElementType(regions=(p.cells,), elemTypes=(tet_all,))
# # #         p.generateMesh()
# # #         return False, 0, len(p.cells), (tet_all.elemCode.name,)

# # # # ---------- main ----------
# # # def main():
# # #     try:
# # #         if not CAE_FILE:
# # #             raise RuntimeError("CAE_FILE is empty. Provide a valid .cae path.")

# # #         openMdb(pathName=CAE_FILE)
# # #         if MODEL_NAME not in mdb.models.keys():
# # #             raise RuntimeError("Model '%s' not found. Available: %s" %
# # #                                (MODEL_NAME, ", ".join(sorted(repo_keys(mdb.models)))))

# # #         m = mdb.models[MODEL_NAME]

# # #         # Resolve part
# # #         if PART_NAME and PART_NAME in m.parts.keys():
# # #             p = m.parts[PART_NAME]
# # #         else:
# # #             p = repo_only(m.parts, what="part")
# # #             if PART_NAME:
# # #                 print("Warning: PART_NAME='%s' not found. Using the only part: '%s'." % (PART_NAME, p.name))

# # #         if len(p.cells) == 0:
# # #             raise RuntimeError("Part has 0 solid cells. Ensure solid geometry before meshing.")

# # #         # BBox padding
# # #         x0, x1, y0, y1, z0, z1 = part_bbox(p)
# # #         span = max(x1 - x0, y1 - y0, z1 - z0)
# # #         PAD  = max(TOL, 0.02 * span)

# # #         # (A+B) Global base seed, then build-only re-seeding (axis-aware)
# # #         seed_build_edges_after_global(p, BASE_SEED, BUILD_SEED, AXIS_ZERO, TOL, PAD, axis=BUILD_AXIS)

# # #         # Meshing: C3D8R default; tet fallback if sweep fails
# # #         used_code = ELEM_CODE
# # #         etype_desired, _, _ = elem_type_from_code("C3D8R")  # target hex for sweepable cells
# # #         ok, n_hex, n_tet, used_codes = mesh_mixed_hex_tet(p, etype_desired, use_c3d4_fallback=False)
# # #         used_code = "/".join(used_codes)
# # #         print("Mixed meshing summary: HEX=%d, TET=%d" % (n_hex, n_tet))
# # #         # Save DB
# # #         try:
# # #             mdb.save()
# # #         except Exception as e:
# # #             print("Warning: save failed: %s" % str(e))

# # #         print(("Meshing done."
# # #                "\n  Base (global) seed = %.6g"
# # #                "\n  Build (local) seed = %.6g"
# # #                "\n  Build axis = %s, zero = %.6g"
# # #                "\n  Elem requested = %s"
# # #                "\n  Elem used = %s") %
# # #               (float(BASE_SEED), float(BUILD_SEED), BUILD_AXIS, float(AXIS_ZERO), ELEM_CODE, used_code))
# # #     except Exception as exc:
# # #         print("ERROR in apply_meshing.py:", str(exc))
# # #         traceback.print_exc()
# # #         raise

# # # if __name__ == "__main__":
# # #     main()

# # # -*- coding: utf-8 -*-
# # """
# # All-tet meshing (simple & robust)
# # ---------------------------------
# # - Global seed only (single size).
# # - Mesh controls: FREE + TET for all cells.
# # - Element type: C3D10 (default) or C3D4 if ELEM_CODE="C3D4".

# # GUI will overwrite the constants below.
# # """

# # from abaqus import *
# # from abaqusConstants import *
# # from caeModules import *
# # import mesh
# # import traceback

# # # ========= (Patched by GUI) =========
# # CAE_FILE   = r""
# # MODEL_NAME = "Model-1"
# # PART_NAME  = "ImportedPart"   # leave "" to auto-pick the only part
# # # Use ONE global seed size. If you still pass BASE_SEED/BUILD_SEED from GUI,
# # # we'll pick BUILD_SEED (if >0) else BASE_SEED. Otherwise set GLOBAL_SEED.
# # BASE_SEED  = 3.0
# # BUILD_SEED = 0.5
# # GLOBAL_SEED = 0.0             # if >0, takes precedence over BASE/BUILD
# # ELEM_CODE  = "C3D10"          # "C3D10" (default) or "C3D4"
# # TOL        = 1.0e-6
# # # ====================================

# # def repo_keys(repo):
# #     try:
# #         return list(repo.keys())
# #     except Exception:
# #         return [k for k in repo.keys()]

# # def repo_only(repo, what="item"):
# #     ks = repo_keys(repo)
# #     if len(ks) == 1:
# #         return repo[ks[0]]
# #     raise RuntimeError("Expected exactly one %s, found %d (%s)" %
# #                        (what, len(ks), ", ".join(sorted(ks))))

# # def main():
# #     try:
# #         if not CAE_FILE:
# #             raise RuntimeError("CAE_FILE is empty. Provide a valid .cae path.")

# #         openMdb(pathName=CAE_FILE)
# #         if MODEL_NAME not in mdb.models:
# #             raise RuntimeError("Model '%s' not found. Available: %s" %
# #                                (MODEL_NAME, ", ".join(sorted(repo_keys(mdb.models)))))

# #         m = mdb.models[MODEL_NAME]

# #         # Resolve part
# #         if PART_NAME and PART_NAME in m.parts:
# #             p = m.parts[PART_NAME]
# #         else:
# #             p = repo_only(m.parts, what="part")
# #             if PART_NAME:
# #                 print("Warning: PART_NAME='%s' not found. Using the only part: '%s'." % (PART_NAME, p.name))

# #         if len(p.cells) == 0:
# #             raise RuntimeError("Part has 0 solid cells. Ensure solid geometry before meshing.")

# #         # Choose global seed
# #         seed_size = float(GLOBAL_SEED) if GLOBAL_SEED and GLOBAL_SEED > 0 else \
# #                     (float(BUILD_SEED) if BUILD_SEED and BUILD_SEED > 0 else float(BASE_SEED))
# #         if seed_size <= 0:
# #             raise RuntimeError("Global seed size must be > 0.")

# #         # Clear any existing mesh (safe)
# #         try:
# #             p.deleteMesh()
# #         except Exception:
# #             pass

# #         # Global seed
# #         p.seedPart(size=seed_size, deviationFactor=0.1, minSizeFactor=0.1)

# #         # All-tet controls + element type
# #         p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)

# #         tet_code = (ELEM_CODE or "C3D10").upper()
# #         if tet_code == "C3D4":
# #             tet_type = mesh.ElemType(elemCode=C3D4)
# #         else:
# #             tet_type = mesh.ElemType(elemCode=C3D10)
# #             tet_code = "C3D10"

# #         p.setElementType(regions=(p.cells,), elemTypes=(tet_type,))

# #         # Generate
# #         p.generateMesh()

# #         # Save
# #         try:
# #             mdb.save()
# #         except Exception as e:
# #             print("Warning: save failed: %s" % str(e))

# #         print(("All-tet meshing done."
# #                "\n  Global seed = %.6g"
# #                "\n  Element type = %s") % (seed_size, tet_code))

# #     except Exception as exc:
# #         print("ERROR in apply_meshing.py:", str(exc))
# #         traceback.print_exc()
# #         raise

# # if __name__ == "__main__":
# #     main()

# # -*- coding: utf-8 -*-
# """
# All-tet meshing with separate base/build seeding
# ------------------------------------------------
# - Global seed = BASE_SEED
# - Re-seed ONLY edges in the build region (> AXIS_ZERO along BUILD_AXIS) to BUILD_SEED
# - Mesh controls: FREE + TET for all cells
# - Element type: C3D10 (default) or C3D4 if ELEM_CODE="C3D4"
# """

# from abaqus import *
# from abaqusConstants import *
# from caeModules import *
# import mesh, traceback, math

# # ========= (Patched by GUI) =========
# CAE_FILE    = r""
# MODEL_NAME  = "Model-1"
# PART_NAME   = "ImportedPart"   # leave "" to auto-pick the only part
# BASE_SEED   = 3.0              # global/coarse
# BUILD_SEED  = 0.5              # local/fine (applied only in build region)
# BUILD_AXIS  = "Y"              # "X" | "Y" | "Z"
# AXIS_ZERO   = 0.0              # split plane position
# ELEM_CODE   = "C3D10"          # "C3D10" (default) or "C3D4"
# TOL         = 1.0e-6
# # ====================================

# # ---------- helpers ----------
# def repo_keys(repo):
#     try: return list(repo.keys())
#     except Exception: return [k for k in repo.keys()]

# def repo_only(repo, what="item"):
#     ks = repo_keys(repo)
#     if len(ks) == 1: return repo[ks[0]]
#     raise RuntimeError("Expected exactly one %s, found %d (%s)" %
#                        (what, len(ks), ", ".join(sorted(ks))))

# def part_bbox(p):
#     xs, ys, zs = [], [], []
#     for v in p.vertices:
#         x, y, z = v.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
#     if not xs:
#         for e in p.edges:
#             x, y, z = e.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
#     if not xs: raise RuntimeError("Cannot compute bounding box (no vertices/edges).")
#     return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

# def _axis_idx(axis):
#     a = (axis or "Y").upper()
#     return 0 if a == "X" else 2 if a == "Z" else 1

# def _bbox_axis(x0,x1,y0,y1,z0,z1, axis, lo, hi, pad=0.0):
#     """Axis-aware bounding box tuple for getByBoundingBox()."""
#     a = (axis or "Y").upper()
#     if a == "X": return (lo, y0 - pad, z0 - pad, hi, y1 + pad, z1 + pad)
#     if a == "Z": return (x0 - pad, y0 - pad, lo, x1 + pad, y1 + pad, hi)
#     # Y default
#     return (x0 - pad, lo, z0 - pad, x1 + pad, hi, z1 + pad)

# def edge_endpoints(edge):
#     try:
#         va = edge.getVertices()
#         (x1,y1,z1) = va[0].pointOn[0]
#         (x2,y2,z2) = va[1].pointOn[0]
#         return (x1,y1,z1), (x2,y2,z2)
#     except Exception:
#         p1 = edge.pointOn[0]; p2 = edge.pointOn[-1]
#         return p1, p2

# def reseed_build_edges_tet(p, build_seed, axis_zero, axis, tol, pad):
#     """
#     Re-seed only edges fully in the build region (coord > axis_zero + eps) by SIZE=build_seed.
#     """
#     x0, x1, y0, y1, z0, z1 = part_bbox(p)
#     span = max(x1 - x0, y1 - y0, z1 - z0)
#     EPS  = max(10.0*tol, 1.0e-6, 1.0e-6*span)

#     thresh = float(axis_zero) + EPS
#     bb = _bbox_axis(x0, x1, y0, y1, z0, z1, axis, thresh, 1.0e99, pad)
#     cand = p.edges.getByBoundingBox(*bb)

#     coord = _axis_idx(axis)
#     build_edges = []
#     for e in cand:
#         (a1,b1,c1), (a2,b2,c2) = edge_endpoints(e)
#         pt = (a1,b1,c1); qt = (a2,b2,c2)
#         if pt[coord] > thresh and qt[coord] > thresh:
#             build_edges.append(e)

#     if build_edges:
#         p.seedEdgeBySize(edges=build_edges,
#                          size=float(build_seed),
#                          deviationFactor=0.1,
#                          minSizeFactor=0.1,
#                          constraint=FINER)
#     return len(build_edges)

# # ---------- main ----------
# def main():
#     try:
#         if not CAE_FILE: raise RuntimeError("CAE_FILE is empty.")

#         openMdb(pathName=CAE_FILE)
#         if MODEL_NAME not in mdb.models:
#             raise RuntimeError("Model '%s' not found. Available: %s" %
#                                (MODEL_NAME, ", ".join(sorted(repo_keys(mdb.models)))))

#         m = mdb.models[MODEL_NAME]

#         # Resolve part
#         if PART_NAME and PART_NAME in m.parts:
#             p = m.parts[PART_NAME]
#         else:
#             p = repo_only(m.parts, what="part")
#             if PART_NAME:
#                 print("Warning: PART_NAME='%s' not found. Using the only part: '%s'." % (PART_NAME, p.name))

#         if len(p.cells) == 0:
#             raise RuntimeError("Part has 0 solid cells. Ensure solid geometry before meshing.")

#         # Clear any existing mesh
#         try: p.deleteMesh()
#         except Exception: pass

#         # BBox + padding
#         x0, x1, y0, y1, z0, z1 = part_bbox(p)
#         span = max(x1 - x0, y1 - y0, z1 - z0)
#         PAD  = max(TOL, 0.02*span)

#         # (A) Global seed = BASE_SEED (coarse, everywhere)
#         if BASE_SEED is None or float(BASE_SEED) <= 0.0:
#             raise RuntimeError("BASE_SEED must be > 0.")
#         p.seedPart(size=float(BASE_SEED), deviationFactor=0.1, minSizeFactor=0.1)

#         # (B) Re-seed edges ONLY in build region to BUILD_SEED (finer)
#         n_reseed = 0
#         if BUILD_SEED and float(BUILD_SEED) > 0.0:
#             n_reseed = reseed_build_edges_tet(p, BUILD_SEED, AXIS_ZERO, BUILD_AXIS, TOL, PAD)

#         # (C) All-tet controls + element type on ALL cells
#         p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)

#         tet_code = (ELEM_CODE or "C3D10").upper()
#         if tet_code == "C3D4":
#             tet_type = mesh.ElemType(elemCode=C3D4)
#             tet_str  = "C3D4"
#         else:
#             tet_type = mesh.ElemType(elemCode=C3D10)
#             tet_str  = "C3D10"

#         p.setElementType(regions=(p.cells,), elemTypes=(tet_type,))

#         # (D) Generate & save
#         p.generateMesh()
#         try: mdb.save()
#         except Exception as e: print("Warning: save failed: %s" % str(e))

#         print(("All-tet meshing done."
#                "\n  Global (base) seed = %.6g"
#                "\n  Build re-seed      = %.6g  (edges re-seeded: %d)"
#                "\n  Build axis / zero  = %s / %.6g"
#                "\n  Element type       = %s") %
#               (float(BASE_SEED), float(BUILD_SEED or 0.0), n_reseed, str(BUILD_AXIS), float(AXIS_ZERO), tet_str))

#     except Exception as exc:
#         print("ERROR in apply_meshing.py:", str(exc))
#         traceback.print_exc()
#         raise

# if __name__ == "__main__":
#     main()


# -*- coding: utf-8 -*-
"""
All-tet meshing with base-edge single bias + build reseed
---------------------------------------------------------
- Global seed = BASE_SEED (everywhere)
- BASE (coord < AXIS_ZERO): edges parallel to BUILD_AXIS get SINGLE-BIAS
    * minSize = BUILD_SEED  (near build interface)
    * maxSize = BASE_SEED   (at base bottom)
- BUILD (coord > AXIS_ZERO): edges reseeded by SIZE = BUILD_SEED
- Mesh controls: FREE + TET for all cells
- Element type: C3D10 (default) or C3D4 if ELEM_CODE="C3D4"
"""

from abaqus import *
from abaqusConstants import *
from caeModules import *
import mesh, traceback, math

# ========= (Patched by GUI) =========
CAE_FILE    = r""
MODEL_NAME  = "Model-1"
PART_NAME   = "ImportedPart"
BASE_SEED   = 3.0              # coarse
BUILD_SEED  = 0.5              # fine
BUILD_AXIS  = "Y"              # "X" | "Y" | "Z"
AXIS_ZERO   = 0.0
ELEM_CODE   = "C3D10"          # "C3D10" or "C3D4"
TOL         = 1.0e-6
# ====================================

# ---------- helpers ----------
def repo_keys(repo):
    try: return list(repo.keys())
    except Exception: return [k for k in repo.keys()]

def repo_only(repo, what="item"):
    ks = repo_keys(repo)
    if len(ks) == 1: return repo[ks[0]]
    raise RuntimeError("Expected exactly one %s, found %d (%s)" %
                       (what, len(ks), ", ".join(sorted(repo_keys(repo)))))

def part_bbox(p):
    xs, ys, zs = [], [], []
    for v in p.vertices:
        x, y, z = v.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        for e in p.edges:
            x, y, z = e.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
    if not xs: raise RuntimeError("Cannot compute bounding box (no vertices/edges).")
    return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

def _axis_idx(axis):
    a = (axis or "Y").upper()
    return 0 if a == "X" else 2 if a == "Z" else 1

def _bbox_axis(x0,x1,y0,y1,z0,z1, axis, lo, hi, pad=0.0):
    """Axis-aware bounding box tuple for getByBoundingBox()."""
    a = (axis or "Y").upper()
    if a == "X": return (lo, y0 - pad, z0 - pad, hi, y1 + pad, z1 + pad)
    if a == "Z": return (x0 - pad, y0 - pad, lo, x1 + pad, y1 + pad, hi)
    # Y default
    return (x0 - pad, lo, z0 - pad, x1 + pad, hi, z1 + pad)

def edge_endpoints(edge):
    try:
        va = edge.getVertices()
        (x1,y1,z1) = va[0].pointOn[0]
        (x2,y2,z2) = va[1].pointOn[0]
        return (x1,y1,z1), (x2,y2,z2)
    except Exception:
        p1 = edge.pointOn[0]; p2 = edge.pointOn[-1]
        return p1, p2

def is_axis_parallel(p1, p2, axis, eps=1e-8):
    """Edge is 'mostly parallel' to given axis if span on that coord dominates."""
    dx = abs(p2[0] - p1[0]); dy = abs(p2[1] - p1[1]); dz = abs(p2[2] - p1[2])
    spans = [dx, dy, dz]
    i = _axis_idx(axis)
    s = spans[i]; others = [spans[(i+1)%3], spans[(i+2)%3]]
    return (s > max(10.0*others[0], 10.0*others[1])) and (s > eps)

def reseed_build_edges_tet(p, build_seed, axis_zero, axis, tol, pad):
    """
    Re-seed only edges fully in the build region (coord > axis_zero + eps) by SIZE=build_seed.
    """
    x0, x1, y0, y1, z0, z1 = part_bbox(p)
    span = max(x1 - x0, y1 - y0, z1 - z0)
    EPS  = max(10.0*tol, 1.0e-6, 1.0e-6*span)

    thresh = float(axis_zero) + EPS
    bb = _bbox_axis(x0, x1, y0, y1, z0, z1, axis, thresh, 1.0e99, pad)
    cand = p.edges.getByBoundingBox(*bb)

    coord = _axis_idx(axis)
    build_edges = []
    for e in cand:
        (a1,b1,c1), (a2,b2,c2) = edge_endpoints(e)
        pt = (a1,b1,c1); qt = (a2,b2,c2)
        if pt[coord] > thresh and qt[coord] > thresh:
            build_edges.append(e)

    if build_edges:
        p.seedEdgeBySize(edges=build_edges,
                         size=float(build_seed),
                         deviationFactor=0.1,
                         minSizeFactor=0.1,
                         constraint=FINER)
    return len(build_edges)

def bias_base_axis_edges(p, base_seed, build_seed, axis_zero, axis, tol, pad):
    """
    Apply SINGLE-BIAS on base-region edges parallel to BUILD_AXIS:
      - maxSize = base_seed at base bottom (smaller coord along axis)
      - minSize = build_seed near build interface (larger coord; closer to axis_zero)
    """
    if base_seed <= 0.0 or build_seed <= 0.0:
        return (0, 0)

    x0, x1, y0, y1, z0, z1 = part_bbox(p)
    span = max(x1 - x0, y1 - y0, z1 - z0)
    EPS  = max(10.0*tol, 1.0e-6, 1.0e-6*span)

    upper = float(axis_zero) - EPS  # strictly below the interface
    bb = _bbox_axis(x0, x1, y0, y1, z0, z1, axis, -1.0e99, upper, pad)
    cand = p.edges.getByBoundingBox(*bb)

    coord = _axis_idx(axis)
    end1_small = []  # edges where the 'end1' endpoint is closer to build -> min at end1
    end2_small = []  # edges where the 'end2' endpoint is closer to build -> min at end2
    kept = 0
    for e in cand:
        p1, p2 = edge_endpoints(e)
        # fully in base, parallel to axis
        if (p1[coord] < upper and p2[coord] < upper) and is_axis_parallel(p1, p2, axis):
            kept += 1
            # which endpoint is nearer to the build interface? (larger coord)
            # compare to end1 vs end2 of this Edge (p1 is end1)
            if p1[coord] >= p2[coord]:
                # end1 is nearer to build -> min at end1
                end1_small.append(e)
            else:
                end2_small.append(e)

    applied = 0
    if end1_small or end2_small:
        try:
            # Primary attempt: specify min/max sizes (SINGLE bias) with edge-end groups
            p.seedEdgeByBias(
                biasMethod=SINGLE,
                end1Edges=end1_small,
                end2Edges=end2_small,
                minSize=float(build_seed),
                maxSize=float(base_seed),
                constraint=FINER,
            )
            applied = len(end1_small) + len(end2_small)
        except Exception as e:
            # Fallback: try number + biasRatio; estimate number per edge and apply per-edge
            # (kept conservative; this rarely triggers on standard 2021 installations)
            bias_ratio = max(1.0, float(base_seed) / float(build_seed))
            for e in end1_small + end2_small:
                try:
                    p1, p2 = edge_endpoints(e)
                    dL = abs(p2[coord] - p1[coord])
                    n  = max(2, int(math.ceil(dL / float(build_seed))))
                    if e in end1_small:
                        p.seedEdgeByBias(biasMethod=SINGLE, end1Edges=(e,), number=n,
                                         biasRatio=bias_ratio, constraint=FINER)
                    else:
                        p.seedEdgeByBias(biasMethod=SINGLE, end2Edges=(e,), number=n,
                                         biasRatio=bias_ratio, constraint=FINER)
                    applied += 1
                except Exception:
                    pass

    return (kept, applied)

# ---------- main ----------
def main():
    try:
        if not CAE_FILE: raise RuntimeError("CAE_FILE is empty.")

        openMdb(pathName=CAE_FILE)
        if MODEL_NAME not in mdb.models:
            raise RuntimeError("Model '%s' not found. Available: %s" %
                               (MODEL_NAME, ", ".join(sorted(repo_keys(mdb.models)))))

        m = mdb.models[MODEL_NAME]

        # Resolve part
        if PART_NAME and PART_NAME in m.parts:
            p = m.parts[PART_NAME]
        else:
            p = repo_only(m.parts, what="part")
            if PART_NAME:
                print("Warning: PART_NAME='%s' not found. Using the only part: '%s'." % (PART_NAME, p.name))

        if len(p.cells) == 0:
            raise RuntimeError("Part has 0 solid cells. Ensure solid geometry before meshing.")

        # Clear any existing mesh
        try: p.deleteMesh()
        except Exception: pass

        # BBox + padding
        x0, x1, y0, y1, z0, z1 = part_bbox(p)
        span = max(x1 - x0, y1 - y0, z1 - z0)
        PAD  = max(TOL, 0.02*span)

        # (A) Global seed = BASE_SEED everywhere
        if BASE_SEED is None or float(BASE_SEED) <= 0.0:
            raise RuntimeError("BASE_SEED must be > 0.")
        p.seedPart(size=float(BASE_SEED), deviationFactor=0.1, minSizeFactor=0.1)

        # (B) Base-region axis-parallel edges: SINGLE-BIAS (max at bottom, min near build)
        kept, biased = bias_base_axis_edges(p, float(BASE_SEED), float(BUILD_SEED),
                                            AXIS_ZERO, BUILD_AXIS, TOL, PAD)

        # (C) Build-region edges: re-seed by SIZE = BUILD_SEED
        n_reseed = 0
        if BUILD_SEED and float(BUILD_SEED) > 0.0:
            n_reseed = reseed_build_edges_tet(p, float(BUILD_SEED), AXIS_ZERO,
                                              BUILD_AXIS, TOL, PAD)

        # (D) All-tet controls + element type on ALL cells
        p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)
        tet_code = (ELEM_CODE or "C3D10").upper()
        if tet_code == "C3D4":
            tet_type = mesh.ElemType(elemCode=C3D4); tet_str = "C3D4"
        else:
            tet_type = mesh.ElemType(elemCode=C3D10); tet_str = "C3D10"
        p.setElementType(regions=(p.cells,), elemTypes=(tet_type,))

        # (E) Generate & save
        p.generateMesh()
        try: mdb.save()
        except Exception as e: print("Warning: save failed: %s" % str(e))

        print(("All-tet meshing done."
               "\n  Global (base) seed     = %.6g"
               "\n  Base bias edges kept   = %d, applied = %d"
               "\n  Build reseed (by size) = %.6g  (edges re-seeded: %d)"
               "\n  Build axis / zero      = %s / %.6g"
               "\n  Element type           = %s") %
              (float(BASE_SEED), kept, biased, float(BUILD_SEED or 0.0),
               n_reseed, str(BUILD_AXIS), float(AXIS_ZERO), tet_str))

    except Exception as exc:
        print("ERROR in apply_meshing.py:", str(exc))
        traceback.print_exc()
        raise

if __name__ == "__main__":
    main()
