# -*- coding: utf-8 -*-
"""
Mesh with separate base/build seeding by swapping the order:
  1) Global seed = BASE_SEED
  2) Local re-seed ONLY on BUILD edges (y > Y_ZERO) with BUILD_SEED
     - Y-parallel edges   -> seedEdgeByNumber (count from BUILD_SEED)
     - Non-Y edges        -> seedEdgeBySize (BUILD_SEED)
  3) Try SWEEP + HEX (C3D8R default); fall back to FREE + TET (C3D10) if needed.

Assumes import_and_partition.py already split at y = Y_ZERO.
"""

from abaqus import *
from abaqusConstants import *
from caeModules import *
import mesh
import math, traceback

# ========= (Patched by GUI) =========
CAE_FILE   = r""
MODEL_NAME = "Model-1"
PART_NAME  = "ImportedPart"   # leave "" to auto-pick the only part
BASE_SEED  = 3.0              # now used as GLOBAL seed
BUILD_SEED = 0.5              # applied only to edges in BUILD (y > Y_ZERO)
Y_ZERO     = 0.0              # split plane provided by import step (often 0)
TOL        = 1.0e-6
ELEM_CODE  = "C3D8R"          # "C3D10", "C3D4", "C3D8R" (default: C3D8R)
# ====================================

# ---------- helpers ----------
def repo_keys(repo):
    try:
        return list(repo.keys())
    except Exception:
        return [k for k in repo.keys()]

def repo_only(repo, what="item"):
    ks = repo_keys(repo)
    if len(ks) == 1:
        return repo[ks[0]]
    raise RuntimeError("Expected exactly one %s, found %d (%s)" %
                       (what, len(ks), ", ".join(sorted(ks))))

def part_bbox(p):
    xs, ys, zs = [], [], []
    for v in p.vertices:
        x, y, z = v.pointOn[0]
        xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        for e in p.edges:
            x, y, z = e.pointOn[0]
            xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        raise RuntimeError("Cannot compute bounding box (no vertices/edges).")
    return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

def elem_type_from_code(code):
    code = (code or "").upper()
    if code == "C3D4":
        return mesh.ElemType(elemCode=C3D4), TET, FREE
    elif code == "C3D8R":
        return mesh.ElemType(elemCode=C3D8R), HEX, SWEEP
    else:
        return mesh.ElemType(elemCode=C3D10), TET, FREE

def edge_endpoints(edge):
    """Return ((x1,y1,z1),(x2,y2,z2)) for an edge."""
    try:
        va = edge.getVertices()  # VertexArray
        (x1, y1, z1) = va[0].pointOn[0]
        (x2, y2, z2) = va[1].pointOn[0]
        return (x1, y1, z1), (x2, y2, z2)
    except Exception:
        p1 = edge.pointOn[0]
        p2 = edge.pointOn[-1]
        return p1, p2

def is_y_parallel(p1, p2, eps=1e-8):
    dx = abs(p2[0] - p1[0])
    dz = abs(p2[2] - p1[2])
    dy = abs(p2[1] - p1[1])
    # "mostly vertical": y-span dominates x/z and is non-negligible
    return (dy > max(10.0*dx, 10.0*dz)) and (dy > eps)

def seed_build_edges_after_global(p, base_seed, build_seed, y_zero, tol, pad):
    """
    Order:
      (A) Global seed everywhere = BASE_SEED
      (B) Re-seed ONLY edges fully in BUILD (y > Y_ZERO + eps) with BUILD_SEED:
          - Y-parallel edges: by NUMBER (ceil(dy / BUILD_SEED))
          - Non-Y edges    : by SIZE (BUILD_SEED)
    """
    # (A) global base seed for entire part
    p.seedPart(size=float(base_seed), deviationFactor=0.1, minSizeFactor=0.1)

    # (B) build-only edge selection
    x0, x1, y0, y1, z0, z1 = part_bbox(p)
    y_thresh = float(y_zero) + max(float(tol), 1.0e-9)  # "strictly above" the split

    # preselect edges strictly above Y_ZERO
    candidates = p.edges.getByBoundingBox(
        x0 - pad, y_thresh, z0 - pad,
        x1 + pad, y1 + pad, z1 + pad
    )

    y_edges_build = []
    xz_edges_build = []
    for e in candidates:
        p1e, p2e = edge_endpoints(e)
        # keep only edges fully in build
        if (p1e[1] > y_thresh) and (p2e[1] > y_thresh):
            if is_y_parallel(p1e, p2e):
                y_edges_build.append((e, p1e, p2e))
            else:
                xz_edges_build.append(e)

    # Non-Y build edges: re-seed by size (overrides global)
    if xz_edges_build:
        p.seedEdgeBySize(
            edges=xz_edges_build,
            size=float(build_seed),
            deviationFactor=0.1,
            minSizeFactor=0.1,
            constraint=FINER,
        )
        print("Build re-seed by SIZE on %d non-Y edges." % len(xz_edges_build))

    # Y-parallel build edges: re-seed by number (sweep honors counts)
    if y_edges_build:
        applied = 0
        for e, p1e, p2e in y_edges_build:
            dy = abs(p2e[1] - p1e[1])
            n_div = int(max(1, math.ceil(dy / float(build_seed))))
            p.seedEdgeByNumber(edges=(e,), number=n_div, constraint=FINER)
            applied += 1
        print("Build re-seed by NUMBER on %d Y-parallel edges." % applied)

def try_hex_then_mesh(p, elemtype):
    """Try SWEEP+HEX; return True if meshing succeeds."""
    p.setMeshControls(regions=p.cells, technique=SWEEP, elemShape=HEX)
    p.setElementType(regions=(p.cells,), elemTypes=(elemtype,))
    try:
        p.generateMesh()
        return True
    except Exception as e:
        print("Hex sweep meshing failed: %s" % str(e))
        return False

def mesh_free_tet(p, use_c3d4=False):
    """Fallback: FREE+TET."""
    et = mesh.ElemType(elemCode=(C3D4 if use_c3d4 else C3D10))
    p.setMeshControls(regions=p.cells, technique=FREE, elemShape=TET)
    p.setElementType(regions=(p.cells,), elemTypes=(et,))
    p.generateMesh()
    return et.elemCode.name

# ---------- main ----------
def main():
    try:
        if not CAE_FILE:
            raise RuntimeError("CAE_FILE is empty. Provide a valid .cae path.")

        openMdb(pathName=CAE_FILE)
        if MODEL_NAME not in mdb.models.keys():
            raise RuntimeError("Model '%s' not found. Available: %s" %
                               (MODEL_NAME, ", ".join(sorted(repo_keys(mdb.models)))))

        m = mdb.models[MODEL_NAME]

        # Resolve part
        if PART_NAME and PART_NAME in m.parts.keys():
            p = m.parts[PART_NAME]
        else:
            p = repo_only(m.parts, what="part")
            if PART_NAME:
                print("Warning: PART_NAME='%s' not found. Using the only part: '%s'." % (PART_NAME, p.name))

        if len(p.cells) == 0:
            raise RuntimeError("Part has 0 solid cells. Ensure solid geometry before meshing.")

        # BBox padding
        x0, x1, y0, y1, z0, z1 = part_bbox(p)
        span = max(x1 - x0, y1 - y0, z1 - z0)
        PAD  = max(TOL, 0.02 * span)

        # (A+B) Global base seed, then build-only re-seeding
        seed_build_edges_after_global(p, BASE_SEED, BUILD_SEED, Y_ZERO, TOL, PAD)

        # Meshing: C3D8R default; tet fallback if sweep fails
        used_code = ELEM_CODE
        etype_desired, _, _ = elem_type_from_code(ELEM_CODE)
        if ELEM_CODE.upper() == "C3D8R":
            if not try_hex_then_mesh(p, etype_desired):
                print("Falling back to FREE + TET (C3D10).")
                used_code = mesh_free_tet(p, use_c3d4=False)
        else:
            if ELEM_CODE.upper() == "C3D4":
                used_code = mesh_free_tet(p, use_c3d4=True)
            else:
                used_code = mesh_free_tet(p, use_c3d4=False)

        # Save DB
        try:
            mdb.save()
        except Exception as e:
            print("Warning: save failed: %s" % str(e))

        print("Meshing done.\n  Base (global) seed = %.6g\n  Build (local) seed = %.6g\n  Elem requested = %s\n  Elem used = %s"
              % (float(BASE_SEED), float(BUILD_SEED), ELEM_CODE, used_code))

    except Exception as exc:
        print("ERROR in apply_meshing.py:", str(exc))
        traceback.print_exc()
        raise

if __name__ == "__main__":
    main()
