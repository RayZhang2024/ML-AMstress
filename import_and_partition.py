# -*- coding: utf-8 -*-
# import_and_partition.py  (v1.2.1 - exact planes; sets from exact slab edges, top layer guaranteed)
# Import CAD (STEP/IGES/SAT), partition along selected BUILD_AXIS from AXIS_ZERO.
# Part-level sets: only BASE and BUILD_ALL.
# Assembly sets: set-0 (base), set-1..set-N (layers), set-(N+1) (whole build).

from abaqus import *
from abaqusConstants import *
from caeModules import *
import os, math

__version__ = "1.2.1"  # fix: last top layer always created (sets built from edges, not plane list)

# ======= GUI-overwritten constants (defaults for standalone runs) =======
CAD_FILE   = r""
SCALE      = 1.0
LAYER_THK  = 0.5
BUILD_H    = 10.0           # total build height above AXIS_ZERO; <=0 uses model extent
BUILD_AXIS = "Y"            # "X" | "Y" | "Z"
AXIS_ZERO  = 0.0            # slicing origin on selected axis
SAVE_AS    = r""
TOL        = 1.0e-9
# =======================================================================

# ---- helpers -----------------------------------------------------------
def _axis_idx(axis):
    a = (axis or "Y").upper()
    return 0 if a == "X" else 2 if a == "Z" else 1

def _principal_plane_perp(axis):
    a = (axis or "Y").upper()
    if a == "X": return YZPLANE   # x = const
    if a == "Z": return XYPLANE   # z = const
    return XZPLANE                # y = const

def _minmax_along_axis(mins, maxs, axis):
    i = _axis_idx(axis)
    return mins[i], maxs[i]

# ---- main --------------------------------------------------------------
def main():
    Mdb()

    # --- Import geometry ---
    ext = os.path.splitext(CAD_FILE)[1].lower()
    if ext in ('.stp', '.step'):
        geom = mdb.openStep(CAD_FILE, scale=SCALE)
    elif ext in ('.igs', '.iges'):
        geom = mdb.openIges(CAD_FILE, topology=SOLID)
    elif ext == '.sat':
        geom = mdb.openAcis(CAD_FILE)
    else:
        raise RuntimeError('Unsupported CAD extension: %s' % ext)

    # Create deformable part
    p = mdb.models['Model-1'].PartFromGeometryFile(
        name='ImportedPart',
        geometryFile=geom,
        dimensionality=THREE_D,
        type=DEFORMABLE_BODY
    )

    # Part bbox (verts->edges fallback)
    xs=[]; ys=[]; zs=[]
    for v in p.vertices:
        x,y,z = v.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        for e in p.edges:
            x,y,z = e.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        raise RuntimeError('Cannot compute bounding box (no vertices/edges)')

    x0,x1 = min(xs), max(xs)
    y0,y1 = min(ys), max(ys)
    z0,z1 = min(zs), max(zs)
    mins = (x0, y0, z0); maxs = (x1, y1, z1)

    # Simple geometry-based scales for selection boxes (not for plane placement)
    SPAN   = max((x1 - x0), (y1 - y0), (z1 - z0))
    EPS    = max(10.0*TOL, 1.0e-6, 1.0e-6 * SPAN)
    MARGIN = 10.0 * EPS

    # Require solid cells
    if len(p.cells) == 0:
        raise RuntimeError('No solid cells found (surface/wire geometry?)')

    # --- Build height from AXIS_ZERO upward (along BUILD_AXIS) ---
    axis_min, axis_max = _minmax_along_axis(mins, maxs, BUILD_AXIS)
    if axis_max <= AXIS_ZERO + TOL:
        raise RuntimeError('No geometry above AXIS_ZERO to slice along %s' % BUILD_AXIS)

    top_limit = (AXIS_ZERO + BUILD_H) if BUILD_H > 0.0 else axis_max
    axis_top  = min(top_limit, axis_max)
    H = max(0.0, axis_top - AXIS_ZERO)
    if H <= 0.0:
        raise RuntimeError('Invalid slicing height (H<=0) along %s' % BUILD_AXIS)
    if LAYER_THK <= 0.0:
        raise RuntimeError('LAYER_THK must be > 0.')

    # ===================== STEP 1: CREATE ALL DATUM PLANES =====================
    plane_ids = []
    pp = _principal_plane_perp(BUILD_AXIS)

    # Exact base plane at AXIS_ZERO (create only if strictly inside bbox to avoid CAE error)
    if axis_min + EPS < AXIS_ZERO < axis_max - EPS:
        try:
            dp0 = p.DatumPlaneByPrincipalPlane(principalPlane=pp, offset=AXIS_ZERO)
            plane_ids.append(dp0.id)
        except:
            print('Note: failed to create base datum plane at AXIS_ZERO.')
    else:
        print('Note: base plane skipped (AXIS_ZERO out of bounds along %s).' % BUILD_AXIS)

    # Build **slab edges** EXACTLY (used for sets), independent of whether planes are creatable.
    # Always include AXIS_ZERO and the final top (clamped).
    slab_edges = []
    n_edges = int(math.ceil(H / LAYER_THK))  # number of layer intervals
    for i in range(0, n_edges + 1):
        slab_edges.append(AXIS_ZERO + i * LAYER_THK)
    # ensure exact top is included (AXIS_ZERO + H), even if it coincides with the last point
    top_edge = AXIS_ZERO + H
    if abs(slab_edges[-1] - top_edge) > TOL:
        slab_edges.append(top_edge)
    slab_edges = sorted(slab_edges)

    # Create datum planes for all **internal** edges strictly inside bbox (top & bottom may sit on outer faces)
    for b in slab_edges:
        if axis_min + EPS < b < axis_max - EPS:
            try:
                dp = p.DatumPlaneByPrincipalPlane(principalPlane=pp, offset=b)
                plane_ids.append(dp.id)
            except:
                pass  # ignore if CAE refuses (coincident with face, etc.)

    # ===================== STEP 2: PARTITION USING ALL PLANES ==================
    for pid in plane_ids:
        try:
            p.PartitionCellByDatumPlane(datumPlane=p.datums[pid], cells=p.cells)
        except:
            pass

    # ===================== STEP 3: CREATE ASSEMBLY + SETS ======================
    a = mdb.models['Model-1'].rootAssembly
    a.DatumCsysByDefault(CARTESIAN)
    inst = a.Instance(name='ImportedPart-1', part=p, dependent=ON)
    a.regenerate()

    # Instance bbox (verts->edges fallback)
    def _inst_bbox(inst_):
        xs=[]; ys=[]; zs=[]
        for v in inst_.vertices:
            x,y,z = v.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
        if not xs:
            for e in inst_.edges:
                x,y,z = e.pointOn[0]; xs.append(x); ys.append(y); zs.append(z)
        if not xs:
            return x0, x1, y0, y1, z0, z1
        return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

    ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst)
    span = max(ix1 - ix0, iy1 - iy0, iz1 - iz0)
    PAD  = max(1.0e-6, 0.05*span)

    def _bbox(axis, lo_val, hi_val):
        a_tag = (axis or "Y").upper()
        if a_tag == "X":
            return (lo_val, iy0 - PAD, iz0 - PAD, hi_val, iy1 + PAD, iz1 + PAD)
        if a_tag == "Z":
            return (ix0 - PAD, iy0 - PAD, lo_val, ix1 + PAD, iy1 + PAD, hi_val)
        return (ix0 - PAD, lo_val, iz0 - PAD, ix1 + PAD, hi_val, iz1 + PAD)  # Y

    # set-0: base (<= AXIS_ZERO) — small margin only for box selection robustness
    base_hi = AXIS_ZERO + MARGIN
    bb_base = _bbox(BUILD_AXIS, -1.0e99, base_hi)
    base_cells = inst.cells.getByBoundingBox(*bb_base)
    if len(base_cells):
        a.Set(cells=base_cells, name='set-0')

    # Per-layer sets from **consecutive slab edges** (guarantees last top layer)
    layer_index = 1
    for i in range(len(slab_edges) - 1):
        lo_edge = slab_edges[i]
        hi_edge = slab_edges[i + 1]
        lo = min(lo_edge, hi_edge) - MARGIN
        hi = max(lo_edge, hi_edge) + MARGIN
        bb_k = _bbox(BUILD_AXIS, lo, hi)
        cells_k = inst.cells.getByBoundingBox(*bb_k)
        if len(cells_k):
            a.Set(cells=cells_k, name='set-%d' % layer_index)
            layer_index += 1
        else:
            # widen a bit if extremely thin
            bb_k2 = _bbox(BUILD_AXIS, lo - 5.0*MARGIN, hi + 5.0*MARGIN)
            cells_k2 = inst.cells.getByBoundingBox(*bb_k2)
            if len(cells_k2):
                a.Set(cells=cells_k2, name='set-%d' % layer_index)
                layer_index += 1
            else:
                print("Warning: empty selection for set-%d (%.9f..%.9f)" % (layer_index, lo_edge, hi_edge))

    n_layers_created = layer_index - 1

    # set-(N+1): whole build
    bb_all = _bbox(BUILD_AXIS, min(slab_edges) - MARGIN, max(slab_edges) + MARGIN)
    all_build = inst.cells.getByBoundingBox(*bb_all)
    if len(all_build):
        a.Set(cells=all_build, name='set-%d' % (n_layers_created + 1))

    # ---------------- Part-level sets (only BASE & BUILD_ALL) ----------------
    def _bbox_p(axis, lo_val, hi_val):
        a_tag = (axis or "Y").upper()
        if a_tag == "X":
            return (lo_val, y0 - TOL, z0 - TOL, hi_val, y1 + TOL, z1 + TOL)
        if a_tag == "Z":
            return (x0 - TOL, y0 - TOL, lo_val, x1 + TOL, y1 + TOL, hi_val)
        return (x0 - TOL, lo_val, z0 - TOL, x1 + TOL, hi_val, z1 + TOL)  # Y

    base_hi_p = AXIS_ZERO + MARGIN
    base_cells_p = p.cells.getByBoundingBox(*_bbox_p(BUILD_AXIS, -1.0e99, base_hi_p))
    if len(base_cells_p):
        p.Set(cells=base_cells_p, name='BASE')

    build_all_p = p.cells.getByBoundingBox(*_bbox_p(BUILD_AXIS, min(slab_edges) - MARGIN, max(slab_edges) + MARGIN))
    if len(build_all_p):
        p.Set(cells=build_all_p, name='BUILD_ALL')

    # Save CAE
    try:
        mdb.saveAs(SAVE_AS)
    except:
        try:
            mdb.save()
        except:
            pass

    print('Import+partition done (axis=%s, zero=%.6g). Layers created: %d. CAE=%s'
          % (BUILD_AXIS, AXIS_ZERO, n_layers_created, SAVE_AS))

if __name__ == '__main__':
    main()
