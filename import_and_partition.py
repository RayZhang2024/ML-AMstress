# # # -*- coding: utf-8 -*-
# # # Import CAD (STEP/IGES/SAT), partition along Y from Y_ZERO, create sets.
# # # NOTE: This file is patched by the GUI before execution.

# # from abaqus import *
# # from abaqusConstants import *
# # from caeModules import *
# # import os, math

# # # ======= Patched constants (GUI will overwrite these lines) =======
# # CAD_FILE   = r""           # ASCII-safe path to CAD file
# # SCALE      = 1.0
# # LAYER_THK  = 0.5
# # BUILD_H    = 10.0          # total build height above Y_ZERO; <=0 means use model extent
# # Y_ZERO     = 0.0           # start slicing from this absolute Y (usually 0.0)
# # SAVE_AS    = r""           # ASCII-safe path to output .cae
# # TOL        = 1.0e-9
# # # ================================================================

# # def main():
# #     Mdb()

# #     # Choose importer by extension
# #     ext = os.path.splitext(CAD_FILE)[1].lower()
# #     if ext in ('.stp', '.step'):
# #         geom = mdb.openStep(CAD_FILE, scale=SCALE)
# #     elif ext in ('.igs', '.iges'):
# #         geom = mdb.openIges(CAD_FILE, topology=SOLID)
# #     elif ext == '.sat':
# #         geom = mdb.openAcis(CAD_FILE)
# #     else:
# #         raise RuntimeError('Unsupported CAD extension: %s' % ext)

# #     # Create 3D deformable part
# #     p = mdb.models['Model-1'].PartFromGeometryFile(
# #         name='ImportedPart',
# #         geometryFile=geom,
# #         dimensionality=THREE_D,
# #         type=DEFORMABLE_BODY
# #     )

# #     # Bounding box via vertices (fallback to edges)
# #     xs=[]; ys=[]; zs=[]
# #     for v in p.vertices:
# #         pt = v.pointOn[0]
# #         xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
# #     if len(xs) == 0:
# #         for e in p.edges:
# #             pt = e.pointOn[0]
# #             xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
# #     if len(xs) == 0:
# #         raise RuntimeError('Cannot compute bounding box (no vertices/edges)')
# #     x0=min(xs); x1=max(xs); y0=min(ys); y1=max(ys); z0=min(zs); z1=max(zs)

# #     # Determine build height H from Y_ZERO upward
# #     if y1 <= Y_ZERO + TOL:
# #         raise RuntimeError('No geometry above Y_ZERO to slice')
# #     y_top_limit = (Y_ZERO + BUILD_H) if BUILD_H > 0.0 else y1
# #     y_top = min(y_top_limit, y1)
# #     H = max(0.0, y_top - Y_ZERO)
# #     if H <= 0.0:
# #         raise RuntimeError('Invalid slicing height (H<=0)')

# #     # Layers count
# #     n_layers = int(math.ceil(H / max(LAYER_THK, 1.0e-12)))

# #     # Require solid cells
# #     if len(p.cells) == 0:
# #         raise RuntimeError('No solid cells found (surface/wire geometry?)')

# #     # First split at Y_ZERO to isolate base
# #     d0 = p.DatumPlaneByPrincipalPlane(principalPlane=XZPLANE, offset=Y_ZERO)
# #     p.PartitionCellByDatumPlane(datumPlane=p.datums[d0.id], cells=p.cells)

# #     # Slice planes along Y
# #     for i in range(1, n_layers):
# #         y = Y_ZERO + i * LAYER_THK
# #         if y >= Y_ZERO + H - TOL:
# #             break
# #         d = p.DatumPlaneByPrincipalPlane(principalPlane=XZPLANE, offset=y)
# #         p.PartitionCellByDatumPlane(datumPlane=p.datums[d.id], cells=p.cells)

# #     # Part-level layer sets (LAYER_001 ...)
# #     for k in range(n_layers):
# #         y_lo = Y_ZERO + k * LAYER_THK
# #         y_hi = min(Y_ZERO + (k+1) * LAYER_THK, Y_ZERO + H)
# #         layer_cells = p.cells.getByBoundingBox(x0, y_lo - TOL, z0, x1, y_hi + TOL, z1)
# #         if len(layer_cells):
# #             p.Set(cells=layer_cells, name='LAYER_%03d' % (k+1))

# #     # Assembly instance and sets
# #     a = mdb.models['Model-1'].rootAssembly
# #     a.DatumCsysByDefault(CARTESIAN)
# #     inst = a.Instance(name='ImportedPart-1', part=p, dependent=ON)
# #     a.regenerate()

# #     # set-1..set-n_layers: each deposited layer
# #     for k in range(n_layers):
# #         y_lo = Y_ZERO + k * LAYER_THK
# #         y_hi = min(Y_ZERO + (k+1) * LAYER_THK, Y_ZERO + H)
# #         cells_k = inst.cells.getByBoundingBox(x0, y_lo - TOL, z0, x1, y_hi + TOL, z1)
# #         if len(cells_k):
# #             a.Set(cells=cells_k, name='set-%d' % (k+1))

# #     # --- recompute bbox on the INSTANCE, not the part ---
# #     def _inst_bbox(inst_):
# #         xs=[]; ys=[]; zs=[]
# #         for v in inst_.vertices:
# #             pt = v.pointOn[0]
# #             xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
# #         if not xs:
# #             for e in inst_.edges:
# #                 pt = e.pointOn[0]
# #                 xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
# #         if not xs:
# #             # fall back to part bbox we computed earlier
# #             return x0, x1, y0, y1, z0, z1
# #         return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

# #     a.regenerate()
# #     ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst)
# #     span = max(ix1 - ix0, iy1 - iy0, iz1 - iz0)
# #     PAD  = max(1.0e-6, 0.05*span)   # 5% of model size (much larger than TOL)

# #     # set-0: base (Y <= Y_ZERO) — big X/Z padding + inclusive Y upper bound
# #     base_cells = inst.cells.getByBoundingBox(
# #         ix0 - PAD, iy0 - PAD, iz0 - PAD,
# #         ix1 + PAD, Y_ZERO + TOL, iz1 + PAD
# #     )
# #     print('[debug] base_cells count:', len(base_cells))
# #     if len(base_cells):
# #         a.Set(cells=base_cells, name='set-0')
# #     else:
# #         print('[warn] set-0 selection empty: Y_ZERO=%.6g' % Y_ZERO)

# #     # set-(n_layers+1): whole build region [Y_ZERO, Y_ZERO+H] — padded
# #     all_build = inst.cells.getByBoundingBox(
# #         ix0 - PAD, Y_ZERO - TOL, iz0 - PAD,
# #         ix1 + PAD, Y_ZERO + H + TOL, iz1 + PAD
# #     )
# #     print('[debug] build_all cells count:', len(all_build))
# #     if len(all_build):
# #         a.Set(cells=all_build, name='set-%d' % (n_layers + 1))
# #     else:
# #         print('[warn] build_all selection empty: Y_ZERO=%.6g, H=%.6g' % (Y_ZERO, H))


# #     base_cells_p = p.cells.getByBoundingBox(
# #         x0 - TOL, y0 - 1.0, z0 - TOL,
# #         x1 + TOL, Y_ZERO + TOL, z1 + TOL
# #     )
# #     if len(base_cells_p):
# #         p.Set(cells=base_cells_p, name='BASE')

# #     build_all_p = p.cells.getByBoundingBox(
# #         x0 - TOL, Y_ZERO - TOL, z0 - TOL,
# #         x1 + TOL, Y_ZERO + H + TOL, z1 + TOL
# #     )
# #     if len(build_all_p):
# #         p.Set(cells=build_all_p, name='BUILD_ALL')

# #     # Save CAE
# #     try:
# #         mdb.saveAs(SAVE_AS)
# #     except:
# #         try:
# #             mdb.save()
# #         except:
# #             pass

# #     print('Import+partition done: %d layers, CAE=%s' % (n_layers, SAVE_AS))

# # if __name__ == '__main__':
# #     main()


# # -*- coding: utf-8 -*-
# # Import CAD (STEP/IGES/SAT), partition along Y from Y_ZERO, create only BASE and BUILD_ALL sets.
# # NOTE: This file is patched by the GUI before execution.

# from abaqus import *
# from abaqusConstants import *
# from caeModules import *
# import os, math

# # ======= Patched constants (GUI will overwrite these lines) =======
# CAD_FILE   = r""           # ASCII-safe path to CAD file
# SCALE      = 1.0
# LAYER_THK  = 0.5
# BUILD_H    = 10.0          # total build height above Y_ZERO; <=0 means use model extent
# Y_ZERO     = 0.0           # start slicing from this absolute Y (usually 0.0)
# SAVE_AS    = r""           # ASCII-safe path to output .cae
# TOL        = 1.0e-9
# # ================================================================

# def main():
#     Mdb()

#     # Choose importer by extension
#     ext = os.path.splitext(CAD_FILE)[1].lower()
#     if ext in ('.stp', '.step'):
#         geom = mdb.openStep(CAD_FILE, scale=SCALE)
#     elif ext in ('.igs', '.iges'):
#         geom = mdb.openIges(CAD_FILE, topology=SOLID)
#     elif ext == '.sat':
#         geom = mdb.openAcis(CAD_FILE)
#     else:
#         raise RuntimeError('Unsupported CAD extension: %s' % ext)

#     # Create 3D deformable part
#     p = mdb.models['Model-1'].PartFromGeometryFile(
#         name='ImportedPart',
#         geometryFile=geom,
#         dimensionality=THREE_D,
#         type=DEFORMABLE_BODY
#     )

#     # Bounding box via vertices (fallback to edges)
#     xs=[]; ys=[]; zs=[]
#     for v in p.vertices:
#         pt = v.pointOn[0]
#         xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
#     if not xs:
#         for e in p.edges:
#             pt = e.pointOn[0]
#             xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
#     if not xs:
#         raise RuntimeError('Cannot compute bounding box (no vertices/edges)')
#     x0=min(xs); x1=max(xs); y0=min(ys); y1=max(ys); z0=min(zs); z1=max(zs)

#     # Determine build height H from Y_ZERO upward
#     if y1 <= Y_ZERO + TOL:
#         raise RuntimeError('No geometry above Y_ZERO to slice')
#     y_top_limit = (Y_ZERO + BUILD_H) if BUILD_H > 0.0 else y1
#     y_top = min(y_top_limit, y1)
#     H = max(0.0, y_top - Y_ZERO)
#     if H <= 0.0:
#         raise RuntimeError('Invalid slicing height (H<=0)')

#     # Require solid cells
#     if len(p.cells) == 0:
#         raise RuntimeError('No solid cells found (surface/wire geometry?)')

#     # First split at Y_ZERO to isolate base
#     d0 = p.DatumPlaneByPrincipalPlane(principalPlane=XZPLANE, offset=Y_ZERO)
#     p.PartitionCellByDatumPlane(datumPlane=p.datums[d0.id], cells=p.cells)

#     # Create assembly instance
#     a = mdb.models['Model-1'].rootAssembly
#     a.DatumCsysByDefault(CARTESIAN)
#     inst = a.Instance(name='ImportedPart-1', part=p, dependent=ON)
#     a.regenerate()

#     # --- recompute bbox on the INSTANCE ---
#     def _inst_bbox(inst_):
#         xs=[]; ys=[]; zs=[]
#         for v in inst_.vertices:
#             pt = v.pointOn[0]
#             xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
#         if not xs:
#             for e in inst_.edges:
#                 pt = e.pointOn[0]
#                 xs.append(pt[0]); ys.append(pt[1]); zs.append(pt[2])
#         if not xs:
#             return x0, x1, y0, y1, z0, z1
#         return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

#     a.regenerate()
#     ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst)
#     span = max(ix1 - ix0, iy1 - iy0, iz1 - iz0)
#     PAD  = max(1.0e-6, 0.05*span)

#     # Assembly set-0: base (Y <= Y_ZERO)
#     base_cells = inst.cells.getByBoundingBox(
#         ix0 - PAD, iy0 - PAD, iz0 - PAD,
#         ix1 + PAD, Y_ZERO + TOL, iz1 + PAD
#     )
#     if len(base_cells):
#         a.Set(cells=base_cells, name='set-0')

#     # Assembly set-1: whole build region [Y_ZERO, Y_ZERO+H]
#     all_build = inst.cells.getByBoundingBox(
#         ix0 - PAD, Y_ZERO - TOL, iz0 - PAD,
#         ix1 + PAD, Y_ZERO + H + TOL, iz1 + PAD
#     )
#     if len(all_build):
#         a.Set(cells=all_build, name='set-1')

#     # Part-level sets: BASE and BUILD_ALL only
#     base_cells_p = p.cells.getByBoundingBox(
#         x0 - TOL, y0 - 1.0, z0 - TOL,
#         x1 + TOL, Y_ZERO + TOL, z1 + TOL
#     )
#     if len(base_cells_p):
#         p.Set(cells=base_cells_p, name='BASE')

#     build_all_p = p.cells.getByBoundingBox(
#         x0 - TOL, Y_ZERO - TOL, z0 - TOL,
#         x1 + TOL, Y_ZERO + H + TOL, z1 + TOL
#     )
#     if len(build_all_p):
#         p.Set(cells=build_all_p, name='BUILD_ALL')

#     # Save CAE
#     try:
#         mdb.saveAs(SAVE_AS)
#     except:
#         try:
#             mdb.save()
#         except:
#             pass

#     print('Import+partition done: BASE/BUILD_ALL sets created, CAE=%s' % SAVE_AS)

# if __name__ == '__main__':
#     main()


# -*- coding: utf-8 -*-
# Import CAD (STEP/IGES/SAT), partition along Y from Y_ZERO,
# create ONLY 'BASE' and 'BUILD_ALL' sets on the PART (no LAYER_xxx).
# Assembly sets remain: set-0 (base), set-1..set-N (layers), set-(N+1) (whole build).

from abaqus import *
from abaqusConstants import *
from caeModules import *
import os, math

# ======= Patched constants (GUI will overwrite these lines) =======
CAD_FILE   = r""
SCALE      = 1.0
LAYER_THK  = 0.5
BUILD_H    = 10.0          # total build height above Y_ZERO; <=0 means use model extent
Y_ZERO     = 0.0           # start slicing from this absolute Y (usually 0.0)
SAVE_AS    = r""
TOL        = 1.0e-9
# ================================================================

def main():
    Mdb()

    # Import geometry
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

    # Part bbox (verts→edges fallback)
    xs=[]; ys=[]; zs=[]
    for v in p.vertices:
        x,y,z = v.pointOn[0]
        xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        for e in p.edges:
            x,y,z = e.pointOn[0]
            xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        raise RuntimeError('Cannot compute bounding box (no vertices/edges)')
    x0,x1,y0,y1,z0,z1 = min(xs),max(xs),min(ys),max(ys),min(zs),max(zs)

    # Build height from Y_ZERO upward
    if y1 <= Y_ZERO + TOL:
        raise RuntimeError('No geometry above Y_ZERO to slice')
    y_top_limit = (Y_ZERO + BUILD_H) if BUILD_H > 0.0 else y1
    y_top = min(y_top_limit, y1)
    H = max(0.0, y_top - Y_ZERO)
    if H <= 0.0:
        raise RuntimeError('Invalid slicing height (H<=0)')

    # Require solid cells
    if len(p.cells) == 0:
        raise RuntimeError('No solid cells found (surface/wire geometry?)')

    # ---------------- Partitioning ----------------
    # Split at Y_ZERO (base vs build)
    d0 = p.DatumPlaneByPrincipalPlane(principalPlane=XZPLANE, offset=Y_ZERO)
    p.PartitionCellByDatumPlane(datumPlane=p.datums[d0.id], cells=p.cells)

    # CHANGED: keep per-layer datum planes & partitions (so layers exist),
    # but do NOT create LAYER_xxx sets on the PART.
    n_layers = int(math.ceil(H / max(LAYER_THK, 1.0e-12)))
    for i in range(1, n_layers):
        y = Y_ZERO + i * LAYER_THK
        if y >= Y_ZERO + H - TOL:
            break
        d = p.DatumPlaneByPrincipalPlane(principalPlane=XZPLANE, offset=y)
        p.PartitionCellByDatumPlane(datumPlane=p.datums[d.id], cells=p.cells)

    # ---------------- Assembly & sets ----------------
    a = mdb.models['Model-1'].rootAssembly
    a.DatumCsysByDefault(CARTESIAN)
    inst = a.Instance(name='ImportedPart-1', part=p, dependent=ON)
    a.regenerate()

    def _inst_bbox(inst_):
        xs=[]; ys=[]; zs=[]
        for v in inst_.vertices:
            x,y,z = v.pointOn[0]
            xs.append(x); ys.append(y); zs.append(z)
        if not xs:
            for e in inst_.edges:
                x,y,z = e.pointOn[0]
                xs.append(x); ys.append(y); zs.append(z)
        if not xs:
            return x0, x1, y0, y1, z0, z1
        return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

    ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst)
    span = max(ix1 - ix0, iy1 - iy0, iz1 - iz0)
    PAD  = max(1.0e-6, 0.05*span)

    # Assembly set-0: base (Y <= Y_ZERO)
    base_cells = inst.cells.getByBoundingBox(ix0 - PAD, iy0 - PAD, iz0 - PAD,
                                             ix1 + PAD, Y_ZERO + TOL, iz1 + PAD)
    if len(base_cells):
        a.Set(cells=base_cells, name='set-0')

    # Assembly per-layer sets: set-1..set-n_layers  (UNCHANGED — needed for activation)
    for k in range(n_layers):
        y_lo = Y_ZERO + k * LAYER_THK
        y_hi = min(Y_ZERO + (k+1) * LAYER_THK, Y_ZERO + H)
        cells_k = inst.cells.getByBoundingBox(ix0 - PAD, y_lo - TOL, iz0 - PAD,
                                              ix1 + PAD, y_hi + TOL, iz1 + PAD)
        if len(cells_k):
            a.Set(cells=cells_k, name='set-%d' % (k+1))

    # Assembly whole build: set-(n_layers+1)
    all_build = inst.cells.getByBoundingBox(ix0 - PAD, Y_ZERO - TOL, iz0 - PAD,
                                            ix1 + PAD, Y_ZERO + H + TOL, iz1 + PAD)
    if len(all_build):
        a.Set(cells=all_build, name='set-%d' % (n_layers + 1))

    # ---------------- Part-level sets (only BASE & BUILD_ALL) ----------------
    # REMOVED: creation of LAYER_### sets on the PART.
    base_cells_p = p.cells.getByBoundingBox(x0 - TOL, y0 - 1.0, z0 - TOL,
                                            x1 + TOL, Y_ZERO + TOL, z1 + TOL)
    if len(base_cells_p):
        p.Set(cells=base_cells_p, name='BASE')

    build_all_p = p.cells.getByBoundingBox(x0 - TOL, Y_ZERO - TOL, z0 - TOL,
                                           x1 + TOL, Y_ZERO + H + TOL, z1 + TOL)
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

    print('Import+partition done: per-layer partitions kept; PART sets = BASE/BUILD_ALL only; CAE=%s' % (SAVE_AS,))

if __name__ == '__main__':
    main()
