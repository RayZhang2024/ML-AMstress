# # -*- coding: utf-8 -*-
# # Apply minimal anti-rigid-body-motion boundary conditions on the meshed model.
# # BC-1: 2 bottom nodes, same (Y,Z), different X -> U1=0
# # BC-2: 4 bottom nodes (not collinear), spread in XZ -> U2=0
# # BC-3: 2 bottom nodes, same (Y,X), different Z -> U3=0
# # All created at Step-1 and propagate to the end.

# from abaqus import *
# from abaqusConstants import *
# from caeModules import *
# import math

# # ======== Patched constants (overwrite from GUI if desired) ========
# CAE_FILE       = r""          # e.g. r"C:/path/model_imported.cae"
# MODEL_NAME     = "Model-1"
# INSTANCE_NAME  = "ImportedPart-1"
# # Mesh-based tolerances (can be overridden by GUI patching)
# BASE_SEED      = 1.0          # typical edge seed in base (mm)
# BUILD_SEED     = 0.5          # typical edge seed in build (mm)
# LAYER_THK      = 0.5          # layer thickness (mm), used as a fallback tol
# # Safety factors for tolerance windows
# Y_TOL_FACTOR   = 0.35         # ~ one third of the smaller seed
# XZ_TOL_FACTOR  = 0.20         # ~ one fifth of the span for picking lines
# CORNER_MARGIN  = 0.12         # box half-width as fraction of X/Z span for corners
# MAX_RELAX_TRY  = 4            # progressively relax tolerances this many times
# # ================================================================

# def _median(vals):
#     if not vals:
#         return 0.0
#     s = sorted(vals)
#     n = len(s)
#     m = n // 2
#     if n % 2:
#         return s[m]
#     else:
#         return 0.5 * (s[m-1] + s[m])

# def _inst_bbox(inst):
#     xs=[]; ys=[]; zs=[]
#     for n in inst.nodes:
#         x,y,z = n.coordinates
#         xs.append(x); ys.append(y); zs.append(z)
#     if not xs:
#         raise RuntimeError("No mesh nodes found on instance '%s'." % inst.name)
#     return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

# def _collect_bottom_nodes(inst, tol_y):
#     # Pick nodes with Y near the global minimum Y (bottom of the base plate)
#     y_min = min(n.coordinates[1] for n in inst.nodes)
#     nodes = [n for n in inst.nodes if abs(n.coordinates[1] - y_min) <= tol_y]
#     return nodes, y_min

# def _labels(nodes):
#     return [n.label for n in nodes]

# def _pick_bc1(inst, bottom_nodes, y_min, x_tol, y_tol):
#     # BC-1: two nodes with same (Y, X) ~ bottom & X≈median, different Z (min-Z, max-Z)
#     if not bottom_nodes:
#         return []
#     x_vals = [n.coordinates[0] for n in bottom_nodes]
#     x0 = _median(x_vals)
#     band = [n for n in bottom_nodes
#             if (abs(n.coordinates[1] - y_min) <= y_tol and abs(n.coordinates[0] - x0) <= x_tol)]
#     if len(band) < 2:
#         return []
#     band_sorted = sorted(band, key=lambda n: n.coordinates[2])  # by Z
#     return [band_sorted[0], band_sorted[-1]]


# def _pick_bc3(inst, bottom_nodes, y_min, z_tol, y_tol):
#     # BC-3: two nodes with same (Y, Z) ~ bottom & Z≈median, different X (min-X, max-X)
#     if not bottom_nodes:
#         return []
#     z_vals = [n.coordinates[2] for n in bottom_nodes]
#     z0 = _median(z_vals)
#     band = [n for n in bottom_nodes
#             if (abs(n.coordinates[1] - y_min) <= y_tol and abs(n.coordinates[2] - z0) <= z_tol)]
#     if len(band) < 2:
#         return []
#     band_sorted = sorted(band, key=lambda n: n.coordinates[0])  # by X
#     return [band_sorted[0], band_sorted[-1]]

# def _pick_bc2_corners(inst, bottom_nodes, x0, x1, z0, z1, margin_frac):
#     # Four nodes near XZ "corners" at bottom: (minX,minZ), (minX,maxZ), (maxX,minZ), (maxX,maxZ)
#     # We tolerate a margin box around each corner and choose the closest node.
#     if not bottom_nodes:
#         return []
#     dx = max(x1 - x0, 1e-12)
#     dz = max(z1 - z0, 1e-12)
#     mx = margin_frac * dx
#     mz = margin_frac * dz
#     corners = [(x0, z0), (x0, z1), (x1, z0), (x1, z1)]

#     chosen = []
#     chosen_labels = set()

#     for (cx, cz) in corners:
#         best = None
#         best_d2 = 1e99
#         for n in bottom_nodes:
#             xn, yn, zn = n.coordinates
#             if (abs(xn - cx) <= mx) and (abs(zn - cz) <= mz):
#                 d2 = (xn - cx)*(xn - cx) + (zn - cz)*(zn - cz)
#                 if d2 < best_d2 and (n.label not in chosen_labels):
#                     best = n; best_d2 = d2
#         if best is not None:
#             chosen.append(best)
#             chosen_labels.add(best.label)

#     # If fewer than 4 found (narrow geometry), augment with extremes in XZ
#     if len(chosen) < 4 and bottom_nodes:
#         # Fill with farthest-from-centroid nodes not yet chosen
#         cx = sum(n.coordinates[0] for n in bottom_nodes)/float(len(bottom_nodes))
#         cz = sum(n.coordinates[2] for n in bottom_nodes)/float(len(bottom_nodes))
#         pool = [n for n in bottom_nodes if n.label not in chosen_labels]
#         pool.sort(key=lambda n: (n.coordinates[0]-cx)**2 + (n.coordinates[2]-cz)**2, reverse=True)
#         for n in pool:
#             chosen.append(n)
#             chosen_labels.add(n.label)
#             if len(chosen) >= 4:
#                 break
#     # Ensure unique nodes
#     unique = []
#     seen = set()
#     for n in chosen:
#         if n.label not in seen:
#             unique.append(n)
#             seen.add(n.label)
#     return unique[:4]

# def _ensure_assembly_set_from_labels(a, set_name, inst_name, labels):
#     if not labels:
#         return None
#     # Create or replace assembly node set from labels
#     if set_name in a.sets.keys():
#         try:
#             del a.sets[set_name]
#         except:
#             pass
#     a.SetFromNodeLabels(name=set_name, nodeLabels=((inst_name, tuple(labels)),))
#     return a.sets[set_name]

# def main():
#     # Open model
#     if CAE_FILE:
#         openMdb(pathName=CAE_FILE)
#     m = mdb.models[MODEL_NAME]
#     a = m.rootAssembly
    
#     def _pick_instance_with_nodes(assembly, preferred_name=None):
#         assembly.regenerate()
#         a.regenerate()

#         # 先用首选名
#         if preferred_name and preferred_name in assembly.instances.keys():
#             inst = assembly.instances[preferred_name]
#             assembly.regenerate()
#             if len(inst.nodes) > 0:
#                 print("[BC] using preferred instance:", preferred_name)
#                 return inst
#             else:
#                 print("[BC] preferred instance has no nodes:", preferred_name)
#         # 否则挑第一个有节点的实例
#         for nm, inst in assembly.instances.items():
#             assembly.regenerate()
#             if len(inst.nodes) > 0:
#                 print("[BC] auto-picked instance with nodes:", nm)
#                 return inst
#         return None
    
#     inst = _pick_instance_with_nodes(a, preferred_name=INSTANCE_NAME)
#     if inst is None:
#         # 给出更清晰的提示并安全返回（不抛异常也行）
#         print("[BC][error] No mesh nodes found on ANY instance. Did meshing finish and save?")
#         raise RuntimeError("No mesh nodes on any instance; aborting BC creation.")
    
#     # if INSTANCE_NAME in a.instances.keys():
#     #     inst = a.instances[INSTANCE_NAME]
#     # else:
#     #     # fallback to the first instance
#     #     if len(a.instances.keys()) == 0:
#     #         raise RuntimeError("No instances in assembly.")
#     #     inst = a.instances.values()[0]

#     # Basic spans and tolerances
#     ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst)
#     dx = max(ix1 - ix0, 1e-12)
#     dz = max(iz1 - iz0, 1e-12)
#     base_build_min = max(1e-6, min(float(BASE_SEED or 0.0), float(BUILD_SEED or 0.0)) or 0.5)
#     y_tol0  = max(1e-6, Y_TOL_FACTOR  * base_build_min)
#     x_tol0  = max(1e-6, XZ_TOL_FACTOR * dx)
#     z_tol0  = max(1e-6, XZ_TOL_FACTOR * dz)

#     # Gather bottom nodes with adaptive Y tolerance
#     bottom = []
#     y_min = None
#     y_tol = y_tol0
#     for k in range(MAX_RELAX_TRY):
#         bottom, y_min = _collect_bottom_nodes(inst, y_tol)
#         if len(bottom) >= 6:  # enough to pick 2+4+2 with some leeway
#             break
#         y_tol *= 1.6  # relax
#     if not bottom:
#         raise RuntimeError("No bottom nodes found (check mesh).")

#     # ---- Pick BC-1 (U1): same (Y,X), different Z ----
#     bc1_nodes = []
#     x_tol = x_tol0
#     for _ in range(MAX_RELAX_TRY):
#         bc1_nodes = _pick_bc1(inst, bottom, y_min, x_tol, y_tol)
#         if len(bc1_nodes) >= 2:
#             break
#         x_tol *= 1.5
#     bc1_labels = _labels(bc1_nodes[:2])
    
#     # ---- Pick BC-3 (U3): same (Y,Z), different X ----
#     bc3_nodes = []
#     z_tol = z_tol0
#     for _ in range(MAX_RELAX_TRY):
#         bc3_nodes = _pick_bc3(inst, bottom, y_min, z_tol, y_tol)
#         if len(bc3_nodes) >= 2:
#             break
#         z_tol *= 1.5
#     bc3_labels = _labels(bc3_nodes[:2])

#     # ---- Pick BC-2 (U2): four bottom nodes (not collinear) ----
#     bc2_nodes = _pick_bc2_corners(inst, bottom, ix0, ix1, iz0, iz1, CORNER_MARGIN)
#     bc2_labels = _labels(bc2_nodes)

#     print("[BC] bottom nodes: %d" % len(bottom))
#     print("[BC] BC-1 labels (U1):", bc1_labels)
#     print("[BC] BC-2 labels (U2):", bc2_labels)
#     print("[BC] BC-3 labels (U3):", bc3_labels)

#     # Create assembly sets
#     set1 = _ensure_assembly_set_from_labels(a, 'BC1_pts', inst.name, bc1_labels)
#     set2 = _ensure_assembly_set_from_labels(a, 'BC2_pts', inst.name, bc2_labels)
#     set3 = _ensure_assembly_set_from_labels(a, 'BC3_pts', inst.name, bc3_labels)

#     # Ensure Step-1 exists
#     if 'Step-1' not in m.steps.keys():
#         # Minimal safeguard: create a tiny static step so BCs have a valid step
#         m.StaticStep(name='Step-1', previous='Initial', timePeriod=1.0)

#     # Create/update BCs (propagate automatically to subsequent steps)
#     # BC-1: U1
#     if set1 is not None:
#         if 'BC-1' in m.boundaryConditions.keys():
#             m.boundaryConditions['BC-1'].setValues(region=a.sets['BC1_pts'], u1=0.0, u2=UNSET, u3=UNSET)
#         else:
#             m.DisplacementBC(name='BC-1', createStepName='Step-1',
#                              region=a.sets['BC1_pts'], u1=0.0, u2=UNSET, u3=UNSET,
#                              ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
#                              distributionType=UNIFORM, fieldName='', localCsys=None)

#     # BC-2: U2
#     if set2 is not None:
#         if 'BC-2' in m.boundaryConditions.keys():
#             m.boundaryConditions['BC-2'].setValues(region=a.sets['BC2_pts'], u1=UNSET, u2=0.0, u3=UNSET)
#         else:
#             m.DisplacementBC(name='BC-2', createStepName='Step-1',
#                              region=a.sets['BC2_pts'], u1=UNSET, u2=0.0, u3=UNSET,
#                              ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
#                              distributionType=UNIFORM, fieldName='', localCsys=None)

#     # BC-3: U3
#     if set3 is not None:
#         if 'BC-3' in m.boundaryConditions.keys():
#             m.boundaryConditions['BC-3'].setValues(region=a.sets['BC3_pts'], u1=UNSET, u2=UNSET, u3=0.0)
#         else:
#             m.DisplacementBC(name='BC-3', createStepName='Step-1',
#                              region=a.sets['BC3_pts'], u1=UNSET, u2=UNSET, u3=0.0,
#                              ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
#                              distributionType=UNIFORM, fieldName='', localCsys=None)

#     try:
#         mdb.save()
#     except:
#         pass

#     print("[BC] Boundary conditions created/updated and saved.")

# if __name__ == "__main__":
#     main()


# -*- coding: utf-8 -*-
# Apply minimal anti-rigid-body boundary conditions (BC-1/BC-2/BC-3)
# and add predefined temperatures: (1) Initial global 25°C, (2) Step-1 USER_DEFINED (UTEMP).
#
# Run: abaqus cae noGUI=apply_boundary.py

from abaqus import *
from abaqusConstants import *
from caeModules import *
import math

# ======== Patched constants (GUI overwrites these lines) ========
CAE_FILE       = r""
MODEL_NAME     = "Model-1"
INSTANCE_NAME  = "ImportedPart-1"
BASE_SEED      = 1.0
BUILD_SEED     = 0.5
LAYER_THK      = 0.5
# Tuning factors
Y_TOL_FACTOR   = 0.35
XZ_TOL_FACTOR  = 0.20
CORNER_MARGIN  = 0.12
MAX_RELAX_TRY  = 4
# ================================================================

def _median(vals):
    if not vals:
        return 0.0
    s = sorted(vals)
    n = len(s)
    m = n // 2
    return s[m] if (n % 2) else 0.5*(s[m-1]+s[m])

def _inst_bbox(inst):
    xs=[]; ys=[]; zs=[]
    for n in inst.nodes:
        x,y,z = n.coordinates
        xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        raise RuntimeError("No mesh nodes found on instance '%s'." % inst.name)
    return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

def _collect_bottom_nodes(inst, tol_y):
    y_min = min(n.coordinates[1] for n in inst.nodes)
    nodes = [n for n in inst.nodes if abs(n.coordinates[1] - y_min) <= tol_y]
    return nodes, y_min

def _labels(nodes):
    return [n.label for n in nodes]

# ---- corrected helpers (swapped as per your spec) ----
def _pick_bc1(inst, bottom_nodes, y_min, x_tol, y_tol):
    # BC-1: same (Y, X) ~ X≈median, different Z -> pick min-Z & max-Z
    if not bottom_nodes:
        return []
    x_vals = [n.coordinates[0] for n in bottom_nodes]
    x0 = _median(x_vals)
    band = [n for n in bottom_nodes
            if (abs(n.coordinates[1] - y_min) <= y_tol and abs(n.coordinates[0] - x0) <= x_tol)]
    if len(band) < 2:
        return []
    band_sorted = sorted(band, key=lambda n: n.coordinates[2])  # by Z
    return [band_sorted[0], band_sorted[-1]]

def _pick_bc3(inst, bottom_nodes, y_min, z_tol, y_tol):
    # BC-3: same (Y, Z) ~ Z≈median, different X -> pick min-X & max-X
    if not bottom_nodes:
        return []
    z_vals = [n.coordinates[2] for n in bottom_nodes]
    z0 = _median(z_vals)
    band = [n for n in bottom_nodes
            if (abs(n.coordinates[1] - y_min) <= y_tol and abs(n.coordinates[2] - z0) <= z_tol)]
    if len(band) < 2:
        return []
    band_sorted = sorted(band, key=lambda n: n.coordinates[0])  # by X
    return [band_sorted[0], band_sorted[-1]]

def _pick_bc2_corners(inst, bottom_nodes, x0, x1, z0, z1, margin_frac):
    if not bottom_nodes:
        return []
    dx = max(x1 - x0, 1e-12)
    dz = max(z1 - z0, 1e-12)
    mx = margin_frac * dx
    mz = margin_frac * dz
    corners = [(x0, z0), (x0, z1), (x1, z0), (x1, z1)]
    chosen = []
    chosen_labels = set()
    for (cx, cz) in corners:
        best = None
        best_d2 = 1e99
        for n in bottom_nodes:
            xn, yn, zn = n.coordinates
            if (abs(xn - cx) <= mx) and (abs(zn - cz) <= mz):
                d2 = (xn - cx)*(xn - cx) + (zn - cz)*(zn - cz)
                if d2 < best_d2 and (n.label not in chosen_labels):
                    best = n; best_d2 = d2
        if best is not None:
            chosen.append(best)
            chosen_labels.add(best.label)
    if len(chosen) < 4 and bottom_nodes:
        cx = sum(n.coordinates[0] for n in bottom_nodes)/float(len(bottom_nodes))
        cz = sum(n.coordinates[2] for n in bottom_nodes)/float(len(bottom_nodes))
        pool = [n for n in bottom_nodes if n.label not in chosen_labels]
        pool.sort(key=lambda n: (n.coordinates[0]-cx)**2 + (n.coordinates[2]-cz)**2, reverse=True)
        for n in pool:
            chosen.append(n)
            chosen_labels.add(n.label)
            if len(chosen) >= 4:
                break
    unique = []
    seen = set()
    for n in chosen:
        if n.label not in seen:
            unique.append(n); seen.add(n.label)
    return unique[:4]

def _ensure_assembly_set_from_labels(a, set_name, inst_name, labels):
    if not labels:
        return None
    if set_name in a.sets.keys():
        try:
            del a.sets[set_name]
        except:
            pass
    a.SetFromNodeLabels(name=set_name, nodeLabels=((inst_name, tuple(labels)),))
    return a.sets[set_name]

def _pick_instance_with_nodes(assembly, preferred_name=None):
    assembly.regenerate()
    if preferred_name and preferred_name in assembly.instances.keys():
        inst = assembly.instances[preferred_name]
        assembly.regenerate()
        if len(inst.nodes) > 0:
            print("[BC] using preferred instance:", preferred_name)
            return inst
        else:
            print("[BC] preferred instance has no nodes:", preferred_name)
    for nm, inst in assembly.instances.items():
        assembly.regenerate()
        if len(inst.nodes) > 0:
            print("[BC] auto-picked instance with nodes:", nm)
            return inst
    return None

def _pick_any_instance(assembly, preferred_name=None):
    assembly.regenerate()
    if preferred_name and preferred_name in assembly.instances.keys():
        return assembly.instances[preferred_name]
    if len(assembly.instances.keys()) == 0:
        return None
    # first in dict order (Py2/CAE has .values() supporting index)
    return assembly.instances[assembly.instances.keys()[0]]

def _ensure_temp_all_set(a, inst, set_name='TEMP_ALL'):
    # Build an assembly set that contains ALL geometry of the instance
    # (vertices, edges, faces, cells) to use as a robust region for Temperature.
    if set_name in a.sets.keys():
        try:
            del a.sets[set_name]
        except:
            pass
    verts = inst.vertices
    edges = inst.edges
    faces = inst.faces
    cells = inst.cells
    a.Set(name=set_name, vertices=verts, edges=edges, faces=faces, cells=cells)
    return a.sets[set_name]

def main():
    # Open model
    if CAE_FILE:
        openMdb(pathName=CAE_FILE)
    m = mdb.models[MODEL_NAME]
    a = m.rootAssembly
    a.regenerate()

    # ---- pick instance for BC (requires mesh nodes) ----
    inst_bc = _pick_instance_with_nodes(a, preferred_name=INSTANCE_NAME)
    # ---- pick instance for temperature (can be without nodes) ----
    inst_temp = inst_bc if inst_bc is not None else _pick_any_instance(a, preferred_name=INSTANCE_NAME)

    # Ensure Step-1 exists for BC & Predefined fields
    if 'Step-1' not in m.steps.keys():
        m.StaticStep(name='Step-1', previous='Initial', timePeriod=1.0)

    # ===================== Predefined Temperatures =====================
    if inst_temp is not None:
        temp_set = _ensure_temp_all_set(a, inst_temp, set_name='TEMP_ALL')
    
        # Repo handle (Abaqus uses 'predefinedFields'; fall back just in case)
        pf = getattr(m, 'predefinedFields', None)
        if pf is None:
            pf = getattr(m, 'preDefinedFields', None)
        if pf is None:
            raise RuntimeError("Model has neither 'predefinedFields' nor 'preDefinedFields'.")
    
        # (1) Initial global 25°C (uniform)
        if 'Predefined Field-1' in pf.keys():
            pf['Predefined Field-1'].setValues(
                region=temp_set,
                distributionType=UNIFORM,
                crossSectionDistribution=CONSTANT_THROUGH_THICKNESS,
                magnitudes=(25.0,)
            )
        else:
            m.Temperature(
                name='Predefined Field-1',
                createStepName='Initial',
                region=temp_set,
                distributionType=UNIFORM,
                crossSectionDistribution=CONSTANT_THROUGH_THICKNESS,
                magnitudes=(25.0,)
            )
    
        # (2) User-defined temperature from Step-1 (UTEMP)
        if 'Predefined Field-2' in pf.keys():
            pf['Predefined Field-2'].setValues(
                region=temp_set,
                distributionType=USER_DEFINED
            )
        else:
            m.Temperature(
                name='Predefined Field-2',
                createStepName='Step-1',
                region=temp_set,
                distributionType=USER_DEFINED
            )
    
        print("[TEMP] Predefined temperatures applied on assembly set 'TEMP_ALL'.")
    else:
        print("[TEMP][warn] No instance found to apply temperature fields.")

    # ===================== Boundary Conditions (requires mesh) =====================
    if inst_bc is None:
        print("[BC][warn] No instance with mesh nodes; skip BC creation.")
        try:
            mdb.save()
        except:
            pass
        return

    # Metrics & tolerances for bottom-node picking
    ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst_bc)
    dx = max(ix1 - ix0, 1e-12)
    dz = max(iz1 - iz0, 1e-12)
    base_build_min = max(1e-6, min(float(BASE_SEED or 0.0), float(BUILD_SEED or 0.0)) or 0.5)
    y_tol0  = max(1e-6, Y_TOL_FACTOR  * base_build_min)
    x_tol0  = max(1e-6, XZ_TOL_FACTOR * dx)
    z_tol0  = max(1e-6, XZ_TOL_FACTOR * dz)

    bottom = []
    y_min = None
    y_tol = y_tol0
    for _ in range(MAX_RELAX_TRY):
        bottom, y_min = _collect_bottom_nodes(inst_bc, y_tol)
        if len(bottom) >= 6:
            break
        y_tol *= 1.6
    if not bottom:
        print("[BC][warn] No bottom nodes found; skip BC creation.")
        try:
            mdb.save()
        except:
            pass
        return

    # ---- Pick BC-1 (U1): same (Y,X), different Z ----
    bc1_nodes = []
    x_tol = x_tol0
    for _ in range(MAX_RELAX_TRY):
        bc1_nodes = _pick_bc1(inst_bc, bottom, y_min, x_tol, y_tol)
        if len(bc1_nodes) >= 2:
            break
        x_tol *= 1.5
    bc1_labels = _labels(bc1_nodes[:2])

    # ---- Pick BC-3 (U3): same (Y,Z), different X ----
    bc3_nodes = []
    z_tol = z_tol0
    for _ in range(MAX_RELAX_TRY):
        bc3_nodes = _pick_bc3(inst_bc, bottom, y_min, z_tol, y_tol)
        if len(bc3_nodes) >= 2:
            break
        z_tol *= 1.5
    bc3_labels = _labels(bc3_nodes[:2])

    # ---- Pick BC-2 (U2): 4 bottom corner-ish nodes ----
    bc2_nodes = _pick_bc2_corners(inst_bc, bottom, ix0, ix1, iz0, iz1, CORNER_MARGIN)
    bc2_labels = _labels(bc2_nodes)

    print("[BC] bottom nodes:", len(bottom))
    print("[BC] BC-1 labels (U1):", bc1_labels)
    print("[BC] BC-2 labels (U2):", bc2_labels)
    print("[BC] BC-3 labels (U3):", bc3_labels)

    # Assembly node sets
    set1 = _ensure_assembly_set_from_labels(a, 'BC1_pts', inst_bc.name, bc1_labels)
    set2 = _ensure_assembly_set_from_labels(a, 'BC2_pts', inst_bc.name, bc2_labels)
    set3 = _ensure_assembly_set_from_labels(a, 'BC3_pts', inst_bc.name, bc3_labels)

    # Create/update BCs at Step-1 (propagate onward)
    if set1 is not None:
        if 'BC-1' in m.boundaryConditions.keys():
            m.boundaryConditions['BC-1'].setValues(region=a.sets['BC1_pts'], u1=0.0, u2=UNSET, u3=UNSET)
        else:
            m.DisplacementBC(name='BC-1', createStepName='Step-1',
                             region=a.sets['BC1_pts'], u1=0.0, u2=UNSET, u3=UNSET,
                             ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                             distributionType=UNIFORM, fieldName='', localCsys=None)
    if set2 is not None:
        if 'BC-2' in m.boundaryConditions.keys():
            m.boundaryConditions['BC-2'].setValues(region=a.sets['BC2_pts'], u1=UNSET, u2=0.0, u3=UNSET)
        else:
            m.DisplacementBC(name='BC-2', createStepName='Step-1',
                             region=a.sets['BC2_pts'], u1=UNSET, u2=0.0, u3=UNSET,
                             ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                             distributionType=UNIFORM, fieldName='', localCsys=None)
    if set3 is not None:
        if 'BC-3' in m.boundaryConditions.keys():
            m.boundaryConditions['BC-3'].setValues(region=a.sets['BC3_pts'], u1=UNSET, u2=UNSET, u3=0.0)
        else:
            m.DisplacementBC(name='BC-3', createStepName='Step-1',
                             region=a.sets['BC3_pts'], u1=UNSET, u2=UNSET, u3=0.0,
                             ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                             distributionType=UNIFORM, fieldName='', localCsys=None)

    try:
        mdb.save()
    except:
        pass

    print("[BC] Boundary conditions and temperature fields created/updated and saved.")

if __name__ == "__main__":
    main()
