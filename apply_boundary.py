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
BUILD_AXIS     = "Y"           # "X" | "Y" | "Z"
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

def _axis_idx(ax):
    ax = (ax or "Y").upper()
    return 0 if ax == "X" else 2 if ax == "Z" else 1

def _plane_axes(ax):
    """Return the two indices forming the plane orthogonal to BUILD_AXIS."""
    i = _axis_idx(ax)
    others = [0,1,2]
    others.remove(i)
    return others[0], others[1]

def _inst_bbox(inst):
    xs=[]; ys=[]; zs=[]
    for n in inst.nodes:
        x,y,z = n.coordinates
        xs.append(x); ys.append(y); zs.append(z)
    if not xs:
        raise RuntimeError("No mesh nodes found on instance '%s'." % inst.name)
    return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)

def _collect_bottom_nodes(inst, tol_axis, build_axis):
    bi = _axis_idx(build_axis)
    cmin = min(n.coordinates[bi] for n in inst.nodes)
    nodes = [n for n in inst.nodes if abs(n.coordinates[bi] - cmin) <= tol_axis]
    return nodes, cmin

def _labels(nodes):
    return [n.label for n in nodes]

# ---- corrected helpers (swapped as per your spec) ----
def _pick_bc1(bottom_nodes, bottom_coord, tol_bottom, tol_hold, build_axis):
    # Hold axis = first in-plane axis; vary along the second in-plane axis
    if not bottom_nodes:
        return []
    hold_idx, var_idx = _plane_axes(build_axis)
    # median along hold axis within bottom band
    hold_vals = [n.coordinates[hold_idx] for n in bottom_nodes if abs(n.coordinates[_axis_idx(build_axis)] - bottom_coord) <= tol_bottom]
    if not hold_vals:
        return []
    hold_med = _median(hold_vals)
    band = [n for n in bottom_nodes
            if (abs(n.coordinates[_axis_idx(build_axis)] - bottom_coord) <= tol_bottom and
                abs(n.coordinates[hold_idx] - hold_med) <= tol_hold)]
    if len(band) < 2:
        return []
    band_sorted = sorted(band, key=lambda n: n.coordinates[var_idx])  # extremes along var axis
    return [band_sorted[0], band_sorted[-1]]

def _pick_bc3(bottom_nodes, bottom_coord, tol_bottom, tol_hold, build_axis):
    # Hold axis = second in-plane axis; vary along the first in-plane axis
    if not bottom_nodes:
        return []
    a, b = _plane_axes(build_axis)
    hold_idx, var_idx = b, a  # swap vs. BC1
    hold_vals = [n.coordinates[hold_idx] for n in bottom_nodes if abs(n.coordinates[_axis_idx(build_axis)] - bottom_coord) <= tol_bottom]
    if not hold_vals:
        return []
    hold_med = _median(hold_vals)
    band = [n for n in bottom_nodes
            if (abs(n.coordinates[_axis_idx(build_axis)] - bottom_coord) <= tol_bottom and
                abs(n.coordinates[hold_idx] - hold_med) <= tol_hold)]
    if len(band) < 2:
        return []
    band_sorted = sorted(band, key=lambda n: n.coordinates[var_idx])  # extremes along var axis
    return [band_sorted[0], band_sorted[-1]]

def _pick_bc2_corners(inst, bottom_nodes, span_min, span_max, other_min, other_max, margin_frac, hold_idx, var_idx):
    """Pick 4 bottom nodes near the rectangle corners in the (hold_idx, var_idx) plane."""
    if not bottom_nodes:
        return []
    d_hold = max(span_max - span_min, 1e-12)
    d_var  = max(other_max - other_min, 1e-12)
    m_hold = margin_frac * d_hold
    m_var  = margin_frac * d_var
    corners = [(span_min, other_min), (span_min, other_max), (span_max, other_min), (span_max, other_max)]    

    chosen = []
    chosen_labels = set()
    for (c_hold, c_var) in corners:
        best = None
        best_d2 = 1e99
        for n in bottom_nodes:
            coords = n.coordinates
            if (abs(coords[hold_idx] - c_hold) <= m_hold) and (abs(coords[var_idx] - c_var) <= m_var):
                d2 = (coords[hold_idx]-c_hold)**2 + (coords[var_idx]-c_var)**2
                if d2 < best_d2 and (n.label not in chosen_labels):
                    best = n; best_d2 = d2
        if best is not None:
            chosen.append(best)
            chosen_labels.add(best.label)
    if len(chosen) < 4 and bottom_nodes:
        # fallback: farthest from centroid in (hold,var) plane
        c_hold = sum(n.coordinates[hold_idx] for n in bottom_nodes)/float(len(bottom_nodes))
        c_var  = sum(n.coordinates[var_idx]  for n in bottom_nodes)/float(len(bottom_nodes))
        pool = [n for n in bottom_nodes if n.label not in chosen_labels]
        pool.sort(key=lambda n: (n.coordinates[hold_idx]-c_hold)**2 + (n.coordinates[var_idx]-c_var)**2, reverse=True)
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

    # Metrics & tolerances (axis-aware)
    ix0, ix1, iy0, iy1, iz0, iz1 = _inst_bbox(inst_bc)
    base_build_min = max(1e-6, min(float(BASE_SEED or 0.0), float(BUILD_SEED or 0.0)) or 0.5)
    # in-plane axes (orthogonal to BUILD_AXIS)
    p1, p2 = _plane_axes(BUILD_AXIS)
    # extents on the two in-plane axes (from full instance bbox)
    mins = [ix0, iy0, iz0]; maxs = [ix1, iy1, iz1]
    span1 = maxs[p1] - mins[p1]; span2 = maxs[p2] - mins[p2]
    tol_bottom0 = max(1e-6, Y_TOL_FACTOR  * base_build_min)        # tolerance along build axis at the bottom
    tol_hold1   = max(1e-6, XZ_TOL_FACTOR * max(span1, 1e-12))     # median-band tol (BC-1 hold axis)
    tol_hold2   = max(1e-6, XZ_TOL_FACTOR * max(span2, 1e-12))     # median-band tol (BC-3 hold axis)

    bottom = []
    bottom_coord = None
    tol_bottom = tol_bottom0
    for _ in range(MAX_RELAX_TRY):
        bottom, bottom_coord = _collect_bottom_nodes(inst_bc, tol_bottom, BUILD_AXIS)
        if len(bottom) >= 6:
            break
        tol_bottom *= 1.6
    if not bottom:
        print("[BC][warn] No bottom nodes found; skip BC creation.")
        try:
            mdb.save()
        except:
            pass
        return

    # ---- Pick BC-1 (U1): hold first in-plane axis @ median, vary along second ----
    bc1_nodes = []
    tol_h = tol_hold1
    for _ in range(MAX_RELAX_TRY):
        bc1_nodes = _pick_bc1(bottom, bottom_coord, tol_bottom, tol_h, BUILD_AXIS)
        if len(bc1_nodes) >= 2:
            break
        tol_h *= 1.5
    bc1_labels = _labels(bc1_nodes[:2])

    # ---- Pick BC-3 (U3): hold second in-plane axis @ median, vary along first ----
    bc3_nodes = []
    tol_h = tol_hold2
    for _ in range(MAX_RELAX_TRY):
        bc3_nodes = _pick_bc3(bottom, bottom_coord, tol_bottom, tol_h, BUILD_AXIS)
        if len(bc3_nodes) >= 2:
            break
        tol_h *= 1.5
    bc3_labels = _labels(bc3_nodes[:2])

    # ---- Pick BC-2 (U2): 4 bottom corner-ish nodes in in-plane rectangle ----
    span_min = mins[p1]; span_max = maxs[p1]
    other_min = mins[p2]; other_max = maxs[p2]
    bc2_nodes = _pick_bc2_corners(inst_bc, bottom, span_min, span_max, other_min, other_max,
                                  CORNER_MARGIN, hold_idx=p1, var_idx=p2)
    bc2_labels = _labels(bc2_nodes)

    print("[BC] build axis:", BUILD_AXIS)
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
