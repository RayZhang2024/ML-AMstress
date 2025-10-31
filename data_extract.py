# # -*- coding: utf-8 -*-
# """
# data_extract.py  (Abaqus Python 2.7, headless)
# ----------------------------------------------
# For each .odb in ODB_DIR:
#   • Select nodes on a geometric plane (XY/XZ/YZ at PLANE_POS ± TOL).
#   • Extract VARIABLE at VAR_POSITION:
#       - "Integration Point": values at elements whose centroid lies on plane.
#       - "Unique Nodal": manual nodal average from IP values at connected elements,
#                         then filter nodes on plane.
#   • Report chosen step/frame (STEP_SELECT, FRAME_SELECT).
#   • Save ONE sorted CSV per ODB into OUTPUT_DIR.
#   • Additionally, write ONE aggregate CSV with rows:
#       [first_num_from_odb_name, second_num_from_odb_name, value1, value2, ...]
#     (no header)

# VARIABLE:     "NT11", "Mises", "S11", "S22", "S33", "U1", "U2", "U3"
# VAR_POSITION: "Unique Nodal" | "Integration Point"
# STEP_SELECT:  "last" | step name | integer index (0-based or negative for from-end)
# FRAME_SELECT: "last" | integer index (0-based or negative for from-end)
# """

# # === Constants (GUI replaces these via regex) ===
# ODB_DIR       = r"C:\path\to\odbs"
# OUTPUT_DIR    = r"C:\path\to\out"
# PLANE         = "XY"            # "XY", "XZ", "YZ"
# PLANE_POS     = 0.0             # model units (e.g., mm)
# VARIABLE      = "S33"           # "NT11", "Mises", "S11", "S22", "S33", "U1", "U2", "U3"
# VAR_POSITION  = "Unique Nodal"  # "Unique Nodal" or "Integration Point"
# TOL           = 1.0e-3          # plane tolerance
# STEP_SELECT   = "last"          # "last" | name | int
# FRAME_SELECT  = "last"          # "last" | int

# from odbAccess import openOdb
# from abaqusConstants import NODAL, INTEGRATION_POINT, MISES
# import os, csv, re

# # ---------- helpers ----------
# def _axis_from_plane(plane):
#     p = (plane or '').upper()
#     if p == 'XY': return 2, 'Z'
#     if p == 'XZ': return 1, 'Y'
#     if p == 'YZ': return 0, 'X'
#     raise ValueError("PLANE must be XY, XZ, or YZ")

# def _sort_indices_from_plane(plane):
#     p = (plane or '').upper()
#     if p == 'XY': return (0, 1)  # sort by X, then Y
#     if p == 'XZ': return (0, 2)  # sort by X, then Z
#     if p == 'YZ': return (1, 2)  # sort by Y, then Z
#     return (0, 1)

# def _want_position(tag):
#     t = (tag or '').lower().strip()
#     return 'NODAL' if t.startswith('unique') else 'IP'

# def _safe_float(x):
#     try: return float(x)
#     except:
#         try: return float(x[0])
#         except: return None

# def _bbox(odb):
#     xmin=ymin=zmin= 1e300; xmax=ymax=zmax=-1e300
#     for inst in odb.rootAssembly.instances.values():
#         for nd in inst.nodes:
#             x,y,z = nd.coordinates
#             if x<xmin: xmin=x
#             if x>xmax: xmax=x
#             if y<ymin: ymin=y
#             if y>ymax: ymax=y
#             if z<zmin: zmin=z
#             if z>zmax: zmax=z
#     return xmin,xmax,ymin,ymax,zmin,zmax

# def _pick_step(odb, sel):
#     names = list(odb.steps.keys())
#     if not names: raise RuntimeError("ODB has no steps")
#     if isinstance(sel, basestring):
#         if sel == 'last': return odb.steps[names[-1]]
#         if sel in odb.steps.keys(): return odb.steps[sel]
#         try:
#             i = int(sel);  i = len(names)+i if i<0 else i
#             return odb.steps[names[i]]
#         except: return odb.steps[names[-1]]
#     else:
#         i = int(sel);  i = len(names)+i if i<0 else i
#         return odb.steps[names[i]]

# def _pick_frame(step, sel):
#     frames = step.frames
#     if not frames: raise RuntimeError("Selected step has no frames")
#     if isinstance(sel, basestring) and sel == 'last':
#         return frames[-1], len(frames)-1
#     try:
#         i = int(sel);  i = len(frames)+i if i<0 else i
#         return frames[i], i
#     except:
#         return frames[-1], len(frames)-1

# def _build_maps(odb):
#     node_coords = {}; elem_conn = {}; elem_cent = {}
#     for iname, inst in odb.rootAssembly.instances.items():
#         ncoord = {}
#         for nd in inst.nodes:
#             ncoord[nd.label] = nd.coordinates
#             node_coords[(iname, nd.label)] = nd.coordinates
#         for el in inst.elements:
#             conn = list(el.connectivity); elem_conn[(iname, el.label)] = conn
#             sx=sy=sz=0.0; n=float(len(conn) or 1)
#             for nl in conn:
#                 x,y,z = ncoord[nl]; sx+=x; sy+=y; sz+=z
#             elem_cent[(iname, el.label)] = (sx/n, sy/n, sz/n)
#     return node_coords, elem_conn, elem_cent

# def _nodes_on_plane(node_coords, axis_idx, pos, tol):
#     sel = {}
#     for (iname, nl), c in node_coords.items():
#         if abs(c[axis_idx] - pos) <= tol:
#             sel.setdefault(iname, set()).add(nl)
#     return sel

# # def _get_scalar_field(frame, var_name):
# #     vn = (var_name or '').upper()
# #     fos = frame.fieldOutputs
# #     if vn == 'NT11':
# #         return fos['NT11'], False
# #     S = fos['S'] if 'S' in fos.keys() else None
# #     if S is None: return None, True
# #     if vn in ('S11','S22','S33'):
# #         try: return S.getScalarField(componentLabel=vn), True
# #         except: return None, True
# #     if vn == 'MISES':
# #         try: return S.getScalarField(invariant=MISES), True
# #         except: return None, True
# #     return None, True

# def _get_scalar_field(frame, var_name):
#     vn = (var_name or '').upper()
#     fos = frame.fieldOutputs

#     # Temperature (nodal scalar)
#     if vn == 'NT11':
#         return fos['NT11'], False  # not stress

#     # Displacement (nodal vector: U1, U2, U3, magnitude)
#     if vn in ('U1', 'U2', 'U3', 'UMAG'):
#         U = fos['U'] if 'U' in fos.keys() else None
#         if U is None:
#             return None, False
#         # Return the whole vector field; caller will project to component/mag at nodal level
#         return U, False

#     # Stress and von Mises (integration-point based, but we later project to nodal if needed)
#     S = fos['S'] if 'S' in fos.keys() else None
#     if S is None:
#         return None, True
#     if vn in ('S11', 'S22', 'S33'):
#         try:
#             return S.getScalarField(componentLabel=vn), True
#         except:
#             return None, True
#     if vn == 'MISES' or vn == 'MISESS' or vn == 'MISE':
#         try:
#             return S.getScalarField(invariant=MISES), True
#         except:
#             return None, True

#     return None, True


# def _manual_nodal_average(ip_field, elem_conn):
#     sums = {}; cnts = {}
#     for v in ip_field.values:
#         inst = v.instance.name
#         elab = getattr(v, 'elementLabel', None)
#         if elab is None: continue
#         conn = elem_conn.get((inst, elab))
#         if not conn: continue
#         val = _safe_float(v.data)
#         if val is None: continue
#         for nl in conn:
#             key = (inst, nl)
#             sums[key] = sums.get(key, 0.0) + val
#             cnts[key] = cnts.get(key, 0) + 1
#     avgs = {}
#     for k in sums:
#         c = float(cnts.get(k, 1))
#         avgs[k] = sums[k]/c
#     return avgs

# def _first_two_numbers_from_base(base):
#     nums = re.findall(r'-?\d+(?:\.\d+)?', base)

#     def _fmt_no_decimals_if_intlike(f):
#         # format as integer if very close to an int, else as compact float
#         if abs(f - int(round(f))) < 1e-9:
#             return str(int(round(f)))
#         return ("%g" % f)

#     a = ''
#     b = ''
#     if len(nums) > 0:
#         try:
#             fa = float(nums[0])
#             a = _fmt_no_decimals_if_intlike(fa)
#         except:
#             a = nums[0]
#     if len(nums) > 1:
#         try:
#             fb = abs(float(nums[1]))  # <- make second number positive
#             b = _fmt_no_decimals_if_intlike(fb)
#         except:
#             # if parsing fails, just strip leading '-' if present
#             b = nums[1].lstrip('-')

#     return a, b

# # ---------- per-ODB extraction (returns values-only list) ----------
# def _extract_one_odb(odb_path, axis_idx, plane_pos, tol, var_name, pos_mode,
#                      step_sel, frame_sel, out_dir, plane_tag, axis_name):
#     base = os.path.splitext(os.path.basename(odb_path))[0]
#     posabbr = 'NODAL' if pos_mode == 'NODAL' else 'IP'
#     posnum  = ("%g" % plane_pos).replace('.', 'p').replace('-', 'm')
#     out_name = "%s_%s_%s_%s%s.csv" % (base, var_name, posabbr, plane_tag, posnum)
#     out_file = os.path.join(out_dir, out_name)

#     print(">> ODB:", odb_path)
#     odb = openOdb(odb_path, readOnly=True)
#     values_only = []
#     try:
#         xmin,xmax,ymin,ymax,zmin,zmax = _bbox(odb)
#         print("   BBox: X[%.3g,%.3g] Y[%.3g,%.3g] Z[%.3g,%.3g]" % (xmin,xmax,ymin,ymax,zmin,zmax))
#         print("   Plane %s = %g (tol=%g)" % (axis_name, plane_pos, tol))

#         step = _pick_step(odb, step_sel)
#         frame, fi = _pick_frame(step, frame_sel)
#         print("   Step: %s | Frame index: %d" % (step.name, fi))

#         node_coords, elem_conn, elem_cent = _build_maps(odb)
#         nodes_on_plane = _nodes_on_plane(node_coords, axis_idx, plane_pos, tol)
#         count_nodes = sum(len(s) for s in nodes_on_plane.values())
#         print("   Nodes on plane:", count_nodes)

#         if count_nodes == 0:
#             print("   .. Skipping (no nodes within tolerance).")
#             return values_only  # empty

#         field, is_stress = _get_scalar_field(frame, var_name)
#         if field is None:
#             print("   !! Field not present:", var_name)
#             return values_only

#         # Collect rows (full) + values_only list
#         rows = []
#         if pos_mode == 'IP':
#             for v in field.values:
#                 inst = v.instance.name
#                 elab = getattr(v, 'elementLabel', None)
#                 if elab is None: continue
#                 cx, cy, cz = elem_cent.get((inst, elab), (None,None,None))
#                 if cx is None: continue
#                 coord = (cx, cy, cz)[axis_idx]
#                 if abs(coord - plane_pos) <= tol:
#                     val = _safe_float(v.data)
#                     if val is None: continue
#                     rows.append([os.path.basename(odb_path), step.name, fi,
#                                 inst, 'elem', elab, cx, cy, cz,
#                                 var_name, 'INTEGRATION_POINT', val])
#                     values_only.append(val)

#         else:
#             if (var_name.upper() in ('U1', 'U2', 'U3')):
#                 # Displacement is nodal vector; filter nodes on plane and extract the required component/magnitude
#                 comp = var_name.upper()
#                 idx_map = {'U1': 0, 'U2': 1, 'U3': 2}
#                 for v in field.values:
#                     inst = v.instance.name
#                     nl = getattr(v, 'nodeLabel', None)
#                     if nl is None:
#                         continue
#                     if nl not in nodes_on_plane.get(inst, ()):
#                         continue
#                     x,y,z = node_coords[(inst, nl)]
#                     # v.data is a vector (tuple/list of 3)
#                     try:
#                         ux, uy, uz = float(v.data[0]), float(v.data[1]), float(v.data[2])
#                     except:
#                         # robust fallback
#                         try:
#                             d = [float(t) for t in v.data]
#                             ux, uy, uz = d[0], d[1], d[2]
#                         except:
#                             continue
        
#                     if comp == 'UMAG':
#                         val = (ux*ux + uy*uy + uz*uz) ** 0.5
#                     else:
#                         val = (ux, uy, uz)[idx_map[comp]]
        
#                     rows.append([os.path.basename(odb_path), step.name, fi,
#                                 inst, 'node', nl, x, y, z,
#                                 var_name, 'NODAL', val])
#                     values_only.append(val)                    

#             else:
#                 if is_stress:
#                     avgs = _manual_nodal_average(field, elem_conn)
#                     for inst, nset in nodes_on_plane.items():
#                         for nl in nset:
#                             key = (inst, nl)
#                             if key not in avgs: continue
#                             x,y,z = node_coords[key]
#                             val = avgs[key]
#                             rows.append([os.path.basename(odb_path), step.name, fi,
#                                         inst, 'node', nl, x, y, z,
#                                         var_name, 'NODAL', val])
#                             values_only.append(val)
#                 else:
#                     # NT11 nodal
#                     for v in field.values:
#                         inst = v.instance.name
#                         nl   = getattr(v, 'nodeLabel', None)
#                         if nl is None: continue
#                         if nl not in nodes_on_plane.get(inst, ()): continue
#                         x,y,z = node_coords[(inst, nl)]
#                         val = _safe_float(v.data)
#                         if val is None: continue
#                         rows.append([os.path.basename(odb_path), step.name, fi,
#                                     inst, 'node', nl, x, y, z,
#                                     var_name, 'NODAL', val])
#                         values_only.append(val)

#         # Sort rows by in-plane coordinates, and reorder values_only accordingly
#         sort_i, sort_j = _sort_indices_from_plane(PLANE)
#         rows.sort(key=lambda r: (float(r[6 + sort_i]), float(r[6 + sort_j])))
#         if rows and len(values_only) == len(rows):
#             # reorder values_only to match sorted rows
#             values_only = [r[-1] for r in rows]

#         # Write per-ODB CSV (Py2 on Windows ⇒ 'wb')
#         f = open(out_file, 'wb')
#         try:
#             w = csv.writer(f)
#             w.writerow(['odb','step','frame','instance','label_type','label',
#                         'x','y','z','variable','position','value'])
#             for r in rows:
#                 w.writerow(r)
#         finally:
#             f.close()

#         print("   Wrote rows:", len(rows), "->", out_file)
#         return values_only

#     finally:
#         odb.close()

# def main():
#     axis_idx, axis_name = _axis_from_plane(PLANE)
#     pos_mode = _want_position(VAR_POSITION)
    
#     var_upper = (VARIABLE or '').upper()
#     if var_upper in ('U1', 'U2', 'U3'):
#         pos_mode = 'NODAL'  # force nodal for displacements


#     if not os.path.isdir(ODB_DIR):
#         raise RuntimeError("ODB_DIR not found: %s" % ODB_DIR)
#     if not os.path.isdir(OUTPUT_DIR):
#         os.makedirs(OUTPUT_DIR)

#     names = [n for n in os.listdir(ODB_DIR) if n.lower().endswith('.odb')]
#     names.sort()
#     if not names:
#         raise RuntimeError("No .odb files in: %s" % ODB_DIR)

#     # Aggregate rows: [firstNum, secondNum, v1, v2, ...]  (no header)
#     aggregate_rows = []

#     posabbr = 'NODAL' if pos_mode == 'NODAL' else 'IP'
#     posnum  = ("%g" % float(PLANE_POS)).replace('.', 'p').replace('-', 'm')
#     aggregate_name = "values_only_%s_%s_%s%s.csv" % (VARIABLE, posabbr, PLANE.upper(), posnum)
#     aggregate_path = os.path.join(OUTPUT_DIR, aggregate_name)

#     for n in names:
#         odb_path = os.path.join(ODB_DIR, n)
#         base = os.path.splitext(os.path.basename(odb_path))[0]
#         first, second = _first_two_numbers_from_base(base)

#         values_only = _extract_one_odb(
#             odb_path=odb_path,
#             axis_idx=axis_idx,
#             plane_pos=float(PLANE_POS),
#             tol=float(TOL),
#             var_name=VARIABLE,
#             pos_mode=pos_mode,
#             step_sel=STEP_SELECT,
#             frame_sel=FRAME_SELECT,
#             out_dir=OUTPUT_DIR,
#             plane_tag=PLANE.upper(),
#             axis_name=axis_name
#         )

#         # if values_only:
#         #     # Round results to 0 decimals for columns 3+
#         #     rounded = []
#         #     for v in values_only:
#         #         try:
#         #             # standard rounding to nearest integer
#         #             iv = int(round(float(v)))
#         #         except:
#         #             # fallback if any non-numeric sneaks in
#         #             try:
#         #                 iv = int(round(float(v[0])))
#         #             except:
#         #                 continue
#         #         rounded.append(iv)

#         #     row = [first, second] + rounded
#         #     aggregate_rows.append(row)
            
#         if values_only:
#             if var_upper in ('U1', 'U2', 'U3'):
#                 # Keep full precision for displacements in the aggregate CSV
#                 six_dec = []
#                 for v in values_only:
#                     try:
#                         fv = float(v)
#                     except:
#                         try:
#                             fv = float(v[0])
#                         except:
#                             continue
#                     six_dec.append("{:.6f}".format(fv))  # no rounding
#                 row = [first, second] + six_dec
#             else:
#                 # Original behavior: round other variables to 0 decimals
#                 rounded = []
#                 for v in values_only:
#                     try:
#                         iv = int(round(float(v)))
#                     except:
#                         try:
#                             iv = int(round(float(v[0])))
#                         except:
#                             continue
#                     rounded.append(iv)
#                 row = [first, second] + rounded

#             aggregate_rows.append(row)

#     # Write aggregate CSV (no header). Py2 Windows ⇒ 'wb'
#     f = open(aggregate_path, 'wb')
#     try:
#         w = csv.writer(f)
#         for row in aggregate_rows:
#             w.writerow(row)
#     finally:
#         f.close()

#     print(">> Aggregate values written:", aggregate_path)

# if __name__ == "__main__":
#     main()


# -*- coding: utf-8 -*-
"""
data_extract.py  (Abaqus Python 2.7, headless)
----------------------------------------------
For each .odb in ODB_DIR:

Path A) Coordinate sampling (IDW)  — if COORD_FILE is non-empty:
  • Load query points (x,y,z) from COORD_FILE (txt/csv; commas or whitespace).
  • Build scalar field for the selected VARIABLE at the requested VAR_POSITION:
      - "Unique Nodal": use nodal fields directly (NT11, U) or nodal average of IP fields (S*, Mises).
      - "Integration Point": use element IP values and element centroids.
  • For each query point, evaluate IDW:
      - Gather up to IDW_K nearest supports within IDW_RADIUS.
      - Weight = 1 / (distance^IDW_POWER).
      - Return weighted average; exact-match (distance=0) returns the exact support value.
  • Save one CSV per ODB with rows:
      [odb,step,frame,variable,position,qx,qy,qz,match_count,value]
  • Aggregate CSV also written (same naming as plane-mode).
    For U* keep 6 decimals; others rounded to integers (legacy behavior).

Path B) Plane extractor  — if COORD_FILE is empty (original behavior):
  • Select nodes (or element centroids) on a geometric plane (XY/XZ/YZ at PLANE_POS ± TOL).
  • Extract VARIABLE at VAR_POSITION.
  • Report chosen step/frame (STEP_SELECT, FRAME_SELECT).
  • Save ONE sorted CSV per ODB into OUTPUT_DIR.
  • Additionally, write ONE aggregate CSV with rows:
      [first_num_from_odb_name, second_num_from_odb_name, value1, value2, ...]
    (no header)

VARIABLE:     "NT11", "Mises", "S11", "S22", "S33", "U1", "U2", "U3", "UMAG"
VAR_POSITION: "Unique Nodal" | "Integration Point"
STEP_SELECT:  "last" | step name | integer index (0-based or negative for from-end)
FRAME_SELECT: "last" | integer index (0-based or negative for from-end)
"""

# === Constants (GUI replaces these via regex) ===
ODB_DIR       = r"C:\path\to\odbs"
OUTPUT_DIR    = r"C:\path\to\out"
PLANE         = "XY"            # "XY", "XZ", "YZ"
PLANE_POS     = 0.0             # model units (e.g., mm)
VARIABLE      = "S33"           # "NT11", "Mises", "S11", "S22", "S33", "U1", "U2", "U3", "UMAG"
VAR_POSITION  = "Unique Nodal"  # "Unique Nodal" or "Integration Point"
TOL           = 1.0e-3          # plane tolerance
STEP_SELECT   = "last"          # "last" | name | int
FRAME_SELECT  = "last"          # "last" | int

# --- IDW coordinate sampling controls (leave COORD_FILE empty to keep plane extractor) ---
COORD_FILE   = r""      # path to txt/csv with x,y,z per line; empty string = disabled
IDW_K        = 4        # number of nearest supports (max)
IDW_RADIUS   = 1e-3     # search radius (model units). Only supports within this radius are used
IDW_POWER    = 2.0      # power in inverse-distance weighting (2.0 = standard IDW)
# -----------------------------------------------------------------------------------------

from odbAccess import openOdb
from abaqusConstants import NODAL, INTEGRATION_POINT, MISES
import os, csv, re, math

# ---------- helpers ----------
def _axis_from_plane(plane):
    p = (plane or '').upper()
    if p == 'XY': return 2, 'Z'
    if p == 'XZ': return 1, 'Y'
    if p == 'YZ': return 0, 'X'
    raise ValueError("PLANE must be XY, XZ, or YZ")

def _sort_indices_from_plane(plane):
    p = (plane or '').upper()
    if p == 'XY': return (0, 1)  # sort by X, then Y
    if p == 'XZ': return (0, 2)  # sort by X, then Z
    if p == 'YZ': return (1, 2)  # sort by Y, then Z
    return (0, 1)

def _want_position(tag):
    t = (tag or '').lower().strip()
    return 'NODAL' if t.startswith('unique') else 'IP'

def _safe_float(x):
    try: return float(x)
    except:
        try: return float(x[0])
        except: return None

def _bbox(odb):
    xmin=ymin=zmin= 1e300; xmax=ymax=zmax=-1e300
    for inst in odb.rootAssembly.instances.values():
        for nd in inst.nodes:
            x,y,z = nd.coordinates
            if x<xmin: xmin=x
            if x>xmax: xmax=x
            if y<ymin: ymin=y
            if y>ymax: ymax=y
            if z<zmin: zmin=z
            if z>zmax: zmax=z
    return xmin,xmax,ymin,ymax,zmin,zmax

def _pick_step(odb, sel):
    names = list(odb.steps.keys())
    if not names: raise RuntimeError("ODB has no steps")
    if isinstance(sel, basestring):
        if sel == 'last': return odb.steps[names[-1]]
        if sel in odb.steps.keys(): return odb.steps[sel]
        try:
            i = int(sel);  i = len(names)+i if i<0 else i
            return odb.steps[names[i]]
        except: return odb.steps[names[-1]]
    else:
        i = int(sel);  i = len(names)+i if i<0 else i
        return odb.steps[names[i]]

def _pick_frame(step, sel):
    frames = step.frames
    if not frames: raise RuntimeError("Selected step has no frames")
    if isinstance(sel, basestring) and sel == 'last':
        return frames[-1], len(frames)-1
    try:
        i = int(sel);  i = len(frames)+i if i<0 else i
        return frames[i], i
    except:
        return frames[-1], len(frames)-1

def _build_maps(odb):
    node_coords = {}; elem_conn = {}; elem_cent = {}
    for iname, inst in odb.rootAssembly.instances.items():
        ncoord = {}
        for nd in inst.nodes:
            ncoord[nd.label] = nd.coordinates
            node_coords[(iname, nd.label)] = nd.coordinates
        for el in inst.elements:
            conn = list(el.connectivity); elem_conn[(iname, el.label)] = conn
            sx=sy=sz=0.0; n=float(len(conn) or 1)
            for nl in conn:
                x,y,z = ncoord[nl]; sx+=x; sy+=y; sz+=z
            elem_cent[(iname, el.label)] = (sx/n, sy/n, sz/n)
    return node_coords, elem_conn, elem_cent

def _nodes_on_plane(node_coords, axis_idx, pos, tol):
    sel = {}
    for (iname, nl), c in node_coords.items():
        if abs(c[axis_idx] - pos) <= tol:
            sel.setdefault(iname, set()).add(nl)
    return sel

def _load_query_points(path):
    pts = []
    if not path:
        return pts
    try:
        f = open(path, 'r')
        try:
            for raw in f:
                s = raw.strip()
                if not s or s.startswith('#'):
                    continue
                if ',' in s:
                    parts = [p.strip() for p in s.split(',')]
                else:
                    parts = s.split()
                if len(parts) < 3:
                    continue
                x = float(parts[0]); y = float(parts[1]); z = float(parts[2])
                pts.append((x, y, z))
        finally:
            f.close()
    except Exception as e:
        print("[WARN] Failed to read COORD_FILE: %s" % str(e))
    return pts

def _squared_distance(a, b):
    dx = a[0]-b[0]; dy = a[1]-b[1]; dz = a[2]-b[2]
    return dx*dx + dy*dy + dz*dz

def _first_two_numbers_from_base(base):
    nums = re.findall(r'-?\d+(?:\.\d+)?', base)

    def _fmt_no_decimals_if_intlike(f):
        if abs(f - int(round(f))) < 1e-9:
            return str(int(round(f)))
        return ("%g" % f)

    a = ''
    b = ''
    if len(nums) > 0:
        try:
            fa = float(nums[0])
            a = _fmt_no_decimals_if_intlike(fa)
        except:
            a = nums[0]
    if len(nums) > 1:
        try:
            fb = abs(float(nums[1]))  # second number positive
            b = _fmt_no_decimals_if_intlike(fb)
        except:
            b = nums[1].lstrip('-')
    return a, b

# ---------- field access ----------
def _get_scalar_field(frame, var_name):
    vn = (var_name or '').upper()
    fos = frame.fieldOutputs

    # Temperature (nodal scalar)
    if vn == 'NT11':
        return fos['NT11'], False  # not stress

    # Displacement (nodal vector: U1, U2, U3, magnitude)
    if vn in ('U1', 'U2', 'U3', 'UMAG'):
        U = fos['U'] if 'U' in fos.keys() else None
        if U is None:
            return None, False
        return U, False

    # Stress and von Mises
    S = fos['S'] if 'S' in fos.keys() else None
    if S is None:
        return None, True
    if vn in ('S11', 'S22', 'S33'):
        try:
            return S.getScalarField(componentLabel=vn), True
        except:
            return None, True
    if vn in ('MISES', 'MISESS', 'MISE'):
        try:
            return S.getScalarField(invariant=MISES), True
        except:
            return None, True
    return None, True

def _manual_nodal_average(ip_field, elem_conn):
    sums = {}; cnts = {}
    for v in ip_field.values:
        inst = v.instance.name
        elab = getattr(v, 'elementLabel', None)
        if elab is None: continue
        conn = elem_conn.get((inst, elab))
        if not conn: continue
        val = _safe_float(v.data)
        if val is None: continue
        for nl in conn:
            key = (inst, nl)
            sums[key] = sums.get(key, 0.0) + val
            cnts[key] = cnts.get(key, 0) + 1
    avgs = {}
    for k in sums:
        c = float(cnts.get(k, 1))
        avgs[k] = sums[k]/c
    return avgs

# ---------- IDW core ----------
def _idw_value(q, supports, values, k, radius, power):
    """
    q: (x,y,z)
    supports: list of (x,y,z) (same order as values)
    values: list of scalar floats
    k: int, max neighbours
    radius: float, max distance (strictly >0) for inclusion
    power: float, IDW power (2.0 typical)
    Returns (val, used_count). If no neighbours, returns (nan, 0).
    """
    if not supports or not values:
        return float('nan'), 0

    # Build list of (d2, idx) and keep smallest k within radius
    cand = []
    r2 = radius * radius
    for i in range(len(supports)):
        s = supports[i]
        d2 = _squared_distance(q, s)
        if d2 <= r2:
            cand.append((d2, i))
    if not cand:
        return float('nan'), 0

    cand.sort(key=lambda t: t[0])
    cand = cand[:max(1, int(k))]

    # Exact match: if distance is ~0, return the exact support value
    if cand[0][0] < 1e-30:
        return float(values[cand[0][1]]), 1

    # IDW
    num = 0.0; den = 0.0; used = 0
    for d2, idx in cand:
        # weight = 1 / (d^power) = 1 / ((d2)^(power/2))
        # to avoid pow underflow in Py2.7, use math.pow:
        w = 1.0 / math.pow(d2, 0.5 * power)
        v = float(values[idx])
        num += w * v
        den += w
        used += 1
    if den <= 0.0:
        return float('nan'), 0
    return (num / den), used

# ---------- per-ODB extraction ----------
def _extract_one_odb_plane(odb_path, axis_idx, plane_pos, tol, var_name, pos_mode,
                           step_sel, frame_sel, out_dir, plane_tag, axis_name):
    """
    Original plane extractor (unchanged), used when COORD_FILE == "".
    Returns values_only list (for aggregate csv).
    """
    base = os.path.splitext(os.path.basename(odb_path))[0]
    posabbr = 'NODAL' if pos_mode == 'NODAL' else 'IP'
    posnum  = ("%g" % plane_pos).replace('.', 'p').replace('-', 'm')
    out_name = "%s_%s_%s_%s%s.csv" % (base, var_name, posabbr, plane_tag, posnum)
    out_file = os.path.join(out_dir, out_name)

    print(">> ODB:", odb_path)
    odb = openOdb(odb_path, readOnly=True)
    values_only = []
    try:
        xmin,xmax,ymin,ymax,zmin,zmax = _bbox(odb)
        print("   BBox: X[%.3g,%.3g] Y[%.3g,%.3g] Z[%.3g,%.3g]" % (xmin,xmax,ymin,ymax,zmin,zmax))
        print("   Plane %s = %g (tol=%g)" % (axis_name, plane_pos, tol))

        step = _pick_step(odb, step_sel)
        frame, fi = _pick_frame(step, frame_sel)
        print("   Step: %s | Frame index: %d" % (step.name, fi))

        node_coords, elem_conn, elem_cent = _build_maps(odb)
        nodes_on_plane = _nodes_on_plane(node_coords, axis_idx, plane_pos, tol)
        count_nodes = sum(len(s) for s in nodes_on_plane.values())
        print("   Nodes on plane:", count_nodes)

        if count_nodes == 0:
            print("   .. Skipping (no nodes within tolerance).")
            return values_only  # empty

        field, is_stress = _get_scalar_field(frame, var_name)
        if field is None:
            print("   !! Field not present:", var_name)
            return values_only

        rows = []
        if pos_mode == 'IP':
            for v in field.values:
                inst = v.instance.name
                elab = getattr(v, 'elementLabel', None)
                if elab is None: continue
                cx, cy, cz = elem_cent.get((inst, elab), (None,None,None))
                if cx is None: continue
                coord = (cx, cy, cz)[axis_idx]
                if abs(coord - plane_pos) <= tol:
                    val = _safe_float(v.data)
                    if val is None: continue
                    rows.append([os.path.basename(odb_path), step.name, fi,
                                inst, 'elem', elab, cx, cy, cz,
                                var_name, 'INTEGRATION_POINT', val])
                    values_only.append(val)

        else:
            vup = (var_name or '').upper()
            if vup in ('U1', 'U2', 'U3', 'UMAG'):
                comp = vup
                idx_map = {'U1': 0, 'U2': 1, 'U3': 2}
                for vv in field.values:
                    inst = vv.instance.name
                    nl = getattr(vv, 'nodeLabel', None)
                    if nl is None:
                        continue
                    if nl not in nodes_on_plane.get(inst, ()):
                        continue
                    x,y,z = node_coords[(inst, nl)]
                    try:
                        ux, uy, uz = float(vv.data[0]), float(vv.data[1]), float(vv.data[2])
                    except:
                        try:
                            d = [float(t) for t in vv.data]
                            ux, uy, uz = d[0], d[1], d[2]
                        except:
                            continue
                    if comp == 'UMAG':
                        val = (ux*ux + uy*uy + uz*uz) ** 0.5
                    else:
                        val = (ux, uy, uz)[idx_map[comp]]
                    rows.append([os.path.basename(odb_path), step.name, fi,
                                inst, 'node', nl, x, y, z,
                                var_name, 'NODAL', val])
                    values_only.append(val)
            else:
                if is_stress:
                    avgs = _manual_nodal_average(field, elem_conn)
                    for inst, nset in nodes_on_plane.items():
                        for nl in nset:
                            key = (inst, nl)
                            if key not in avgs: continue
                            x,y,z = node_coords[key]
                            val = avgs[key]
                            rows.append([os.path.basename(odb_path), step.name, fi,
                                        inst, 'node', nl, x, y, z,
                                        var_name, 'NODAL', val])
                            values_only.append(val)
                else:
                    # NT11 nodal
                    for vv in field.values:
                        inst = vv.instance.name
                        nl   = getattr(vv, 'nodeLabel', None)
                        if nl is None: continue
                        if nl not in nodes_on_plane.get(inst, ()): continue
                        x,y,z = node_coords[(inst, nl)]
                        val = _safe_float(vv.data)
                        if val is None: continue
                        rows.append([os.path.basename(odb_path), step.name, fi,
                                    inst, 'node', nl, x, y, z,
                                    var_name, 'NODAL', val])
                        values_only.append(val)

        sort_i, sort_j = _sort_indices_from_plane(PLANE)
        rows.sort(key=lambda r: (float(r[6 + sort_i]), float(r[6 + sort_j])))
        if rows and len(values_only) == len(rows):
            values_only = [r[-1] for r in rows]

        f = open(out_file, 'wb')
        try:
            w = csv.writer(f)
            w.writerow(['odb','step','frame','instance','label_type','label',
                        'x','y','z','variable','position','value'])
            for r in rows:
                w.writerow(r)
        finally:
            f.close()

        print("   Wrote rows:", len(rows), "->", out_file)
        return values_only

    finally:
        odb.close()

def _extract_one_odb_idw(odb_path, var_name, pos_mode, step_sel, frame_sel,
                         out_dir, idw_k, idw_radius, idw_power, queries):
    """
    IDW extractor (coordinate sampling). Uses nodes for NODAL fields and
    element centroids for IP fields. Returns values_only list in the same
    order as 'queries'.
    """
    base = os.path.splitext(os.path.basename(odb_path))[0]
    posabbr = 'NODAL' if pos_mode == 'NODAL' else 'IP'
    out_name = "coord_idw__%s__%s__%s.csv" % (var_name, posabbr, base)
    out_file = os.path.join(out_dir, out_name)

    print(">> ODB (IDW):", odb_path)
    odb = openOdb(odb_path, readOnly=True)
    values_only = []
    try:
        step = _pick_step(odb, step_sel)
        frame, fi = _pick_frame(step, frame_sel)
        print("   Step: %s | Frame index: %d" % (step.name, fi))
        print("   IDW params: K=%s, R=%g, power=%g | queries=%d" % (str(idw_k), float(idw_radius), float(idw_power), len(queries)))

        node_coords, elem_conn, elem_cent = _build_maps(odb)
        field, is_stress = _get_scalar_field(frame, var_name)
        if field is None:
            print("   !! Field not present:", var_name)
            # still write file with headers, empty rows
            f = open(out_file, 'wb')
            try:
                w = csv.writer(f)
                w.writerow(['odb','step','frame','variable','position',
                            'qx','qy','qz','match_count','value'])
            finally:
                f.close()
            return values_only

        # --- Build supports (locations + values) for IDW
        supports_xyz = []
        supports_val = []

        vup = (var_name or '').upper()

        if pos_mode == 'NODAL':
            if vup in ('U1', 'U2', 'U3', 'UMAG'):
                idx_map = {'U1':0, 'U2':1, 'U3':2}
                for vv in field.values:
                    inst = vv.instance.name
                    nl   = getattr(vv, 'nodeLabel', None)
                    if nl is None: continue
                    key = (inst, nl)
                    if key not in node_coords: continue
                    x,y,z = node_coords[key]
                    try:
                        ux, uy, uz = float(vv.data[0]), float(vv.data[1]), float(vv.data[2])
                    except:
                        try:
                            d = [float(t) for t in vv.data]
                            ux, uy, uz = d[0], d[1], d[2]
                        except:
                            continue
                    if vup == 'UMAG':
                        val = math.sqrt(ux*ux + uy*uy + uz*uz)
                    else:
                        val = (ux, uy, uz)[idx_map[vup]]
                    supports_xyz.append((x,y,z))
                    supports_val.append(val)

            elif vup == 'NT11':
                for vv in field.values:
                    inst = vv.instance.name
                    nl   = getattr(vv, 'nodeLabel', None)
                    if nl is None: continue
                    key = (inst, nl)
                    if key not in node_coords: continue
                    x,y,z = node_coords[key]
                    val = _safe_float(vv.data)
                    if val is None: continue
                    supports_xyz.append((x,y,z))
                    supports_val.append(val)

            else:
                # stress-like variable requested at NODAL: use manual nodal average of IP field
                avgs = _manual_nodal_average(field, elem_conn)
                for key, val in avgs.items():
                    if key not in node_coords: continue
                    x,y,z = node_coords[key]
                    supports_xyz.append((x,y,z))
                    supports_val.append(val)

        else:  # pos_mode == 'IP'
            # Use element centroids + IP scalar values
            for vv in field.values:
                inst = vv.instance.name
                elab = getattr(vv, 'elementLabel', None)
                if elab is None: continue
                xyzt = elem_cent.get((inst, elab), None)
                if xyzt is None: continue
                val = _safe_float(vv.data)
                if val is None: continue
                supports_xyz.append(xyzt)
                supports_val.append(val)

        # --- Perform IDW for each query point
        rows = []
        for (qx, qy, qz) in queries:
            q = (qx, qy, qz)
            val, used = _idw_value(q, supports_xyz, supports_val, int(IDW_K), float(IDW_RADIUS), float(IDW_POWER))
            rows.append([os.path.basename(odb_path), step.name, fi,
                         var_name, posabbr, qx, qy, qz, used, val])
            values_only.append(val)

        # --- Write per-ODB CSV
        f = open(out_file, 'wb')
        try:
            w = csv.writer(f)
            w.writerow(['odb','step','frame','variable','position','qx','qy','qz','match_count','value'])
            for r in rows:
                w.writerow(r)
        finally:
            f.close()

        print("   Wrote IDW rows:", len(rows), "->", out_file)
        return values_only

    finally:
        odb.close()

# ---------- main ----------
def main():
    # Basic I/O checks
    if not os.path.isdir(ODB_DIR):
        raise RuntimeError("ODB_DIR not found: %s" % ODB_DIR)
    if not os.path.isdir(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    names = [n for n in os.listdir(ODB_DIR) if n.lower().endswith('.odb')]
    names.sort()
    if not names:
        raise RuntimeError("No .odb files in: %s" % ODB_DIR)

    # Aggregate CSV init
    pos_mode = _want_position(VAR_POSITION)
    var_upper = (VARIABLE or '').upper()

    # Displacements must be nodal
    if var_upper in ('U1', 'U2', 'U3', 'UMAG'):
        pos_mode = 'NODAL'

    posabbr = 'NODAL' if pos_mode == 'NODAL' else 'IP'
    posnum  = ("%g" % float(PLANE_POS)).replace('.', 'p').replace('-', 'm')
    aggregate_name = "values_only_%s_%s_%s%s.csv" % (VARIABLE, posabbr, PLANE.upper(), posnum)
    aggregate_path = os.path.join(OUTPUT_DIR, aggregate_name)
    aggregate_rows = []

    # Decide path: IDW iff coordinates provided
    queries = _load_query_points(COORD_FILE)
    use_idw = True if (COORD_FILE and len(queries) > 0) else False
    if use_idw:
        print("[INFO] Using IDW coordinate sampling from: %s (N=%d)" % (COORD_FILE, len(queries)))
    else:
        print("[INFO] COORD_FILE empty or no valid points found — using plane extractor.")

    if not use_idw:
        # Plane extractor path
        axis_idx, axis_name = _axis_from_plane(PLANE)
        for n in names:
            odb_path = os.path.join(ODB_DIR, n)
            base = os.path.splitext(os.path.basename(odb_path))[0]
            first, second = _first_two_numbers_from_base(base)

            values_only = _extract_one_odb_plane(
                odb_path=odb_path,
                axis_idx=axis_idx,
                plane_pos=float(PLANE_POS),
                tol=float(TOL),
                var_name=VARIABLE,
                pos_mode=pos_mode,
                step_sel=STEP_SELECT,
                frame_sel=FRAME_SELECT,
                out_dir=OUTPUT_DIR,
                plane_tag=PLANE.upper(),
                axis_name=axis_name
            )

            if values_only:
                if var_upper in ('U1', 'U2', 'U3', 'UMAG'):
                    six_dec = []
                    for v in values_only:
                        try:
                            fv = float(v)
                        except:
                            try:
                                fv = float(v[0])
                            except:
                                continue
                        six_dec.append("{:.6f}".format(fv))
                    row = [first, second] + six_dec
                else:
                    rounded = []
                    for v in values_only:
                        try:
                            iv = int(round(float(v)))
                        except:
                            try:
                                iv = int(round(float(v[0])))
                            except:
                                continue
                        rounded.append(iv)
                    row = [first, second] + rounded
                aggregate_rows.append(row)

    else:
        # IDW coordinate sampling path
        for n in names:
            odb_path = os.path.join(ODB_DIR, n)
            base = os.path.splitext(os.path.basename(odb_path))[0]
            first, second = _first_two_numbers_from_base(base)

            values_only = _extract_one_odb_idw(
                odb_path=odb_path,
                var_name=VARIABLE,
                pos_mode=pos_mode,
                step_sel=STEP_SELECT,
                frame_sel=FRAME_SELECT,
                out_dir=OUTPUT_DIR,
                idw_k=int(IDW_K),
                idw_radius=float(IDW_RADIUS),
                idw_power=float(IDW_POWER),
                queries=queries
            )

            if values_only:
                if var_upper in ('U1', 'U2', 'U3', 'UMAG'):
                    six_dec = []
                    for v in values_only:
                        try:
                            fv = float(v)
                        except:
                            try:
                                fv = float(v[0])
                            except:
                                continue
                        six_dec.append("{:.6f}".format(fv))
                    row = [first, second] + six_dec
                else:
                    rounded = []
                    for v in values_only:
                        try:
                            iv = int(round(float(v)))
                        except:
                            try:
                                iv = int(round(float(v[0])))
                            except:
                                continue
                        rounded.append(iv)
                    row = [first, second] + rounded
                aggregate_rows.append(row)

    # Write aggregate CSV (no header). Py2 Windows ⇒ 'wb'
    f = open(aggregate_path, 'wb')
    try:
        w = csv.writer(f)
        for row in aggregate_rows:
            w.writerow(row)
    finally:
        f.close()

    print(">> Aggregate values written:", aggregate_path)

if __name__ == "__main__":
    main()
