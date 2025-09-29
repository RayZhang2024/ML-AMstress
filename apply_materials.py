from abaqus import *
from abaqusConstants import *
from caeModules import *
import csv, os, re

# ======= Patched constants (GUI overwrites these lines) =======
CAE_FILE        = r""
BASE_CSV        = r""
BUILD_CSV       = r""
BASE_MAT_NAME   = "base_material"
BUILD_MAT_NAME  = "additive_material"
BASE_SEC_NAME   = "base_sec"
BUILD_SEC_NAME  = "additive_sec"
MODEL_NAME      = "Model-1"
TOL             = 1.0e-9
TIME_INTERVAL   = 0.8   # "INTERVAL" or "END_STEP"


# =============================================================

def _read_csv_rows(path):
    rows = []
    with open(path, 'rb') as f:  # Py2 binary then decode
        for raw in f:
            try:
                line = raw.decode('utf-8').strip()
            except:
                line = raw.decode('latin-1').strip()
            if not line or line.startswith('#'):
                continue
            rows.append(line)
    if not rows:
        return [], []
    header = [h.strip().lower() for h in rows[0].split(',')]
    data = []
    for r in rows[1:]:
        parts = [p.strip() for p in r.split(',')]
        data.append(parts)
    return header, data

def _to_float(s, default=None):
    if s is None or s == "":
        return default
    try:
        return float(s.replace(' ', ''))
    except:
        return default

def _group_tables(header, data):
    # Expected canonical columns: law,E,nu,alpha,sigma,epsp,T,Tanneal
    idx = dict((name, -1) for name in ['law','e','nu','alpha','sigma','epsp','t','tanneal'])
    for i, h in enumerate(header):
        if h in idx: idx[h] = i

    elastic, expansion, plastic = [], [], []
    annealT = None

    for row in data:
        law = row[idx['law']].strip().upper() if idx['law'] >= 0 and idx['law'] < len(row) else ''
        if law == 'ELASTIC':
            E = _to_float(row[idx['e']]) if idx['e']>=0 else None
            nu = _to_float(row[idx['nu']]) if idx['nu']>=0 else None
            T = _to_float(row[idx['t']]) if idx['t']>=0 else None
            if E is not None and nu is not None and T is not None:
                elastic.append((E, nu, T))
        elif law == 'EXPANSION':
            a = _to_float(row[idx['alpha']]) if idx['alpha']>=0 else None
            T = _to_float(row[idx['t']]) if idx['t']>=0 else None
            if a is not None and T is not None:
                expansion.append((a, T))
        elif law == 'PLASTIC':
            s = _to_float(row[idx['sigma']]) if idx['sigma']>=0 else None
            e = _to_float(row[idx['epsp']]) if idx['epsp']>=0 else None
            T = _to_float(row[idx['t']]) if idx['t']>=0 else None
            if s is not None and e is not None and T is not None:
                plastic.append((s, e, T))
        elif law == 'ANNEAL':
            tA = _to_float(row[idx['tanneal']]) if idx['tanneal']>=0 else None
            if tA is not None: annealT = tA

    elastic.sort(key=lambda x: x[2])
    expansion.sort(key=lambda x: x[1])
    plastic.sort(key=lambda x: x[2])
    return elastic, expansion, plastic, annealT

def _create_material_from_csv(model, name, csv_path):
    header, data = _read_csv_rows(csv_path)
    if not header:
        raise RuntimeError('Empty CSV: %s' % csv_path)
    elastic, expansion, plastic, annealT = _group_tables(header, data)

    mat = model.Material(name=name)
    if elastic:
        mat.Elastic(temperatureDependency=ON, table=tuple(elastic))
    if expansion:
        mat.Expansion(temperatureDependency=ON, table=tuple(expansion))
    if plastic:
        mat.Plastic(temperatureDependency=ON, table=tuple(plastic))
    if annealT is not None:
        mat.plastic.AnnealTemperature(table=((annealT,),))
    return mat

def _infer_layers_from_assembly(assembly):
    """Return N = number of per-layer assembly sets (set-1..set-N)."""
    ids = []
    for nm in assembly.sets.keys():
        if nm.startswith('set-'):
            try:
                k = int(nm.split('-')[1])
                ids.append(k)
            except:
                pass
    if not ids:
        return 0
    # Expect contiguous 1..N and (N+1)=whole build. Count how many 1..(kmax-1) exist.
    kmax = max(ids)
    n_layers = 0
    for k in range(1, kmax):  # ignore set-0 (base); last (kmax) is usually whole build
        if ('set-%d' % k) in assembly.sets.keys():
            n_layers += 1
    return n_layers

def main():
    # Open model
    openMdb(pathName=CAE_FILE)
    m = mdb.models[MODEL_NAME]
    a = m.rootAssembly
    p = m.parts['ImportedPart']   # must match import template

    # --- Materials ---
    _create_material_from_csv(m, BASE_MAT_NAME, BASE_CSV)
    _create_material_from_csv(m, BUILD_MAT_NAME, BUILD_CSV)

    # --- Sections ---
    if BASE_SEC_NAME not in m.sections.keys():
        m.HomogeneousSolidSection(name=BASE_SEC_NAME, material=BASE_MAT_NAME, thickness=None)
    if BUILD_SEC_NAME not in m.sections.keys():
        m.HomogeneousSolidSection(name=BUILD_SEC_NAME, material=BUILD_MAT_NAME, thickness=None)

    # --- Section assignments on PART level ---
    if 'BASE' in p.sets.keys():
        p.SectionAssignment(region=p.sets['BASE'], sectionName=BASE_SEC_NAME)

    # Prefer BUILD_ALL; fall back to legacy LAYER_### if present and BUILD_ALL missing.
    if 'BUILD_ALL' in p.sets.keys():
        p.SectionAssignment(region=p.sets['BUILD_ALL'], sectionName=BUILD_SEC_NAME)
        layer_names = []  # not needed for counting anymore
    else:
        layer_names = sorted([nm for nm in p.sets.keys() if nm.startswith('LAYER_')])
        for nm in layer_names:
            p.SectionAssignment(region=p.sets[nm], sectionName=BUILD_SEC_NAME)

    # ===================== Steps (use ASSEMBLY sets to count layers) =====================
    layer_number = _infer_layers_from_assembly(a)
    # If fallback legacy part sets exist and imply more layers, prefer the larger count
    if layer_names:
        layer_number = max(layer_number, len(layer_names))

    def _mk_step(idx, prev, initInc, maxInc, maxNumInc, minInc, period):
        name = 'Step-%d' % idx
        if name not in m.steps.keys():
            m.StaticStep(name=name, previous=prev, timePeriod=period,
                         initialInc=initInc, maxInc=maxInc, maxNumInc=maxNumInc, minInc=minInc)
        return name

    prev = 'Initial'
    prev = _mk_step(1, prev, 0.08, 0.3, 10000, 0.0002, 4.0)  # base
    for i in range(1, layer_number+1):                       # layers
        prev = _mk_step(i+1, prev, 0.08, 0.3, 10000, 0.0002, 4.0)
    prev = _mk_step(layer_number+2, prev, 0.1, 1.0, 10000, 0.0002, 1.0)   # cooling
    prev = _mk_step(layer_number+3, prev, 1.0, 1.0, 10000, 0.0002, 1.0)   # base removal

    # # Field Output
    # if 'F-Output-1' in m.fieldOutputRequests.keys():
    #     m.fieldOutputRequests['F-Output-1'].setValues(timeInterval=0.8, timeMarks=OFF, variables=('S','NT', 'U'))
    # else:
    #     m.FieldOutputRequest(name='F-Output-1', createStepName='Step-1',
    #                          variables=('S','NT', 'U'), timeInterval=0.8, timeMarks=OFF)

    # # Field Output (configurable: interval or end-of-step)
    # req_vars = ('S', 'NT', 'U')
    
    # # define helper to apply the chosen frequency settings
    # def _apply_fout_settings(req):
    #     mode = (FOUT_MODE or '').upper().strip()
    #     if mode == 'END_STEP':
    #         # last increment of each step (i.e., end-of-step)
    #         req.setValues(variables=req_vars, frequency=LAST_INCREMENT)
    #     else:
    #         # interval mode, clamp to [0, 4.0] and use timeInterval
    #         dt = float(FOUT_DT if FOUT_DT is not None else 0.8)
    #         if dt < 0.0: dt = 0.0
    #         if dt > 4.0: dt = 4.0
    #         req.setValues(variables=req_vars, timeInterval=dt, timeMarks=OFF)
    
    # if 'F-Output-1' in m.fieldOutputRequests.keys():
    #     _apply_fout_settings(m.fieldOutputRequests['F-Output-1'])
    # else:
    #     # create with a safe default first, then apply settings
    #     m.FieldOutputRequest(name='F-Output-1', createStepName='Step-1',
    #                          variables=req_vars, timeInterval=0.8, timeMarks=OFF)
    #     _apply_fout_settings(m.fieldOutputRequests['F-Output-1'])

    if 'F-Output-1' in m.fieldOutputRequests.keys():
        m.fieldOutputRequests['F-Output-1'].setValues(
            variables=('S', 'U', 'NT'),
            timeInterval=TIME_INTERVAL,
            timeMarks=OFF
        )
    else:
        m.FieldOutputRequest(
            name='F-Output-1',
            createStepName='Step-1',
            variables=('S', 'U', 'NT'),
            timeInterval=TIME_INTERVAL,
            timeMarks=OFF
        )

    # ===================== Interactions (ModelChange) =====================
    # All-build set is set-(N+1)
    all_nm = 'set-%d' % (layer_number + 1)
    if all_nm in a.sets.keys():
        if 'Int-1' not in m.interactions.keys():
            m.ModelChange(name='Int-1', createStepName='Step-1',
                          region=a.sets[all_nm], activeInStep=False, includeStrain=False)

    # Reactivate each layer incrementally: Step-2..Step-(N+1)
    for i in range(layer_number):  # i = 0..N-1
        step_name = 'Step-%d' % (i+2)
        set_name  = 'set-%d' % (i+1)
        int_name  = 'Int-%d' % (i+2)
        if set_name in a.sets.keys() and int_name not in m.interactions.keys():
            m.ModelChange(name=int_name, createStepName=step_name,
                          region=a.sets[set_name], activeInStep=True, includeStrain=False)

    # Deactivate base in Step-(N+3)
    base_step = 'Step-%d' % (layer_number + 3)
    if 'set-0' in a.sets.keys():
        nm = 'Int-%d' % (layer_number + 2)
        if nm not in m.interactions.keys():
            m.ModelChange(name=nm, createStepName=base_step,
                          region=a.sets['set-0'], activeInStep=False, includeStrain=False)

    try:
        mdb.save()
    except:
        pass

    print('Materials/sections applied, %d layers; steps & interactions created.' % layer_number)

if __name__ == '__main__':
    main()
