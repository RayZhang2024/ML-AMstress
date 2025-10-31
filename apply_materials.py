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
BUILD_AXIS      = "Y"   # "X" | "Y" | "Z" (not used in this file; kept for consistency)
AXIS_ZERO       = 0.0   # split plane value along BUILD_AXIS (ditto)
HT_ENABLED      = 0      # 0/1; GUI will overwrite
HT_TEMP_C       = 650.0  # °C; not used here but kept for clarity

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
    # Prefer 'ImportedPart' (import flow). Otherwise, if there's only one part, use it.
    if 'ImportedPart' in m.parts.keys():
        p = m.parts['ImportedPart']
    else:
        if len(m.parts.keys()) != 1:
            raise RuntimeError("Cannot resolve target part: expected 'ImportedPart' or a single part in the model.")
        p = m.parts[m.parts.keys()[0]]
    # --- Materials ---
    _create_material_from_csv(m, BASE_MAT_NAME, BASE_CSV)
    _create_material_from_csv(m, BUILD_MAT_NAME, BUILD_CSV)

    # # --- Sections ---
    # if BASE_SEC_NAME not in m.sections.keys():
    #     m.HomogeneousSolidSection(name=BASE_SEC_NAME, material=BASE_MAT_NAME, thickness=None)
    # if BUILD_SEC_NAME not in m.sections.keys():
        # m.HomogeneousSolidSection(name=BUILD_SEC_NAME, material=BUILD_MAT_NAME, thickness=None)

    # Create or update homogeneous solid sections
    if BASE_SEC_NAME in m.sections.keys():
        del m.sections[BASE_SEC_NAME]
    if BUILD_SEC_NAME in m.sections.keys():
        del m.sections[BUILD_SEC_NAME]
    m.HomogeneousSolidSection(name=BASE_SEC_NAME,  material=BASE_MAT_NAME,  thickness=None)
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

    if int(HT_ENABLED) == 1:
        # Heat-treatment step at index N+4; choose a 1.0 time unit period (UTEMP uses piecewise in-step time)
        prev = _mk_step(layer_number+4, prev, 0.05, 1.0, 10000, 0.0002, 0.05)    

    _ti = float(TIME_INTERVAL if TIME_INTERVAL is not None else 0.8)
    if _ti < 0.0: _ti = 0.8
    if _ti > 4.0: _ti = 4.0
    if 'F-Output-1' in m.fieldOutputRequests.keys():
        m.fieldOutputRequests['F-Output-1'].setValues(variables=('S','U','NT'),
                                                      timeInterval=_ti, timeMarks=OFF)
    else:
        m.FieldOutputRequest(name='F-Output-1', createStepName='Step-1',
                             variables=('S','U','NT'), timeInterval=_ti, timeMarks=OFF)


    def _is_static_general(step_obj):
        try:
            return step_obj.__class__.__name__ == 'StaticStep'
        except:
            return False
    
    def _ensure_build_steps(m, total_layers):
        """
        Ensure Static, General steps exist. We need steps 1..(N+3).
        If any existing Step-* is not StaticStep, use a clean BStep-* sequence.
        """
        need = int(total_layers) + 3
        use_prefix = 'Step'
        ok = True
        for i in range(1, need+1):
            nm = '%s-%d' % (use_prefix, i)
            if nm in m.steps.keys():
                if not _is_static_general(m.steps[nm]):
                    ok = False
                    break
        if not ok:
            use_prefix = 'BStep'
    
        prev = 'Initial'
        for i in range(1, need+1):
            nm = '%s-%d' % (use_prefix, i)
            if nm not in m.steps.keys():
                m.StaticStep(name=nm, previous=prev)
            prev = nm
    
        def _step(i):  # 1-based accessor
            return '%s-%d' % (use_prefix, i)
        return _step
    
    def _set_size(set_obj):
        """
        Best-effort size of a Set: counts any available collections.
        Works on Abaqus Set objects in assembly.
        """
        total = 0
        for attr in ('nodes', 'elements', 'faces', 'edges', 'cells', 'vertices'):
            if hasattr(set_obj, attr):
                try:
                    total += len(getattr(set_obj, attr))
                except:
                    pass
        return total
    
    def _detect_layers_and_allset(asm, fallback_layers):
        """
        Return (N, all_name) where:
          N        = number of layer sets set-1..set-N
          all_name = the "all-build" aggregate set (usually set-(N+1)), or None
        Strategy:
          - collect all indices i for which set-i exists (i >= 1)
          - choose all_name as the set with the largest size
          - set N as the largest index excluding the all_name index
        If detection fails, use fallback_layers and try 'set-(N+1)' as all_name.
        """
        ids = []
        for nm in asm.sets.keys():
            if nm.startswith('set-'):
                try:
                    idx = int(nm.split('-')[1])
                    if idx >= 1:
                        ids.append(idx)
                except:
                    pass
        if not ids:
            N = int(fallback_layers)
            all_name = 'set-%d' % (N + 1)
            if all_name not in asm.sets.keys():
                all_name = None
            return N, all_name
    
        # sizes for each set-i
        sizes = {}
        for i in ids:
            nm = 'set-%d' % i
            try:
                sizes[i] = _set_size(asm.sets[nm])
            except:
                sizes[i] = 0
        # pick the largest as "all-build" candidate
        all_idx = max(sizes, key=lambda k: sizes[k])
        # N is the largest index except the all-build candidate
        others = [i for i in ids if i != all_idx]
        if not others:
            # fallback: cannot separate; use fallback_layers
            N = int(fallback_layers)
            all_name = 'set-%d' % (N + 1) if ('set-%d' % (N + 1)) in asm.sets.keys() else None
            return N, all_name
        N = max(others)
        all_name = 'set-%d' % all_idx
    
        # Heuristic correction: if all_idx is not exactly N+1 but set-(N+1) exists, prefer that
        nmN1 = 'set-%d' % (N + 1)
        if nmN1 in asm.sets.keys():
            all_name = nmN1
        return int(N), all_name

    # ===================== Interactions (ModelChange) =====================
    # 1) Robustly detect N and the all-build set
    N, all_nm = _detect_layers_and_allset(a, fallback_layers=layer_number)
    if int(N) != int(layer_number):
        print("[WARN] layer_number=%d but detected %d layer sets; using detected value." % (layer_number, N))
        layer_number = int(N)

    # 2) Ensure valid Static, General steps exist: Step-1..Step-(N+3) or BStep-*
    step_of = _ensure_build_steps(m, N)

    # 3) Int-1: deactivate the all-build set at step 1 (if present)
    if all_nm and (all_nm in a.sets.keys()):
        if 'Int-1' in m.interactions.keys():
            del m.interactions['Int-1']
        m.ModelChange(name='Int-1', createStepName=step_of(1),
                      region=a.sets[all_nm], activeInStep=False, includeStrain=False)
    else:
        print("[INFO] No all-build set found; skipping Int-1.")

    # 4) Activate each layer i=1..N at step (i+1): Int-(i+1)
    made = 0
    for i in range(1, N+1):
        set_name = 'set-%d' % i
        if set_name not in a.sets.keys():
            print("[WARN] Missing %s - skipping." % set_name)
            continue
        int_name = 'Int-%d' % (i+1)
        if int_name in m.interactions.keys():
            del m.interactions[int_name]
        m.ModelChange(name=int_name, createStepName=step_of(i+1),
                      region=a.sets[set_name], activeInStep=True, includeStrain=False)
        made += 1

    # 5) Deactivate base (set-0) at step (N+3) for post-build release
    base_int = 'Int-%d' % (N + 2)
    if 'set-0' in a.sets.keys():
        if base_int in m.interactions.keys():
            del m.interactions[base_int]
        m.ModelChange(name=base_int, createStepName=step_of(N+3),
                      region=a.sets['set-0'], activeInStep=False, includeStrain=False)
    else:
        print("[INFO] No 'set-0' base set; skipping base deactivation.")

    print("[CHECK] N layers =", N,
          "| Int-1 present =", ('Int-1' in m.interactions.keys()),
          "| Layer activations created =", made)
    try:
        mdb.save()
    except:
        pass

    print('Materials/sections applied, %d layers; steps & interactions created.' % layer_number)

if __name__ == '__main__':
    main()
