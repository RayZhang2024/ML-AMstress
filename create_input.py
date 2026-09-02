
# create_input_v1, previoud version, create_input_v0
# modification 2023-07-02
# 1. keep the maximum tem8perature constant while the base plate temperature increases.
# 2. heat the base plate by half of the layer thickness



import sys
from abaqus import *
from abaqusConstants import *
import math

# 1=X, 2=Y, 3=Z (will be injected by AM_gui Input&UTEMP tab; keep safe defaults if not provided)
try:
    COORD_IDX
except NameError:
    COORD_IDX = 2  # default Y

try:
    AXIS_ZERO
except NameError:
    AXIS_ZERO = 0.0
    
# ---- HT flags injected by GUI; keep safe defaults ----
try:
    HT_ENABLED
except NameError:
    HT_ENABLED = 0  # 0 = off, 1 = on

try:
    HT_TEMP_C
except NameError:
    HT_TEMP_C = 650.0  # 
# ------------------------------------------------------


session.viewports['Viewport: 1'].setValues(displayedObject=None)
# The first CLI argument is the .cae file path passed in from AM_gui:
# CAE_FILE = sys.argv[1] if len(sys.argv) > 1 else None
# if not CAE_FILE:
#     raise ValueError("No CAE file supplied to create_input.py!")

openMdb(pathName=CAE_FILE)


def _imported_set_indices(assembly):
    """Return numeric indices for lower-case imported assembly sets."""
    indices = []
    for name in assembly.sets.keys():
        if not name.startswith('set-'):
            continue
        try:
            index = int(name[4:])
        except (TypeError, ValueError):
            continue
        if index >= 0:
            indices.append(index)
    return sorted(set(indices))


def _cell_key(cell):
    """Return a stable key for an Abaqus geometry cell when available."""
    try:
        return ('index', int(cell.index))
    except Exception:
        try:
            return ('point', tuple(float(value) for value in cell.pointOn[0]))
        except Exception:
            return None


def _element_key(element):
    """Return a deduplication key for an Abaqus mesh element."""
    try:
        instance_name = str(element.instanceName)
    except Exception:
        instance_name = ''
    try:
        return (instance_name, int(element.label))
    except Exception:
        return (instance_name, id(element))


def _set_membership(set_obj):
    """Collect both direct elements and elements associated with geometry cells."""
    element_keys = set()
    cell_keys = set()
    errors = []

    try:
        direct_elements = set_obj.elements
    except AttributeError:
        direct_elements = ()
    except Exception as exc:
        direct_elements = ()
        errors.append("cannot read direct elements (%s)" % str(exc))
    try:
        for element in direct_elements:
            element_keys.add(_element_key(element))
    except Exception as exc:
        errors.append("cannot iterate direct elements (%s)" % str(exc))

    try:
        cells = set_obj.cells
    except AttributeError:
        cells = ()
    except Exception as exc:
        cells = ()
        errors.append("cannot read geometry cells (%s)" % str(exc))

    try:
        for cell in cells:
            key = _cell_key(cell)
            if key is None:
                errors.append("cannot identify a geometry cell")
                continue
            cell_keys.add(key)
            try:
                cell_elements = cell.getElements()
            except Exception as exc:
                errors.append("cell mesh lookup failed (%s)" % str(exc))
                continue
            try:
                for element in cell_elements:
                    element_keys.add(_element_key(element))
            except Exception as exc:
                errors.append("cannot iterate cell mesh elements (%s)" % str(exc))
    except Exception as exc:
        errors.append("cannot iterate geometry cells (%s)" % str(exc))

    return cell_keys, element_keys, errors


def validate_imported_model_ready():
    """Validate the imported-CAD set/mesh contract before generating any output."""
    model = mdb.models['Model-1']
    if 'ImportedPart' not in model.parts.keys():
        print("[VALIDATION] Legacy/non-imported model detected; imported-CAD readiness check not applied.")
        return

    print("[VALIDATION] Imported model readiness check")
    assembly = model.rootAssembly
    indices = _imported_set_indices(assembly)
    errors = []

    if not indices:
        errors.append("No lower-case imported assembly sets named set-0, set-1, ... were found.")
        max_index = None
    else:
        max_index = max(indices)

    if 0 not in indices:
        errors.append("Missing required base set: set-0.")

    if max_index is not None and max_index < 2:
        errors.append("Missing required build-layer and aggregate sets; expected at least set-1 and set-2.")

    if max_index is not None:
        missing = []
        for index in range(0, max_index + 1):
            if index not in indices:
                missing.append('set-%d' % index)
        if missing:
            errors.append("Missing/non-contiguous imported sets: %s." % ', '.join(missing))

    membership = {}
    for index in indices:
        set_name = 'set-%d' % index
        cell_keys, element_keys, membership_errors = _set_membership(assembly.sets[set_name])
        membership[index] = (cell_keys, element_keys)
        for detail in membership_errors:
            errors.append("%s: %s." % (set_name, detail))

    positive_indices = [index for index in indices if index > 0]
    aggregate_index = None
    actual_layers = None
    if positive_indices:
        aggregate_candidates = []
        for candidate in positive_indices:
            candidate_cells = membership[candidate][0]
            if not candidate_cells:
                continue
            union_of_other_cells = set()
            for other in positive_indices:
                if other != candidate:
                    union_of_other_cells.update(membership[other][0])
            if candidate_cells == union_of_other_cells:
                aggregate_candidates.append(candidate)

        # With one build layer, set-1 and set-2 have identical geometry; the
        # contract's required aggregate name resolves that otherwise ambiguous case.
        if (positive_indices == [1, 2] and
                membership[1][0] and membership[1][0] == membership[2][0]):
            aggregate_candidates = [2]

        if len(aggregate_candidates) == 1:
            aggregate_index = aggregate_candidates[0]
            layer_indices = [index for index in positive_indices if index != aggregate_index]
            expected_layers = list(range(1, len(layer_indices) + 1))
            if layer_indices != expected_layers:
                errors.append("Imported layer sets are not contiguous: found %s; expected set-1..set-%d." %
                              (', '.join('set-%d' % index for index in layer_indices), len(layer_indices)))
            if aggregate_index != len(layer_indices) + 1:
                errors.append("Aggregate geometry is named set-%d; expected set-%d for %d layers." %
                              (aggregate_index, len(layer_indices) + 1, len(layer_indices)))
            else:
                actual_layers = len(layer_indices)
        elif len(aggregate_candidates) > 1:
            errors.append("Aggregate geometry is ambiguous; candidate sets: %s." %
                          ', '.join('set-%d' % index for index in aggregate_candidates))
        else:
            # If 1..max are all present but no set represents their union, keep
            # every numbered set as a layer for diagnostics and explicitly name
            # the missing aggregate. Never reinterpret the last layer as it.
            if positive_indices == list(range(1, max(positive_indices) + 1)):
                actual_layers = max(positive_indices)
                errors.append("Missing aggregate set: set-%d does not represent the union of set-1..set-%d geometry." %
                              (actual_layers + 1, actual_layers))
            else:
                errors.append("Could not independently identify an aggregate set as the union of build-layer geometry.")

    if actual_layers is not None:
        print("[VALIDATION] Detected build layers: %d" % actual_layers)

    required_indices = [0] + positive_indices
    if aggregate_index is not None and aggregate_index in required_indices:
        required_indices.remove(aggregate_index)
        required_indices.append(aggregate_index)
    for index in required_indices:
        set_name = 'set-%d' % index
        if set_name not in assembly.sets.keys():
            continue
        element_count = len(membership[index][1])
        label = " (BUILD_ALL)" if aggregate_index is not None and index == aggregate_index else ""
        print("[VALIDATION] %s%s: elements=%d" % (set_name, label, element_count))
        if element_count == 0:
            errors.append("%s has zero mesh elements." % set_name)

    try:
        requested_layers = int(layer_n)
    except NameError:
        requested_layers = None
        errors.append("Input & UTEMP layer_n was not provided.")
    except (TypeError, ValueError):
        requested_layers = None
        errors.append("Input & UTEMP layer_n is not a valid integer: %s." % str(layer_n))

    if requested_layers is not None:
        if actual_layers is None:
            errors.append("Cannot compare layer_n until the CAE layer count and aggregate are established.")
        elif actual_layers != requested_layers:
            print("[VALIDATION] layer_n: %d [MISMATCH]" % requested_layers)
            errors.append("Layer count mismatch: CAE has %d build layers, Input & UTEMP layer_n is %d." %
                          (actual_layers, requested_layers))
            expected_aggregate = 'set-%d' % (requested_layers + 1)
            if (expected_aggregate not in assembly.sets.keys() and
                    all(('set-%d' % index) in assembly.sets.keys()
                        for index in range(0, requested_layers + 1))):
                errors.append("Missing expected aggregate set: %s." % expected_aggregate)
        else:
            print("[VALIDATION] layer_n: %d [OK]" % requested_layers)

    if errors:
        for error in errors:
            print("[VALIDATION][ERROR] %s" % error)
        raise RuntimeError("[VALIDATION] FAIL - model is not ready for input generation.")

    print("[VALIDATION] PASS - model is ready for input generation")


validate_imported_model_ready()


def create_input (temp_step, temp_initial,temp_interval, grad_step, grad_initial, grad_interval):
    for x in range (temp_step):
    
        temp = temp_initial+temp_interval*(x)
    
        for y in range (grad_step):
            temp_gradient = grad_initial+grad_interval*(y)      
            mdb.Job(name=str(temp)+'-'+str(temp_gradient), model='Model-1', description='', type=ANALYSIS, 
                 atTime=None, waitMinutes=0, waitHours=0, queue=None, memory=90, 
                 memoryUnits=PERCENTAGE, getMemoryFromAnalysis=True, 
                 explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, echoPrint=OFF, 
                 modelPrint=OFF, contactPrint=OFF, historyPrint=OFF, userSubroutine='', 
                 scratch='', resultsFormat=ODB, multiprocessingMode=DEFAULT, numCpus=1, 
                 numGPUs=0)
            mdb.jobs[str(temp)+'-'+str(temp_gradient)].writeInput(consistencyChecking=OFF)



session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=105.70832824707, 
    height=122.412033081055)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)

create_input()

#==============================================================================================
#create subroutine files


for x in range (temp_step):

    temp = temp_initial+temp_interval*(x)
    
    for y in range (grad_step):
        temp_gradient = grad_initial+grad_interval*(y)   
        filename = str(temp)+'-'+str(temp_gradient)+'.'+'for'
        with open(filename, 'w') as fid:
            fid.write("      SUBROUTINE UTEMP(TEMP,NSECPT,KSTEP,KINC,TIME,NODE,COORDS)\n")
            fid.write("      INCLUDE 'ABA_PARAM.INC'\n")
            fid.write("      DIMENSION TEMP(NSECPT), TIME(2), COORDS(3)\n")
            fid.write("      TS=80.0\n")
            fid.write("      CC=-1.5\n")
            fid.write("      HC=-4.0\n")
            fid.write("      TEMPT=0.0\n")
            fid.write("      Ly_T=4.0\n")
            fid.write("      A="+str(temp)+"\n")  # maximum temperature on the top layer
            fid.write("      R2="+str(temp_gradient/100.0)+"\n")   # temperature gradient
            fid.write("      Do i = 1, "+str(layer_n+1)+"\n")
            fid.write("      IF(KSTEP==i) THEN\n")
            fid.write("          GT=0\n")
            fid.write("          IF(i==1) THEN\n")    # heat the base plate for the first few layers
            fid.write("             GT=GT+(0)*EXP(-3*((COORDS("+str(int(COORD_IDX))+")-(" \
                      +str(float(AXIS_ZERO))+"+(i-0.5)*"+str(layer_sp)+"))/R2)**2)\n")
            fid.write("             IF(TIME(1) .GE. 0.0 .AND. TIME(1)<1.0) THEN\n")
            fid.write("                 TEMP(1)=(1-EXP(HC*TIME(1)))*GT+TEMPT+TS+50.0\n")
            fid.write("     1            *i**0.3-50.0\n")
            fid.write("             ELSE IF(TIME(1) .GE. 1.0 .AND. TIME(1)<Ly_T-2.0) THEN\n")
            fid.write("                 TEMP(1)=GT+TEMPT+TS+50.0*i**0.3-50\n")
            fid.write("             ELSE IF (TIME(1) .GE. Ly_T-2.0 .AND. TIME(1)<Ly_T-0.5) THEN\n")
            fid.write("                 TEMP(1)=EXP(CC*(TIME(1)-2.0))*GT+TEMPT+TS+50.0\n")
            fid.write("     1            *i**0.3-50.0\n")
            fid.write("             ELSE IF (TIME(1)>3.5) THEN\n")
            fid.write("                 TEMP(1)=TS+50.0*i**0.3-50\n")
            fid.write("             END IF\n")
            fid.write("          ELSE \n")
            fid.write("             GT=GT+(A-((i-1)**0.3*50.0-50.0))*EXP(-3*((COORDS(" \
                      +str(int(COORD_IDX))+")-(" +str(float(AXIS_ZERO))+"+(i-1)*"+str(layer_sp) \
                      +"))/R2)**2)\n")            
            fid.write("             IF(TIME(1) .GE. 0.0 .AND. TIME(1)<1.0) THEN\n")
            fid.write("                 TEMP(1)=(1-EXP(HC*TIME(1)))*GT+TEMPT+TS+50.0*\n")
            fid.write("     1         i**0.3-50.0\n")
            fid.write("             ELSE IF(TIME(1) .GE. 1.0 .AND. TIME(1)<Ly_T-2.0) THEN\n")
            fid.write("                 TEMP(1)=GT+TEMPT+TS+50.0*i**0.3-50.0\n")
            fid.write("             ELSE IF (TIME(1) .GE. Ly_T-2.0 .AND. TIME(1)<Ly_T-0.5) THEN\n")
            fid.write("                 TEMP(1)=EXP(CC*(TIME(1)-2.0))*GT+TEMPT+TS+50.0\n")
            fid.write("     1         *i**0.3-50.0\n")
            fid.write("             ELSE IF (TIME(1)>3.5) THEN\n")
            fid.write("                 TEMP(1)=TS+50.0*i**0.3-50.0\n")
            fid.write("             END IF\n")
            fid.write("          END IF\n")
            fid.write("      END IF\n")
            fid.write("      END Do\n")
            fid.write("      IF (KSTEP=="+str(layer_n+2)+") THEN\n")
            fid.write("          IF (TIME(1)<0.9) THEN\n")
            fid.write("          TEMP(1)=EXP(CC*(TIME(1)))*(TS+50.0*i**0.3-50.0)+25\n")
            fid.write("          ELSE\n")
            fid.write("          TEMP(1)=25\n")
            fid.write("          END IF\n")
            fid.write("      END IF\n")

            # --- NEW: heat-treatment step (only when HT is enabled) ---
            if int(HT_ENABLED) == 1:
                fid.write("      IF (KSTEP=="+str(layer_n+4)+") THEN\n")
                fid.write("          ! Ramp 25C -> T_HT over 0.25 of step time, hold 0.5, cool back in 0.25\n")
                fid.write("          THT = "+("{:.6f}".format(float(HT_TEMP_C)))+"\n")
                fid.write("          TAMB = 25.0\n")
                fid.write("          t = TIME(1)\n")
                fid.write("          IF (t .LT. 0.4D0) THEN\n")
                fid.write("              TEMP(1) = TAMB + (THT - TAMB)*(t/0.4D0)\n")
                fid.write("          ELSEIF (t .LT. 0.6D0) THEN\n")
                fid.write("              TEMP(1) = THT\n")
                fid.write("          ELSE\n")
                fid.write("              TEMP(1) = THT - (THT - TAMB)*((t-0.6D0)/0.4D0)\n")
                fid.write("          ENDIF\n")
                fid.write("      END IF\n")            
            
            fid.write("      RETURN\n")
            fid.write("      END\n")
            
        fid=open('submit.bat','a')
        fid.write("call abq2021 job="+str(temp)+"-"+str(temp_gradient)+" user="+str(temp)+"-"+str(temp_gradient)+" cpus=14 gpus=1 int \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".sta \n")
        # fid.write("del "+str(temp)+"-"+str(temp_gradient)+".dat \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".prt \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".com \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".sim \n")
        # fid.write("del "+str(temp)+"-"+str(temp_gradient)+".msg \n")   
fid.close()