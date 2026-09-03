
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

try:
    HT_STEP_INDEX
except NameError:
    # Legacy/non-imported models retain the historical layer_n + 4 behavior.
    HT_STEP_INDEX = None

# Abaqus job/submission resources injected by the Input & UTEMP GUI.  These
# conservative defaults keep direct/manual execution possible without making
# the GUI's values dependent on hidden hardware detection.
try:
    NUM_CPUS
except NameError:
    NUM_CPUS = 1

try:
    NUM_GPUS
except NameError:
    NUM_GPUS = 0
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
        instance_name = str(cell.instanceName)
    except Exception:
        instance_name = ''
    try:
        return ('index', instance_name, int(cell.index))
    except Exception:
        try:
            return ('point', instance_name,
                    tuple(float(value) for value in cell.pointOn[0]))
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


def _validation_log(message):
    """Write validation status to the GUI-captured Abaqus stream immediately."""
    try:
        sys.stderr.write("%s\n" % message)
        sys.stderr.flush()
    except Exception:
        try:
            print(message)
        except Exception:
            pass
    try:
        report_file = VALIDATION_REPORT_FILE
    except NameError:
        report_file = None
    if report_file:
        try:
            report_stream = open(report_file, 'a')
            try:
                report_stream.write("%s\n" % message)
                report_stream.flush()
            finally:
                report_stream.close()
        except Exception:
            pass


def _assembly_cell_is_meshed(model, assembly_cell):
    """Resolve a dependent assembly cell to its part cell and inspect mesh stats."""
    instance_name = getattr(assembly_cell, 'instanceName', None)
    if not instance_name:
        raise RuntimeError("assembly cell has no instanceName")
    instance = model.rootAssembly.instances[instance_name]
    part_name = getattr(instance, 'partName', None)
    if not part_name:
        raise RuntimeError("instance %s has no partName" % instance_name)
    part = model.parts[part_name]
    cell_index = int(assembly_cell.index)
    part_cell = part.cells[cell_index]
    stats = part.getMeshStats(regions=(part_cell,))
    return int(stats.numMeshedRegions) > 0


def _set_membership(set_obj, model):
    """Collect direct mesh membership and per-cell mesh completeness separately."""
    element_keys = set()
    cell_keys = set()
    errors = []
    meshed_cell_count = 0
    unverified_cell_count = 0
    direct_mesh_status = 'ok'

    try:
        direct_elements = set_obj.elements
    except Exception as exc:
        direct_mesh_status = 'access_error'
        errors.append("cannot access direct mesh elements; inspect set.elements (%s)" % str(exc))
    else:
        try:
            for element in direct_elements:
                element_keys.add(_element_key(element))
        except Exception as exc:
            direct_mesh_status = 'iterate_error'
            errors.append("cannot iterate direct mesh elements; inspect set.elements (%s)" % str(exc))

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
            if key in cell_keys:
                continue
            cell_keys.add(key)
            try:
                if _assembly_cell_is_meshed(model, cell):
                    meshed_cell_count += 1
            except Exception as exc:
                unverified_cell_count += 1
                errors.append("geometry cell mesh association could not be verified (%s)" % str(exc))
    except Exception as exc:
        errors.append("cannot iterate geometry cells (%s)" % str(exc))

    return (cell_keys, element_keys, meshed_cell_count,
            unverified_cell_count, direct_mesh_status, errors)


def _mesh_quality_summary(assembly):
    """Return Abaqus Verify Mesh analysis-check counts for an assembly."""
    try:
        quality = assembly.verifyMeshQuality(ANALYSIS_CHECKS)
    except Exception as exc:
        return None, "cannot run Abaqus analysis mesh checks (%s)" % str(exc)
    if not isinstance(quality, dict):
        return None, "Abaqus analysis mesh checks returned an unexpected result (%s)" % type(quality)

    try:
        total_elements = int(quality['numElements'])
    except Exception as exc:
        return None, "Abaqus analysis mesh checks did not return numElements (%s)" % str(exc)

    counts = {'total_elements': total_elements}
    for result_key, count_key in (('failedElements', 'analysis_errors'),
                                  ('warningElements', 'analysis_warnings'),
                                  ('naElements', 'not_applicable')):
        try:
            counts[count_key] = len(quality[result_key])
        except Exception as exc:
            return None, "Abaqus analysis mesh checks did not return %s (%s)" % (result_key, str(exc))
    return counts, None


def _resolve_heat_treatment_step(model, layer_count):
    """Find the CAE heat-treatment step after the configured removal sequence."""
    prefixes = {}
    for step_name in model.steps.keys():
        for prefix in ('Step', 'BStep'):
            marker = prefix + '-'
            if not step_name.startswith(marker):
                continue
            try:
                index = int(step_name[len(marker):])
            except (TypeError, ValueError):
                continue
            prefixes.setdefault(prefix, []).append(index)

    first_post_removal = int(layer_count) + 4
    step_sequences = {}
    for prefix, indices in prefixes.items():
        indices = sorted(set(indices))
        if any(index not in indices for index in range(1, first_post_removal)):
            continue
        post_removal = [index for index in indices if index >= first_post_removal]
        if post_removal and post_removal == list(range(first_post_removal, max(post_removal) + 1)):
            step_sequences[prefix] = (indices, post_removal)

    def _created_step_name(interaction):
        try:
            history = list(interaction.history)
            step_names = list(model.steps.keys())
        except Exception:
            return None
        try:
            created_state = CREATED
        except NameError:
            created_state = None
        for history_index, state in enumerate(history):
            try:
                is_created = str(state) == 'CREATED'
            except Exception:
                is_created = False
            if created_state is not None:
                try:
                    is_created = is_created or state == created_state
                except Exception:
                    pass
            if not is_created or history_index >= len(step_names):
                continue
            return step_names[history_index]
        return None

    # ModelChange history is the source of truth for the final removal step.
    # This is required for BStep-* fallback sequences, whose generated steps
    # use the default time period and therefore have no unique HT period marker.
    removal_steps = {}
    try:
        for interaction_name in model.interactions.keys():
            is_base_removal = interaction_name == 'Int-%d' % (int(layer_count) + 2)
            is_bottom_removal = interaction_name.startswith('Int-bottom-')
            if not (is_base_removal or is_bottom_removal):
                continue
            step_name = _created_step_name(model.interactions[interaction_name])
            if step_name is None:
                continue
            for prefix in ('Step', 'BStep'):
                marker = prefix + '-'
                if not step_name.startswith(marker):
                    continue
                try:
                    index = int(step_name[len(marker):])
                except (TypeError, ValueError):
                    continue
                removal_steps.setdefault(prefix, []).append(index)
    except Exception:
        removal_steps = {}

    candidates = []
    for prefix, sequence in step_sequences.items():
        indices, post_removal = sequence
        candidate_index = max(post_removal)
        referenced_removals = sorted(set(removal_steps.get(prefix, [])))
        if referenced_removals:
            # The base removal must be present, and the HT step must be the
            # distinct final step immediately following all CAE removals.
            if int(layer_count) + 3 not in referenced_removals:
                continue
            if candidate_index != max(referenced_removals) + 1:
                continue
            candidates.append((prefix, candidate_index))

    if len(candidates) == 1:
        return candidates[0][1], None
    if len(candidates) > 1:
        return None, "multiple compatible heat-treatment step sequences were found: %s" % \
            ', '.join('%s-%d' % candidate for candidate in candidates)
    return None, "no distinct post-removal heat-treatment step was found in the CAE model (expected the final %s-%d-style step after the removal interactions)" % \
        ('Step', first_post_removal)


def validate_imported_model_ready():
    """Validate the imported-CAD set/mesh contract before generating any output."""
    model = mdb.models['Model-1']
    if 'ImportedPart' not in model.parts.keys():
        _validation_log("[VALIDATION] Legacy/non-imported model detected; imported-CAD readiness check not applied.")
        return

    _validation_log("[VALIDATION] Imported model readiness check")
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
        (cell_keys, element_keys, meshed_cell_count,
         unverified_cell_count, direct_mesh_status,
         membership_errors) = _set_membership(assembly.sets[set_name], model)
        membership[index] = (cell_keys, element_keys, meshed_cell_count,
                             unverified_cell_count, direct_mesh_status)
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
        _validation_log("[VALIDATION] Detected build layers: %d" % actual_layers)

    required_indices = [0] + positive_indices
    if aggregate_index is not None and aggregate_index in required_indices:
        required_indices.remove(aggregate_index)
        required_indices.append(aggregate_index)
    for index in required_indices:
        set_name = 'set-%d' % index
        if set_name not in assembly.sets.keys():
            continue
        element_count = len(membership[index][1])
        cell_count = len(membership[index][0])
        meshed_cell_count = membership[index][2]
        unverified_cell_count = membership[index][3]
        direct_mesh_status = membership[index][4]
        label = " (BUILD_ALL)" if aggregate_index is not None and index == aggregate_index else ""
        _validation_log("[VALIDATION] %s%s: cells=%d, meshed_cells=%d, elements=%d" %
                        (set_name, label, cell_count, meshed_cell_count, element_count))
        if direct_mesh_status == 'ok' and element_count == 0:
            errors.append("%s has zero mesh elements." % set_name)
        if unverified_cell_count == 0 and cell_count > 0 and meshed_cell_count < cell_count:
            missing_cell_count = cell_count - meshed_cell_count
            if meshed_cell_count > 0:
                errors.append("%s is partially meshed: %d of %d geometry cells has no mesh elements." %
                              (set_name, missing_cell_count, cell_count))
            elif element_count > 0:
                errors.append("%s is completely unmeshed by per-cell mesh association: %d geometry cells have no mesh elements." %
                              (set_name, cell_count))

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
            _validation_log("[VALIDATION] layer_n: %d [MISMATCH]" % requested_layers)
            errors.append("Layer count mismatch: CAE has %d build layers, Input & UTEMP layer_n is %d." %
                          (actual_layers, requested_layers))
            expected_aggregate = 'set-%d' % (requested_layers + 1)
            if (expected_aggregate not in assembly.sets.keys() and
                    all(('set-%d' % index) in assembly.sets.keys()
                        for index in range(0, requested_layers + 1))):
                errors.append("Missing expected aggregate set: %s." % expected_aggregate)
        else:
            _validation_log("[VALIDATION] layer_n: %d [OK]" % requested_layers)

    quality, quality_error = _mesh_quality_summary(assembly)
    if quality_error is not None:
        errors.append("Mesh analysis checks unavailable: %s." % quality_error)
    else:
        total_elements = quality['total_elements']
        analysis_errors = quality['analysis_errors']
        analysis_warnings = quality['analysis_warnings']
        not_applicable = quality['not_applicable']
        _validation_log("[MESH] Total elements: %d" % total_elements)
        _validation_log("[MESH] Analysis errors: %d [%s]" %
                        (analysis_errors, 'FAIL' if analysis_errors else 'OK'))
        _validation_log("[MESH] Analysis warnings: %d [%s]" %
                        (analysis_warnings, 'WARNING' if analysis_warnings else 'OK'))
        if total_elements > 0:
            _validation_log("[MESH] Warning fraction: %.2f%%" %
                            (100.0 * analysis_warnings / total_elements))
        if not_applicable:
            _validation_log("[MESH] Not-applicable elements: %d" % not_applicable)
        if analysis_errors:
            errors.append("Mesh contains %d elements that fail Abaqus analysis checks." % analysis_errors)

    heat_treatment_step = None
    if int(HT_ENABLED) == 1:
        if actual_layers is None:
            errors.append("Cannot resolve the heat-treatment step until the CAE layer count is established.")
        else:
            heat_treatment_step, heat_treatment_error = _resolve_heat_treatment_step(model, actual_layers)
            if heat_treatment_error is not None:
                errors.append("Heat-treatment step unavailable: %s." % heat_treatment_error)
            else:
                _validation_log("[VALIDATION] Heat-treatment step: KSTEP=%d [OK]" % heat_treatment_step)

    if errors:
        failure_lines = ["[VALIDATION] FAIL - model is not ready for input generation."]
        _validation_log(failure_lines[0])
        for error in errors:
            detail = "[VALIDATION][ERROR] %s" % error
            _validation_log(detail)
            failure_lines.append(detail)
        for stream in (sys.stdout, sys.stderr):
            try:
                stream.flush()
            except Exception:
                pass
        raise RuntimeError("\n".join(failure_lines))

    globals()['HT_STEP_INDEX'] = heat_treatment_step
    _validation_log("[VALIDATION] PASS - model is ready for input generation")


validate_imported_model_ready()


def create_input (temp_step, temp_initial,temp_interval, grad_step, grad_initial, grad_interval):
    # Start each generation with a fresh submission script.  The sweep below
    # intentionally appends one command per generated job.
    with open('submit.bat', 'w') as fid:
        pass

    for x in range (temp_step):
    
        temp = temp_initial+temp_interval*(x)
    
        for y in range (grad_step):
            temp_gradient = grad_initial+grad_interval*(y)      
            mdb.Job(name=str(temp)+'-'+str(temp_gradient), model='Model-1', description='', type=ANALYSIS, 
                 atTime=None, waitMinutes=0, waitHours=0, queue=None, memory=90, 
                 memoryUnits=PERCENTAGE, getMemoryFromAnalysis=True, 
                 explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, echoPrint=OFF, 
                 modelPrint=OFF, contactPrint=OFF, historyPrint=OFF, userSubroutine='', 
                 scratch='', resultsFormat=ODB, multiprocessingMode=DEFAULT, numCpus=NUM_CPUS,
                 numGPUs=NUM_GPUS)
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
                ht_step_index = HT_STEP_INDEX
                if ht_step_index is None:
                    ht_step_index = layer_n + 4
                fid.write("      IF (KSTEP=="+str(ht_step_index)+") THEN\n")
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
        fid.write("call abq2021 job="+str(temp)+"-"+str(temp_gradient)+" user="+str(temp)+"-"+str(temp_gradient)+" cpus="+str(NUM_CPUS)+" gpus="+str(NUM_GPUS)+" int \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".sta \n")
        # fid.write("del "+str(temp)+"-"+str(temp_gradient)+".dat \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".prt \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".com \n")
        fid.write("del "+str(temp)+"-"+str(temp_gradient)+".sim \n")
        # fid.write("del "+str(temp)+"-"+str(temp_gradient)+".msg \n")   
fid.close()
