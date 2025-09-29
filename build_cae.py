# -*- coding: utf-8 -*-
"""
Created on Fri Feb 10 19:56:37 2023

@author: ruiyao.zhang
"""

# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2021 replay file
# Internal Version: 2020_03_06-22.50.37 167380
# Run by ruiyao.zhang on Fri Feb 10 19:56:23 2023
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
#: Warning: Permission was denied for "abaqus.rpy"; "abaqus.rpy.135" will be used for this session's replay file.
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=129.0, 
    height=120.40161895752)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
#=================================================================================
# PART-0 Input variables

#=================================================================================

savepathName = R'D:\1_Project\AM_modelGUI\GUI\AM900_' # cae save path

#=================================================================================
# PART-1 Define build shape, height and layer thickness
#shape index: 1 for L-shape, 2 for square, 3 for rectangle, 4 for cylinder, 5 for tube
#=================================================================================

shape_index = 3 # select the shape using the index below

if shape_index == 1:
    shape_type = 'L'
elif shape_index == 2:
    shape_type = 'square'
elif shape_index == 3:
    shape_type = 'rectangle'
elif shape_index == 4:
    shape_type = 'cylinder'
elif shape_index == 5:
    shape_type = 'tube'
    
build_height = 12.0   # the hieght of the build part, unit mm
layer_thickness = 0.5
layer_number = int(build_height / layer_thickness)
baseplane_offset = 0.0 # the build bottom coordinate

#=================================================================================
# PART-2 create build part
#=================================================================================

s = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
    sheetSize=200.0)
g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
s.setPrimaryObject(option=STANDALONE)
s.rectangle(point1=(-12.5, -15.0), point2=(12.5, build_height))



p = mdb.models['Model-1'].Part(name='Part-1', dimensionality=THREE_D, 
    type=DEFORMABLE_BODY)
p = mdb.models['Model-1'].parts['Part-1']
p.BaseSolidExtrude(sketch=s, depth=25.0)
s.unsetPrimaryObject()

session.viewports['Viewport: 1'].setValues(displayedObject=p)
del mdb.models['Model-1'].sketches['__profile__']

f1, e1 = p.faces, p.edges
t = p.MakeSketchTransform(sketchPlane=f1[1], sketchUpEdge=e1[6], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.0, 12.0, 12.5))
s = mdb.models['Model-1'].ConstrainedSketch(name='__profile__', 
    sheetSize=74.67, gridSpacing=1.86, transform=t)
g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
s.setPrimaryObject(option=SUPERIMPOSE)

p.projectReferencesOntoSketch(sketch=s, filter=COPLANAR_EDGES)
s.rectangle(point1=(-12.5, -12.5), point2=(12.5, 12.5))

if shape_index == 1: # for L shape
    
    s.Line(point1=(-10, -10), point2=(10, -10))
    s.HorizontalConstraint(entity=g[6], addUndoState=False)
    s.Line(point1=(10, -10), point2=(10, -2))
    s.VerticalConstraint(entity=g[7], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[6], entity2=g[7], addUndoState=False)
    s.Line(point1=(10,-2), point2=(2,-2))
    s.HorizontalConstraint(entity=g[8], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[7], entity2=g[8], addUndoState=False)
    s.Line(point1=(2,-2), point2=(2,10))
    s.VerticalConstraint(entity=g[9], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[8], entity2=g[9], addUndoState=False)
    s.Line(point1=(2,10), point2=(-10,10))
    s.HorizontalConstraint(entity=g[10], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[9], entity2=g[10], addUndoState=False)
    s.Line(point1=(-10,10), point2=(-10,-10))
    s.VerticalConstraint(entity=g[11], addUndoState=False)
    s.PerpendicularConstraint(entity1=g[10], entity2=g[11], addUndoState=False)

elif shape_index == 2: # for square
    s.rectangle(point1=(-10.0, -10.0), point2=(10.0, 10.0)) 
    
elif shape_index == 3: # for rectangle
    s.rectangle(point1=(-7.5, -5), point2=(7.5, 5.0)) 
    
elif shape_index == 4: # for cylinder
    s.CircleByCenterPerimeter(center=(0.0, 0.0), point1=(10.0, 0.0)) 

elif shape_index == 5: # for tube 
    s.CircleByCenterPerimeter(center=(0.0, 0.0), point1=(10.0, 0.0)) 
    s.CircleByCenterPerimeter(center=(0.0, 0.0), point1=(5.0, 0.0)) 


f, e = p.faces, p.edges
p.CutExtrude(sketchPlane=f[1], sketchUpEdge=e[6], sketchPlaneSide=SIDE1, 
    sketchOrientation=RIGHT, sketch=s, depth=build_height, flipExtrudeDirection=OFF)
s.unsetPrimaryObject()
del mdb.models['Model-1'].sketches['__profile__']

#======================================================================================
# PART-3 create datum planes and partition layers
#======================================================================================


for i in range(layer_number):
    p.DatumPlaneByPrincipalPlane(offset=i*layer_thickness + baseplane_offset, principalPlane=XZPLANE)

p.DatumPlaneByPrincipalPlane(offset=12.5, principalPlane=XYPLANE)


for j in range(layer_number):    
    p.PartitionCellByDatumPlane(cells= p.cells.findAt(((0,build_height - 0.1 ,3),),((0,build_height - 0.1 ,12.5),),((0,build_height - 0.1 ,15),), ), datumPlane=p.datums[j + 3])    

p.PartitionCellByDatumPlane(cells= p.cells.getByBoundingBox(-500,0,-500,500,500,500), datumPlane=p.datums[layer_number + 3]) 

e = p.edges
pickedEdges = e.findAt(((-12.5, -15, 12.5),), ((-12.5, 0, 12.5),), 
                       (( 12.5, -15, 12.5),), (( 12.5, 0, 12.5),),
                       ((    0, -15,    0),), ((    0, 0,    0),), 
                       ((    0, -15,   25),), ((    0, 0,   25),),)
p.PartitionEdgeByParam(edges=pickedEdges, parameter=0.5)


#================================================================================
# PART-4 create materials properties
#================================================================================
session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
    engineeringFeatures=ON)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=OFF)
mdb.models['Model-1'].Material(name='base_material')
mdb.models['Model-1'].materials['base_material'].Elastic(
    temperatureDependency=ON, table=((206460.0, 0.312, 25.0), (202590.0, 0.314, 100.0), 
    (197370.0, 0.317, 200.0), (191790.0, 0.32, 300.0), (186030.0, 0.322, 400.0), 
    (180000.0, 0.325, 500.0), (173790.0, 0.328, 600.0), (165510.0, 0.331, 700.0), 
    (156420.0, 0.334, 800.0), (145440.0, 0.339, 900.0), (135000.0, 0.344, 
    1000.0), (121500.0, 0.351, 1100.0), (10917.0, 0.357, 1200.0), (1467.0, 
    0.38, 1300.0), (0.1, 0.38, 1330.0)))
mdb.models['Model-1'].materials['base_material'].Expansion(table=((1.11e-05, 
    25.0), (1.14e-05, 100.0), (1.19e-05, 200.0), (1.24e-05, 300.0), (1.28e-05, 
    400.0), (1.33e-05, 500.0), (1.37e-05, 600.0), (1.47e-05, 700.0), (1.58e-05, 
    800.0), (1.76e-05, 900.0), (1.89e-05, 1000.0), (2.06e-05, 1100.0), (
    2.17e-05, 1200.0), (2.58e-05, 1300.0)), temperatureDependency=ON)
mdb.models['Model-1'].materials['base_material'].Plastic(
    temperatureDependency=ON, table=((400.0, 0.0, 25.0), (620.0, 0.05, 25.0), (
    370.0, 0.0, 100.0), (580.0, 0.05, 100.0), (350.0, 0.0, 200.0), (550.0, 
    0.05, 200.0), (330.0, 0.0, 400.0), (520.0, 0.05, 400.0), (80.0, 0.0, 
    800.0), (130.0, 0.05, 800.0), (15.0, 0.0, 1200.0)))
mdb.models['Model-1'].materials['base_material'].plastic.AnnealTemperature(
    table=((1200.0, ), ))

mdb.models['Model-1'].Material(name='additive_material')
mdb.models['Model-1'].materials['additive_material'].Elastic(
    temperatureDependency=ON, table=((206460.0, 0.312, 25.0), (202590.0, 0.314, 100.0), 
    (197370.0, 0.317, 200.0), (191790.0, 0.32, 300.0), (186030.0, 0.322, 400.0), 
    (180000.0, 0.325, 500.0), (173790.0, 0.328, 600.0), (165510.0, 0.331, 700.0), 
    (156420.0, 0.334, 800.0), (145440.0, 0.339, 900.0), (135000.0, 0.344, 
    1000.0), (121500.0, 0.351, 1100.0), (10917.0, 0.357, 1200.0), (1467.0, 
    0.38, 1300.0), (0.1, 0.38, 1330.0)))
mdb.models['Model-1'].materials['additive_material'].Expansion(table=((
    1.11e-05, 25.0), (1.14e-05, 100.0), (1.19e-05, 200.0), (1.24e-05, 300.0), (
    1.28e-05, 400.0), (1.33e-05, 500.0), (1.37e-05, 600.0), (1.47e-05, 700.0), 
    (1.58e-05, 800.0), (1.76e-05, 900.0), (1.89e-05, 1000.0), (2.06e-05, 
    1100.0), (2.17e-05, 1200.0), (2.58e-05, 1300.0)), temperatureDependency=ON)
mdb.models['Model-1'].materials['additive_material'].Plastic(
    temperatureDependency=ON, table=((713.0, 0.0, 25.0), (1238.0, 0.35, 25.0), 
    (686.0, 0.0, 100.0), (1226.0, 0.35, 100.0), (663.0, 0.0, 200.0), 
    (1213.0, 0.35, 200.0), (646.0, 0.0, 300.0), (1193.0, 0.35, 300.0), 
    (634.0, 0.0, 400.0), (1164.0, 0.35, 400.0), (624.0, 0.0, 500.0), 
    (1123.0, 0.35, 500.0), (615.0, 0.0, 600.0), (1072.0, 0.35, 600.0), 
    (606.0, 0.0, 700.0), (1011.0, 0.35, 700.0), (587.0, 0.0, 800.0), 
    (922.0, 0.35, 800.0), (532.0, 0.0, 900.0), (792.0, 0.35, 900.0), 
    (324.0, 0.0, 1000.0), (395.0, 0.35, 1000.0), (204.0, 0.0, 1050.0), 
    (259.0, 0.35, 1050.0), (113.0, 0.0, 1100.0), (151.0, 0.35, 1100.0), (19.0, 
    0.0, 1150.0), (32.0, 0.35, 1150.0), (14.0, 0.0, 1200.0)))

mdb.models['Model-1'].materials['additive_material'].plastic.AnnealTemperature(
    table=((1200.0, ), ))

#======================================================================================
# PART-5 Create assembly sets, set-0 is the base, set-1 to layer_number is for each layer, 
# set-layer_number+1 is for the whole build-part
#======================================================================================
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    optimizationTasks=OFF, geometricRestrictions=OFF, stopConditions=OFF)
a = mdb.models['Model-1'].rootAssembly
#a.DatumCsysByDefault(CARTESIAN)

a.Instance(name='Part-1-1', part=p, dependent=ON)
# set for base
a.Set(cells=a.instances['Part-1-1'].cells.getByBoundingBox(-500,-500,-500,500,0,500), name='Set-0') 
# set for each layer
for i in range(layer_number):    
    a.Set(cells=
    a.instances['Part-1-1'].cells.getByBoundingBox(-500,i*layer_thickness,-500,500,(i+1)*layer_thickness,500), name='Set-'+str(i+1))

c1 = a.instances['Part-1-1'].cells
#getByBoundingBox(xmin,ymin,zmin,xmax,ymax,zmax)
cells1 = c1.getByBoundingBox(-12.5, 0.0, 0, 12.5, build_height, 25) 
a.Set(cells=cells1, name='Set-'+str(layer_number+1)) # set for the whole build-part

cells2 = c1.getByBoundingBox(-12.5, 0.0, 12.5, 12.5, build_height, 25) 
a.Set(cells=cells2, name='Set-'+str(layer_number+2)) # set for the half build-part

#======================================================================================
# PART-6 Create part geometry sets, set-0 for the base and set-1 for the build-part
#======================================================================================



c = p.cells
cells = c.getByBoundingBox(-12.5,-15,0,12.5,0,25)
p.Set(cells=cells, name='Set-0')

c = p.cells
cells = c.getByBoundingBox(-12.5,0,0,12.5,build_height,25)
p.Set(cells=cells, name='Set-1')

#======================================================================================
# PART-7 Create sections, assign sections and properties
#======================================================================================
session.viewports['Viewport: 1'].setValues(displayedObject=p)
mdb.models['Model-1'].HomogeneousSolidSection(name='base', 
    material='base_material', thickness=None)

region = p.sets['Set-0']

p.SectionAssignment(region=region, sectionName='base', offset=0.0, 
    offsetType=MIDDLE_SURFACE, offsetField='', 
    thicknessAssignment=FROM_SECTION)
mdb.models['Model-1'].HomogeneousSolidSection(name='additive', 
    material='additive_material', thickness=None)

region = p.sets['Set-1']

p.SectionAssignment(region=region, sectionName='additive', offset=0.0, 
    offsetType=MIDDLE_SURFACE, offsetField='', 
    thicknessAssignment=FROM_SECTION)


#======================================================================================
# PART-8 Create steps
# Step-1 for the base layer
# Step-2 to layer_number+1 for each build layer
# Step-layer_number+2 for cooling to room temperature
# Step-layer_number+3 for base removal
# Step-lyaer_number+4 for build part half -- not implemented
# create fieldOutputRequests
#======================================================================================
for i in range(layer_number+3):
    if i == 0:
        mdb.models['Model-1'].StaticStep(initialInc=0.08, maxInc=0.3, maxNumInc=10000, 
        minInc=0.0002, name='Step-1', previous='Initial', timePeriod=4.0) # Create the step 1 with the privious step being initial
    elif i < layer_number+1:
        mdb.models['Model-1'].StaticStep(initialInc=0.08, maxInc=0.3, maxNumInc=10000, 
        minInc=0.0002, name='Step-'+str(i+1), previous='Step-'+str(i), timePeriod=4.0)
    elif i == layer_number+1:
        mdb.models['Model-1'].StaticStep(initialInc=0.1, maxInc=1.0, maxNumInc=10000, 
        minInc=0.0002, name='Step-'+str(i+1), previous='Step-'+str(i), timePeriod=1.0)
    else:
        mdb.models['Model-1'].StaticStep(initialInc=1.0, maxInc=1.0, maxNumInc=10000, 
        minInc=0.0002, name='Step-'+str(i+1), previous='Step-'+str(i), timePeriod=1.0)
        
mdb.models['Model-1'].fieldOutputRequests['F-Output-1'].setValues(timeInterval=
    0.8, timeMarks=OFF, variables=('S', 'NT', 'U'))

#======================================================================================
# PART-9 Interactions
# Deactivate the build part
#======================================================================================
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
a1 = mdb.models['Model-1'].rootAssembly
region =a1.sets['Set-'+str(layer_number+1)]
mdb.models['Model-1'].ModelChange(name='Int-1', createStepName='Step-1', 
    region=region, activeInStep=False, includeStrain=False)

# Reactivate each layer incrementally
#======================================================================================
for i in range(layer_number):    
    mdb.models['Model-1'].ModelChange(activeInStep=True, createStepName='Step-'+str(i+2), 
    includeStrain=False, name='Int-'+str(i+2), region=
    mdb.models['Model-1'].rootAssembly.sets['Set-'+str(i+1)])

# Deactivate the base part
#======================================================================================
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-'+str(layer_number+3))
a1 = mdb.models['Model-1'].rootAssembly
region =a1.sets['Set-0']
mdb.models['Model-1'].ModelChange(name='Int-'+str(layer_number+2), createStepName='Step-'+str(layer_number+3), 
    region=region, activeInStep=False, includeStrain=False)

#======================================================================================
# PART-10 Mesh
#======================================================================================

session.viewports['Viewport: 1'].setValues(displayedObject=p)
session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=OFF, 
    engineeringFeatures=OFF, mesh=ON)
session.viewports['Viewport: 1'].partDisplay.meshOptions.setValues(
    meshTechnique=ON)
session.viewports['Viewport: 1'].view.setProjection(projection=PARALLEL)

session.viewports['Viewport: 1'].view.setValues(session.views['Front'])

e = p.edges
pickedEdges1 = e.findAt(((-12.5,-10,25 ), ),((-12.5,-10,25 ), ),((-12.5,-10,25 ), ),((-12.5,-10,25 ), ),)
pickedEdges2 = e.findAt(((-12.5,-5,25 ), ),((-12.5,-5,25 ), ),((-12.5,-5,25 ), ),((-12.5,-5,25 ), ),)
p.seedEdgeByBias(biasMethod=SINGLE, end2Edges=pickedEdges2,
     minSize=0.5, maxSize=4.0, constraint=FINER)

session.viewports['Viewport: 1'].view.setValues(session.views['Top'])
e = p.edges
pickedEdges = e.findAt(((-12.5,0,11.5 ), ),((-12.5,-15,11.5 ), ),((12.5,0,11.5 ), ),((12.5,-15,11.5 ), ),((-1,0,0 ), ),((-1,-15,0 ), ),((-1,0,25 ), ),((-1,-15,25 ), ),
                       ((-12.5,0,13.5 ), ),((-12.5,-15,13.5 ), ),((12.5,0,13.5 ), ),((12.5,-15,13.5 ), ),(( 1,0,0 ), ),(( 1,-15,0 ), ),(( 1,0,25 ), ),(( 1,-15,25 ), ),)
p.seedEdgeBySize(edges=pickedEdges, size=2.0, deviationFactor=0.1, 
    constraint=FINER)

# create global seed size
p.seedPart(size=0.5, deviationFactor=0.1, minSizeFactor=0.1)

p.generateMesh()

# Deactive geometry set for build part half -- not implemented
#======================================================================================
# session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=OFF, 
#     interactions=ON, constraints=ON, connectors=ON, engineeringFeatures=ON)
# session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
#     meshTechnique=OFF)
# session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-'+str(layer_number+4))
# a3 = mdb.models['Model-1'].rootAssembly
# region =a3.sets['Set-'+str(layer_number+2)]
# mdb.models['Model-1'].ModelChange(name='Int-'+str(layer_number+3), createStepName='Step-'+str(layer_number+4), 
#     region=region, regionType=ELEMENTS, activeInStep=False, 
#     includeStrain=False)

#======================================================================================
# PART-11 predefined temperature 
# 25 C global temperature
#======================================================================================
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
a4 = mdb.models['Model-1'].rootAssembly
c1 = a4.instances['Part-1-1'].cells
cells1 = c1.getByBoundingBox(-12.5,-15,0,12.5,build_height,25)
f1 = a4.instances['Part-1-1'].faces
faces1 = f1.getByBoundingBox(-12.5,-15,0,12.5,build_height,25)
e1 = a4.instances['Part-1-1'].edges
edges1 = e1.getByBoundingBox(-12.5,-15,0,12.5,build_height,25)
v1 = a4.instances['Part-1-1'].vertices
verts1 = v1.getByBoundingBox(-12.5,-15,0,12.5,build_height,25)
region = a4.Set(vertices=verts1, edges=edges1, faces=faces1, cells=cells1, 
    name='Set-'+str(layer_number+3))
mdb.models['Model-1'].Temperature(name='Predefined Field-1', 
    createStepName='Initial', region=region, distributionType=UNIFORM, 
    crossSectionDistribution=CONSTANT_THROUGH_THICKNESS, magnitudes=(25.0, ))
#: A field was created, but its display is currently turned off.
#: Use "View->Assembly Display Options" to turn on the display of fields.

# predefined temperature subroutine
#======================================================================================
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
a4 = mdb.models['Model-1'].rootAssembly
region = a4.sets['Set-'+str(layer_number+3)]
mdb.models['Model-1'].Temperature(name='Predefined Field-2', 
    createStepName='Step-1', region=region, distributionType=USER_DEFINED)
#: A field was created, but its display is currently turned off.
#: Use "View->Assembly Display Options" to turn on the display of fields.

#======================================================================================
# PART-11 Create rigid body constraint
#======================================================================================

v1 = a4.instances['Part-1-1'].vertices
verts1 = v1.findAt(((0,-15,0),),((0,-15,25),),) # two points constrained in X
region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+4))
mdb.models['Model-1'].DisplacementBC(name='BC-1', createStepName='Step-1', 
    region=region, u1=0.0, u2=UNSET, u3=UNSET, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
    amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
    localCsys=None)
#: A boundary condition was created, but its display is currently turned off.
#: Use "View->Assembly Display Options" to turn on the display of boundary conditions.
verts1 = v1.findAt(((-12.5,-15,0),),((12.5,-15,0),),((-12.5,-15,25),),((12.5,-15,25),),) # 4 points constrained in Y
region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+5))
mdb.models['Model-1'].DisplacementBC(name='BC-2', createStepName='Step-1', 
    region=region, u1=UNSET, u2=0.0, u3=UNSET, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
    amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
    localCsys=None)

verts1 = v1.findAt(((-12.5,-15,12.5),),((12.5,-15,12.5),),) # 2 points constrained in Z
region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+6))
mdb.models['Model-1'].DisplacementBC(name='BC-3', createStepName='Step-1', 
    region=region, u1=UNSET, u2=UNSET, u3=0.0, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
    amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
    localCsys=None)
#: A boundary condition was created, but its display is currently turned off.
#: Use "View->Assembly Display Options" to turn on the display of boundary conditions.

session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-'+str(layer_number+3))
session.viewports['Viewport: 1'].view.setValues(nearPlane=57.0251, 
    farPlane=133.735, cameraPosition=(60.4699, 34.1623, -52.169), 
    cameraUpVector=(-0.180837, 0.910015, 0.373055), cameraTarget=(-0.947622, 
    -3.28515, 9.40685))
session.viewports['Viewport: 1'].view.setValues(nearPlane=61.7253, 
    farPlane=129.034, width=68.1123, height=28.35, cameraPosition=(65.3938, 
    42.1876, -42.3772), cameraTarget=(3.97626, 4.74014, 19.1987))
a4 = mdb.models['Model-1'].rootAssembly
v1 = a4.instances['Part-1-1'].vertices



#==================================================================================
# rigid body constraints for half model removal

# #verts1 = v1.findAt(((-10.0,0,12.5),),((10.0,0,12.5),),((-10.0,build_height,12.5),),)
# verts1 = v1.findAt(((-5.0,0,12.5),),((5.0,0,12.5),),((-5.0,build_height,12.5),),) # 3 points at the mid-cut constrained in Z
# region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+7))
# mdb.models['Model-1'].DisplacementBC(name='BC-4', createStepName='Step-'+str(layer_number+3), 
#     region=region, u1=UNSET, u2=UNSET, u3=0.0, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
#     amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
#     localCsys=None)
# #: A boundary condition was created, but its display is currently turned off.
# #: Use "View->Assembly Display Options" to turn on the display of boundary conditions.
# a4 = mdb.models['Model-1'].rootAssembly
# v1 = a4.instances['Part-1-1'].vertices
# #verts1 = v1.findAt(((-10.0,0,12.5),),((10.0,0,12.5),),)
# verts1 = v1.findAt(((-5.0,0,12.5),),((5.0,0,12.5),),) # 2 points at the mid-cut constrained in Y
# region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+8))
# mdb.models['Model-1'].DisplacementBC(name='BC-5', createStepName='Step-'+str(layer_number+3), 
#     region=region, u1=UNSET, u2=0.0, u3=UNSET, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
#     amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
#     localCsys=None)
# #: A boundary condition was created, but its display is currently turned off.
# #: Use "View->Assembly Display Options" to turn on the display of boundary conditions.
# a4 = mdb.models['Model-1'].rootAssembly
# v1 = a4.instances['Part-1-1'].vertices
# #verts1 = v1.findAt(((-10.0,0,12.5),),)
# verts1 = v1.findAt(((-5.0,0,12.5),),) # 1 point at the mid-cut constrained in Z
# region = a4.Set(vertices=verts1, name='Set-'+str(layer_number+9))
# mdb.models['Model-1'].DisplacementBC(name='BC-6', createStepName='Step-'+str(layer_number+3), 
#     region=region, u1=0.0, u2=UNSET, u3=UNSET, ur1=UNSET, ur2=UNSET, ur3=UNSET, 
#     amplitude=UNSET, fixed=OFF, distributionType=UNIFORM, fieldName='', 
#     localCsys=None)
# #: A boundary condition was created, but its display is currently turned off.
# #: Use "View->Assembly Display Options" to turn on the display of boundary conditions.


#======================================================================================
# PART-12 Save cae file
#======================================================================================
mdb.saveAs(
    pathName= savepathName + str(layer_number))


