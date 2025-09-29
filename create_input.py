
# create_input_v1, previoud version, create_input_v0
# modification 2023-07-02
# 1. keep the maximum tem8perature constant while the base plate temperature increases.
# 2. heat the base plate by half of the layer thickness



import sys
from abaqus import *
from abaqusConstants import *
import math
# session.viewports['Viewport: 1'].setValues(displayedObject=None) #
# openMdb(
   # pathName='D:/1_Project/A2021HN080001_AM_national/004_Data/code/test/24.cae')
 #             D:\1_Project\A2021HN080001_AM_national\004_Data\code

session.viewports['Viewport: 1'].setValues(displayedObject=None)
# The first CLI argument is the .cae file path passed in from AM_gui:
# CAE_FILE = sys.argv[1] if len(sys.argv) > 1 else None
# if not CAE_FILE:
#     raise ValueError("No CAE file supplied to create_input.py!")

openMdb(pathName=CAE_FILE)


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
            fid.write("             GT=GT+(0)*EXP(-3*((COORDS(2)-(i-0.5)*"+str(layer_sp)+")/R2)**2)\n")
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
            fid.write("             GT=GT+(A-((i-1)**0.3*50.0-50.0))*EXP(-3*((COORDS(2)-\n")
            fid.write("     1         (i-1)*"+str(layer_sp)+")/R2)**2)\n")
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
            fid.write("          TEMP(1)="+str(layer_n+1)+"\n")
            fid.write("          END IF\n")
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