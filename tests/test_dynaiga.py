import os
import sys
import pytest


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre.dynaiga import *

def comparefile(outputf,standardf):
    with open(outputf,'r') as fp1,open (standardf,'r') as fp2:
        line = fp1.readline()
        line = fp1.readline()
        while True:
            line1 = fp1.readline()
            line2 = fp2.readline()
            if line1=='' or line2 == '':
                break
            if line1 != line2:
                return False
    return True


def test_iga(iga_initialfile,resolve_server_path,resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(iga_initialfile)
    solution.open_files(fns)
    iga = DynaIGA()
    solution.add(iga)
    iga.set_timestep(timestep_size_for_mass_scaled=-0.0004)
    cylinder1 = RigidwallCylinder(Point(2472.37, -600.000, 1270.98),Point(2472.37, -600.000, 2668.53),100,1000)
    iga.add(cylinder1)
    cylinder2 = RigidwallCylinder(Point(3580.25, -600.000, 1261.37),Point(3580.25, -600.000, 3130.49),100,1000)
    iga.add(cylinder2)
    cylinder3 = RigidwallCylinder(Point(3090.59, -955.35, 1299.42),Point(3090.59, -955.35, 2958.43),100,1000)
    cylinder3.set_motion(Curve(x=[0,100],y=[20,20]),dir = Direction(0,1,0))
    iga.add(cylinder3)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path,"output","test_iga.k")
    standardfile = os.path.join(resolve_standard_path,"iga.k")
    assert comparefile(outputfile,standardfile)
    
    