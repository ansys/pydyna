"""
Rigid test example
=====================

This example show how to create an ISPH model with Pydyna-pre module
"""

import os
import sys

sys.path.append(os.path.join(sys.path[0],'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynamaterial import *
from ansys.dyna.pre.dynaisph import *
from ansys.dyna.pre import examples


if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    isphsolution = DynaSolution(hostname)
    fns = []
    path = examples.isph_rigidtest+ os.sep
    fns.append(path + "rigidtest.k")
    isphsolution.open_files(fns)

    isphsolution.set_termination(0.5)
    
    isphobj = DynaISPH()
    isphsolution.add(isphobj)

    platemat1 = MatRigid(mass_density=1e-9,young_modulus=10,center_of_mass_constraint=1,translational_constraint=5,rotational_constraint=4)
    platemat2 = MatRigid(mass_density=1e-9,young_modulus=10,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    #platemat3 = MatRigid(mass_density=1e-9,young_modulus=10)
    matelastic = MatElastic(mass_density=1e-9,young_modulus=1)
    matsphfluid = MatSPHIncompressibleFluid(mass_density=1e-9, dynamic_viscosity=1e-9, tension_coefficient1=1e6,tension_coefficient2=1000)
    matsphstruct = MatSPHIncompressibleStructure(mass_density=1e-9)


    sensorplane = ShellPart(1)
    sensorplane.set_material(platemat2)
    sensorplane.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    sensorplane.set_thickness(0.1)
    isphobj.parts.add(sensorplane)

    movingcube = ShellPart(7)
    movingcube.set_material(platemat1)
    movingcube.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    movingcube.set_thickness(0.1)
    isphobj.parts.add(movingcube)

    wallsmesh = ShellPart(8)
    wallsmesh.set_material(platemat2)
    wallsmesh.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    wallsmesh.set_thickness(0.1)
    isphobj.parts.add(wallsmesh)

    # Define boundary conddition
    isphobj.boundaryconditions.create_imposed_motion(PartSet([7]),Curve(x=[0,0.1,0.11,20],y=[3000,3000,0,0]),dof=DOF.X_TRANSLATIONAL,motion=Motion.VELOCITY)
    isphobj.boundaryconditions.create_imposed_motion(PartSet([7]),Curve(x=[0,0.1,0.11,20],y=[500,500,500,500]),dof=DOF.Z_ROTATIONAL,motion=Motion.VELOCITY,scalefactor=0.01)
    
    #Load
    g = Gravity(dir=GravityOption.DIR_Z,load = Curve(x=[0, 100],y=[9810, 9810]))
    isphobj.add(g)
    
    isphsolution.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)
    isphsolution.create_database_binary(dt=5e-4, ieverp=1)
    isphsolution.save_file()
