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

    isphobj.set_timestep(tssfac=1,max_timestep = Curve(x=[0,0.04,0.05,0.1,100],y=[0.5,0.5,1,1,1]))
    isphobj.isphanalysis.set_box(Box(-750,800,-800,800,-100,3000))

    platemat1 = MatRigid(mass_density=1e-9,young_modulus=10,center_of_mass_constraint=1,translational_constraint=5,rotational_constraint=4)
    platemat2 = MatRigid(mass_density=1e-9,young_modulus=10,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    #platemat3 = MatRigid(mass_density=1e-9,young_modulus=10)
    #matelastic = MatElastic(mass_density=1e-9,young_modulus=1)
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

    sphwall = ISPHStructPart(4,PartSet([8]),12)
    sphwall.set_material(matsphstruct)
    sphwall.set_smoothing_length(1,1,1,12)
    isphobj.parts.add(sphwall)

    sphcube = ISPHStructPart(5,PartSet([7]),12)
    sphcube.set_material(matsphstruct)
    sphcube.set_smoothing_length(1,1,1,12)
    isphobj.parts.add(sphcube)

    sphwater = ISPHFluidPart(6,Point(-588,-588,9),Point(1176,1176,204),98,98,17)
    sphwater.set_material(matsphfluid)
    sphwater.set_smoothing_length(1,1,1,12)
    sphwater.create_massflow_plane(PartSet([1]))
    isphobj.parts.add(sphwater)

     #Constraint
    isphobj.constraints.merge_two_rigid_bodies(7,1)

    # Define boundary conddition
    isphobj.boundaryconditions.create_imposed_motion(PartSet([7]),Curve(x=[0,0.1,0.11,20],y=[3000,3000,0,0]),dof=DOF.X_TRANSLATIONAL,motion=Motion.VELOCITY)
    isphobj.boundaryconditions.create_imposed_motion(PartSet([7]),Curve(x=[0,0.1,0.11,20],y=[500,500,500,500]),dof=DOF.Z_ROTATIONAL,motion=Motion.VELOCITY,scalefactor=0.01)
    
    #Load
    g = Gravity(dir=GravityOption.DIR_Z,load = Curve(x=[0, 100],y=[9810, 9810]))
    isphobj.add(g)
    
    isphsolution.set_output_database(glstat=0.001,sphmassflow=0.001)
    isphsolution.create_database_binary(dt=0.01)
    isphsolution.save_file()