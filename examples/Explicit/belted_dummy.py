"""
Belted dummy example
=====================

This example show how to create an Belted dummy model with Pydyna-pre module
"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynamech import *
from ansys.dyna.pre.dynamaterial import *
from belted_dummy_data import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    
    dummy_solution = DynaSolution(hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "belted_dummy" + os.sep
    fns.append(path + "belted_dummy.k")
    dummy_solution.open_files(fns)
    dummy_solution.set_termination(termination_time=0.12)
    dummy_solution.create_database_binary(dt=2.5e-3)

    dummy = DynaMech()
    dummy_solution.add(dummy)    
    
    dummy.set_timestep(tssfac=0.8)
    dummy.set_init_velocity(Velocity(14.8, 0, 0))
    
    #Define material
    shellmatlist = []
    for i in range(15):
        matrigid = MatRigid(mass_density=rigidmats[i][0],young_modulus=rigidmats[i][1],poisson_ratio=0.3)
        shellmatlist.append(matrigid)

    for i in range(16, 23):
        index = i - 16
        matelastic = MatElastic(mass_density=elasticmats[index][0],young_modulus=elasticmats[index][1],poisson_ratio=0.3)
        shellmatlist.append(matelastic)

    discmatlist = []
    for i in range(101, 143):
        index = i-101
        mat = MatSpringNonlinearElastic(curve=Curve(x=curvedata[index][0],y=curvedata[index][1]))
        discmatlist.append(mat)

    for i in range(143, 185):
        index = i-143
        mat = MatDamperViscous(damping_constant = dampingconst[index])
        discmatlist.append(mat)

    for i in range(185, 209):
        index = i - 185
        mat = MatDamperNonlinearViscous(curve=Curve(x=curvedata[lcidlist[index]][0],y=curvedata[lcidlist[index]][1]))
        discmatlist.append(mat)
    
    #Set part properties
    for i in range (1,23):
        part = ShellPart(i)
        part.set_material(shellmatlist[i-1])
        part.set_thickness(shellsec[i-1][0])
        part.set_integration_points(shellsec[i-1][1])
        if i in range(1,16):
            part.set_extra_nodes(NodeSet(extra_nodes[i-1]))

    for i in range(101, 209):
        index = i-101
        part = DiscretePart(i)
        part.set_material(discmatlist[index])
        part.set_displacement_option(displacement_option=DRO.DESCRIBES_TORSIONAL_SPRING)
    
    #Contact
    fslist = [0.62, 0.62, 0.62, 0.8, 1, 0.8, 0.88, 0.88, 0.16, 0.88, 0]
    for i in range(11):
        contact = Contact(category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
        contact.set_friction_coefficient(static=fslist[i])
        surf1=ContactSurface(SegmentSet(segments[2*i]))
        surf2=ContactSurface(SegmentSet(segments[2*i+1]))
        contact.set_slave_surface(surf1)
        contact.set_master_surface(surf2)
    
    #Constraint
    cons = Constraint()
    for i in range(14):
        cons.create_joint_spherical(nodes=jointlist[i])

    #Boundary condition
    bdy = BoundaryCondition()
    bdy.create_imposed_motion(NodeSet(motion_nodes),Curve(x=motion_curve_x,y=motion_curve_y),motion=Motion.ACCELERATION,scalefactor=-1)
    #Load
    g = Gravity(dir=GravityOption.DIR_Z,load = Curve(x=[0, 0.152],y=[9.81, 9.81]))

    dummy_solution.create_database_binary(dt=2.5e-3) 
    dummy_solution.save_file()
