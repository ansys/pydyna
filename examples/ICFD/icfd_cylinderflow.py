"""
Cylinder flow example
=====================

This example demonstrates how to create a simple ICFD cylinder flow input deck.
"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynaicfd import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]

    icfd_solution = DynaSolution(hostname)
    #Import the initial mesh data(nodes and elements)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep
    fns.append(path + os.sep + "icfd_cylinderflow" + os.sep + "mesh.k")
    icfd_solution.open_files(fns)

    #Set total time of simulation
    icfd_solution.set_termination(termination_time=100)    
    icfd_solution.create_database_binary(dt=1)

    icfd = DynaICFD()
    icfd_solution.add(icfd)

    #define model
    mat = MatICFD(flow_density=1.0,dynamic_viscosity=0.005)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_prescribed_velocity(dof=DOF.X,motion=Curve(x=[0, 10000],y=[1, 1]))
    part_inflow.set_prescribed_velocity(dof=DOF.Y,motion=Curve(x=[0, 10000],y=[0, 0]))

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_prescribed_pressure(pressure = Curve(x=[0, 10000],y=[0, 0]))

    part_symmetric = ICFDPart(3)
    part_symmetric.set_material(mat)
    part_symmetric.set_free_slip()

    part_wall= ICFDPart(4)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.compute_drag_force()
    part_wall.set_boundary_layer(number=3)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    partvol.set_material(mat)  
    # define the volume space that will be meshed,The boundaries 
    #of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces = [1, 2, 3, 4])

    icfd_solution.save_file()
