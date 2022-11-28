"""
Cylinder flow example
=====================

This example demonstrates how to create a simple ICFD cylinder flow input deck.
"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre.dynaicfd import *
from ansys.dyna.core.pre import examples


hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

icfd_solution = DynaSolution(hostname)
#Import the initial mesh data(nodes and elements)
fns = []
path = examples.icfd_cylinderflow+ os.sep
fns.append(path + "mesh.k")
icfd_solution.open_files(fns)
#Set total time of simulation  
icfd_solution.set_termination(termination_time=100) 

icfd = DynaICFD()
icfd_solution.add(icfd)

#define model
mat = MatICFD(flow_density=1.0,dynamic_viscosity=0.005)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X,motion=Curve(x=[0, 10000],y=[1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y,motion=Curve(x=[0, 10000],y=[0, 0]))
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure = Curve(x=[0, 10000],y=[0, 0]))
icfd.parts.add(part_outflow)

part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_free_slip()
icfd.parts.add(part_symmetric)

part_wall= ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=3)
icfd.parts.add(part_wall)

partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)  
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries 
#of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces = [1, 2, 3, 4])
icfd.add(meshvol)

icfd_solution.create_database_binary(dt=1)
icfd_solution.save_file()
