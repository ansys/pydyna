"""
Mesh Adaptivity
===============

This example shows a simple ICFD problem with adaptivity.
"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaicfd import (
    DynaICFD,
    MatICFD,
    ICFDPart,
    ICFDDOF,
    Curve,
    ICFDVolumePart,
    MeshedVolume,
    ICFDAnalysis
)
from ansys.dyna.core.pre import examples


hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = DynaSolution(hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.mesh_adaptivity + os.sep
fns.append(path + "mesh_adaptivity.k")
solution.open_files(fns)
solution.set_termination(termination_time=40)
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfdanalysis.set_mesh_adaptivity(min_mesh_size=0.02,max_mesh_size=0.2,max_perceptual_error=2,num_iteration=10)
icfd.add(icfdanalysis)

# define model
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)

part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_free_slip()
icfd.parts.add(part_symmetric)

part_wall = ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=2)
icfd.parts.add(part_wall)

part_meshsize = ICFDPart(5)
part_meshsize.set_material(mat)
icfd.parts.add(part_meshsize)

partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)

solution.create_database_binary(dt=0.5)
solution.save_file()