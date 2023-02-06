"""
Weak FSI
========

This example shows a simple FSI coupling problem using weak/loose/explicit FSI.
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
    ICFDAnalysis,
    ShellPart,
    ShellFormulation,
    PartSet,
    Curve,
    DOF,
    Motion
)
from ansys.dyna.core.pre.dynamaterial import MatRigid
from ansys.dyna.core.pre import examples


hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = DynaSolution(hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.weak_fsi + os.sep
fns.append(path + "weak_fsi.k")
solution.open_files(fns)
solution.set_termination(termination_time=40)
icfd = DynaICFD()
solution.add(icfd)

icfd.set_timestep(tssfac=0.9,max_timestep=Curve(x=[0,10000],y=[0.05,0.05]))

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.05)
icfdanalysis.set_fsi()
icfd.add(icfdanalysis)

# define model
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 5, 6, 10000], y=[0, 0, 1, 1]))
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
part_wall.set_fsi()
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=3)
icfd.parts.add(part_wall)

partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)

#define rigid cylinder
matrigid = MatRigid(mass_density=1000,young_modulus=2e11,poisson_ratio=0.3)
cylinder = ShellPart(1)
cylinder.set_material(matrigid)
cylinder.set_element_formulation(ShellFormulation.PLANE_STRESS)
icfd.parts.add(cylinder)
# Define boundary conddition
icfd.boundaryconditions.create_imposed_motion(PartSet([1]),Curve(func="2*3.14/10*sin(2*3.14/10*TIME+3.14/2)"),dof=DOF.Y_TRANSLATIONAL,motion=Motion.VELOCITY)

solution.create_database_binary(dt=0.2)
solution.save_file()
