"""
Internal 3D flow
================

This example shows a simple 3D ICFD problem.
The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""

import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFD_SurfRemeshMethod,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/internal_3d_flow.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.internal_3d_flow + os.sep
fns.append(path + "internal_3d_flow.k")
solution.open_files(fns)
solution.set_termination(termination_time=10)
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(timestep=0.05)
icfdanalysis.set_volume_mesh(mesh_growth_scale_factor=1.1)
icfdanalysis.set_surface_mesh(remesh_method=ICFD_SurfRemeshMethod.LAPLACIAN_SMOOTHING)
icfd.add(icfdanalysis)
# define model
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
part_inflow.set_boundary_layer_symmetry_condition()
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
part_outflow.set_boundary_layer_symmetry_condition()
part_outflow.compute_flux()
icfd.parts.add(part_outflow)

part_wall = ICFDPart(3)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.set_boundary_layer(2)
icfd.parts.add(part_wall)

partvol = ICFDVolumePart(surfaces=[1, 2, 3])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2, 3])
icfd.add(meshvol)

solution.create_database_binary(dt=1)
solution.save_file()
