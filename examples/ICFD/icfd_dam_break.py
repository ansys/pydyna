"""
Dam break
=========

This example shows a simple free surface example using the ICFD solver. A column of water collapses under the load of gravity.
"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaicfd import (
    DynaICFD,
    MatICFD,
    ICFDPart,
    Curve,
    ICFDVolumePart,
    MeshedVolume,
    ICFDAnalysis,
    Gravity,
    GravityOption,
    Compressible
)
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/dam_break.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = DynaSolution(hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.dam_break + os.sep
fns.append(path + "dam_break.k")
solution.open_files(fns)
solution.set_termination(termination_time=50)
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfd.add(icfdanalysis)

# define model
mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
mat2 = MatICFD(flag=Compressible.VACUUM)

part1 = ICFDPart(1)
part1.set_material(mat1)
part1.set_free_slip()
icfd.parts.add(part1)

part2 = ICFDPart(2)
part2.set_material(mat2)
part2.set_free_slip()
icfd.parts.add(part2)

part3 = ICFDPart(3)
part3.set_material(mat1)
icfd.parts.add(part3)

g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[9.81, 9.81]))
icfd.add(g)

partvol1 = ICFDVolumePart(surfaces=[1, 3])
partvol1.set_material(mat1)
icfd.parts.add(partvol1)

partvol2 = ICFDVolumePart(surfaces=[2, 3])
partvol2.set_material(mat2)
icfd.parts.add(partvol2)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2])
meshvol.set_fluid_interfaces([3])
icfd.add(meshvol)

solution.create_database_binary(dt=0.2)
solution.save_file()
