"""
Dam break
=========

This example shows a simple free surface example using the ICFD solver. A column of water collapses under the load of gravity. \n
LS-DYNA version : ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe
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
###############################################################################
# Manually start the dyna.core.pre server
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy the folder pyDyna/src/ansys/dyna/core/pre/Server to a desired location.
# Start the dyna.core.pre server at this location as shown below
#
# python kwserver.py
#
# Now the pre server is up and running and is waiting to be connected to the client.
# Connect to the server using the hostname and the port. In this example, default
# "localhost" and port "50051" are used
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/dam_break.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
    
###############################################################################
# Start the Solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dynasolution class is like a workflow orchestrator.
# It inherits methods from other classes and helps create a complete workflow
# Import the initial mesh data(nodes and elements)
solution = DynaSolution(hostname)
fns = []
path = examples.dam_break + os.sep
fns.append(path + "dam_break.k")
solution.open_files(fns)
###############################################################################
# Create standard ICFD cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# **set_termination()** method is used to set the termination time to 
# 50 in *CONTROL_TERMINATION. DynaICFD class automatically generates 
# the common cards used in ICFD problems. Those cards are,
#
# | a) *ICFD_SECTION is automatically generated. 
# | b) Termination time and Time step defaults are set automatically.
solution.set_termination(termination_time=50)
icfd = DynaICFD()
solution.add(icfd)
###############################################################################
# **ICFDAnalysis()** class is used to activate the analysis with important control parameters. 
# **set_timestep()** method is used to set the timestep size to it's default. 
# This change is added to the DynaSolution through the DynaICFD() class.
#
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfd.add(icfdanalysis)
#
###############################################################################
# Define the model
# ~~~~~~~~~~~~~~~~
# Create two ICFD materials, 
#
# | a) (*ICFD_MAT) with density=1000 & Dynamic Viscosity=1e-03
# | b) Vaccum using *ICFD_MAT -> Flag = 0
mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
mat2 = MatICFD(flag=Compressible.VACUUM)
###############################################################################
# | 1) Create ICFD part 1.
# | 2) Set ICFD part 1 with the already created "mat1".
# | 3) Set free slip boundary condition for Part 1.
# | 4) Add the part to **DynaSolution()** through **DynaICFD().parts**
part1 = ICFDPart(1)
part1.set_material(mat1)
part1.set_free_slip()
icfd.parts.add(part1)
###############################################################################
# | 1) Create ICFD part 2.
# | 2) Set ICFD part 2 with the already created "mat2".
# | 3) Set free slip boundary condition for Part 2.
# | 4) Add the part to **DynaSolution()** through **DynaICFD().parts**
part2 = ICFDPart(2)
part2.set_material(mat2)
part2.set_free_slip()
icfd.parts.add(part2)
###############################################################################
# | 1) Create ICFD part 3.
# | 2) Set ICFD part 2 with the already created "mat1".
# | 3) Add the part to **DynaSolution()** through **DynaICFD().parts**
# | 4) Add a gravity load in the Y direction
part3 = ICFDPart(3)
part3.set_material(mat1)
icfd.parts.add(part3)
g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[9.81, 9.81]))
icfd.add(g)
###############################################################################
# Build the volumes of the multiphase model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the first volume of the part using *ICFD_PART_VOL.
# Parts 1 & 3 form the boundaries of the first volume.
# Set the material properties for the first volume by using "mat1" that has already been created.
partvol1 = ICFDVolumePart(surfaces=[1, 3])
partvol1.set_material(mat1)
icfd.parts.add(partvol1)
###############################################################################
# Define second volume of the part using *ICFD_PART_VOL.
# Parts 2 & 3 form the boundaries of the first volume.
# Set the material properties for the second volume by using "mat2" that has already been created.
partvol2 = ICFDVolumePart(surfaces=[2, 3])
partvol2.set_material(mat2)
icfd.parts.add(partvol2)
###############################################################################
# Define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids". Finally, define the
# mesh interface to indicate the free surface.
meshvol = MeshedVolume(surfaces=[1, 2])
meshvol.set_fluid_interfaces([3])
icfd.add(meshvol)
###############################################################################
# Request Output and Save the generated file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *DATABASE_BINARY_D3PLOT is used to request D3PLOT to visualize results in a postprocessing tool.
# The generated keyword file is saved to the output folder in the dyna_pre_server directory.
# Please note that the output frequency is set to every 0.2 seconds.
solution.create_database_binary(dt=0.2)
solution.save_file()
