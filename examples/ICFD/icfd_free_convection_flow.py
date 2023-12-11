"""
Thermal flow
============

This example shows a simple ICFD forced convection input deck with a coarse mesh. \n
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
    GravityOption

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
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/free_convection_flow.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
###############################################################################
# Start the Solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dynasolution class is like a workflow orchestrator.
# It inherits methods from other classes and helps create a complete workflow.
# Import the initial mesh data(nodes and elements).
solution = DynaSolution(hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.free_convection_flow + os.sep
fns.append(path + "free_convection_flow.k")
solution.open_files(fns)
###############################################################################
# Create standard ICFD cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# **set_termination()** method is used to set the termination time to 
# 100 in *CONTROL_TERMINATION. DynaICFD class automatically generates 
# the common cards used in ICFD problems. Those cards are,
#
# | a) *ICFD_SECTION is automatically generated. 
# | b) Termination time and Time step defaults are set automatically.
solution.set_termination(termination_time=30)
icfd = DynaICFD()
solution.add(icfd)
###############################################################################
# **ICFDAnalysis()** class is used to activate the analysis with important control parameters. 
# **set_timestep()** method is used to set the timestep size to it's default. 
# This change is added to the DynaSolution through the DynaICFD() class.
#
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.01)
icfd.add(icfdanalysis)
###############################################################################
# Define the model
# ~~~~~~~~~~~~~~~~
# Create an ICFD material (*ICFD_MAT) with density=37.799999, Dynamic Viscosity=1, Heat Capacity = 0.7, Thermal Conductivity = 1.0, Thermal Expansion Coefficient = 1
mat = MatICFD(flow_density=37.799999, dynamic_viscosity=1,heat_capacity = 0.7,thermal_conductivity=1.0,thermal_expansion_coefficient=1)
###############################################################################
# | 1) Create ICFD part 1.
# | 2) Set ICFD part 1 with the already created "mat".
# | 3) Set no slip boundary condition on Part 1.
# | 4) Set a temperature boundary condition on Part 1.
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_non_slip()
part_inflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part_inflow)
###############################################################################
# | 1) Create ICFD part 2.
# | 2) Set ICFD part 2 with the already created "mat".
# | 3) Set no slip boundary condition on Part 2.
# | 4) Compute temperature using *ICFD_DATABASE_TEMP
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_non_slip()
part_outflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)
###############################################################################
# | 1) Create ICFD part 3.
# | 2) Set ICFD part 3 with the already created "mat".
# | 3) Set no slip boundary condition on Part 3.
# | 4) Compute temperature using *ICFD_DATABASE_TEMP
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_non_slip()
part_symmetric.compute_temperature()
icfd.parts.add(part_symmetric)
###############################################################################
# | 1) Create ICFD part 4.
# | 2) Set ICFD part 4 with the already created "mat".
# | 3) Set no slip boundary condition on Part 3.
# | 4) Compute temperature using *ICFD_DATABASE_TEMP
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_wall = ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.compute_temperature()
icfd.parts.add(part_wall)
###############################################################################
# Simple initialization of velocity and temperature within a volume.
icfd.set_initial()
###############################################################################
# Prescribe body load in the Y direction
g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[1, 1]))
icfd.add(g)
###############################################################################
# Build the volume of the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define volume of the part using *ICFD_PART_VOL.
# Parts 1, 2, 3, & 4 form the boundaries of the volume.
# Now, set the material properties for the volume by using "mat" that has already been created.
# Finally, define the volume space that will be meshed. The boundaries
# of the volume are the surfaces "spids".
partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)
meshvol = MeshedVolume(surfaces=[1, 2, 3,4])
icfd.add(meshvol)
###############################################################################
# Request Output and Save the generated file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *DATABASE_BINARY_D3PLOT is used to request D3PLOT to visualize results in a postprocessing tool.
# The generated keyword file is saved to the output folder in the dyna_pre_server directory.
solution.create_database_binary(dt=1)
solution.save_file()
