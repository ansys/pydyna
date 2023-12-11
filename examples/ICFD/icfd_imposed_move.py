"""
Imposed move
============

This example shows how to impose the displacements on the fluid nodes through the use of the ICFD_CONTROL_IMPOSED_MOVE keyword. \n
LS-DYNA version : ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe
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
    Curve
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
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/imposed_move.png'

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
path = examples.imposed_move + os.sep
fns.append(path + "imposed_move.k")
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
solution.set_termination(termination_time=40)
icfd = DynaICFD()
solution.add(icfd)
###############################################################################
# **ICFDAnalysis()** class is used to activate the analysis with important control parameters. 
# **set_timestep()** method is used to set the timestep size to it's default. 
# This change is added to the DynaSolution through the DynaICFD() class.
#
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.05)
icfd.add(icfdanalysis)
#
###############################################################################
# Define the model
# ~~~~~~~~~~~~~~~~
# Create an ICFD material (*ICFD_MAT) with density=1.0 & Dynamic Viscosity=5e-03
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)
###############################################################################
# | 1) Create ICFD part 1.
# | 2) Set ICFD part 1 with the already created "mat".
# | 3) Set Inflow velocity (*ICFD_BOUNDARY_PRESCRIBED_VEL) in DOF X.
# | 4) Set Inflow velocity (*ICFD_BOUNDARY_PRESCRIBED_VEL) in DOF Y.
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 5, 6, 10000], y=[0, 0, 1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_inflow)
###############################################################################
# | 1) Create ICFD part 2.
# | 2) Set ICFD part 2 with the already created "mat".
# | 3) Set zero gauge pressure (*ICFD_BOUNDARY_PRESCRIBED_PRE) to the part 2.
# | 4) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)
###############################################################################
# | 1) Create ICFD part 3.
# | 2) Set ICFD part 3 with the already created "mat".
# | 3) Set free slip (*ICFD_BOUNDARY_FREESLIP) to part 3.
# | 4) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_free_slip()
icfd.parts.add(part_symmetric)
###############################################################################
# | 1) Create ICFD part 4.
# | 2) Set ICFD part 4 with the already created "mat".
# | 3) Set no slip (*ICFD_BOUNDARY_NONSLIP) to part 4.
# | 4) Set *ICFD_DATABASE_DRAG to compute drag forces on part 4.
# | 5) Set *MESH_BL to capture boundary layer flow on part 4.
# | 6) Set an imposed move card to move the ICFD volume mesh.
# | 6) Add the part to **DynaSolution()** through **DynaICFD().parts**
#
part_wall = ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=3)
part_wall.set_imposed_move(vy=Curve(func="2*3.14/10*sin(2*3.14/10*TIME+3.14/2)"))
icfd.parts.add(part_wall)
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

meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)
###############################################################################
# Request Output and Save the generated file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *DATABASE_BINARY_D3PLOT is used to request D3PLOT to visualize results in a postprocessing tool.
# The generated keyword file is saved to the output folder in the dyna_pre_server directory.
solution.create_database_binary(dt=0.5)
solution.save_file()
