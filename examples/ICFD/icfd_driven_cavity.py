"""
Driven cavity
=============

This example shows the universally famous driven cavity case tested with the second order steady solver and for Re=1000. \n
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
    ICFD_AnalysisType,
    ICFD_MessageLevel,
    ICFDDOF
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
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/driven_cavity.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
###############################################################################
# Start the Solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dynasolution class is like a workflow orchestrator.
# It inherits methods from other classes and helps create a complete workflow.
# Import the initial mesh data(nodes and elements)
solution = DynaSolution(hostname)
fns = []
path = examples.driven_cavity + os.sep
fns.append(path + "driven_cavity.k")
solution.open_files(fns)
###############################################################################
# Set up a Steady State Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# | a) Set the Analysis Type to **Steady State Analysis**
# | b) The control card for the steady state analysis is set with its inputs
# | c) Once the basic inputs are set, the analysis is added to the orchestrator
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_type(analysis_type=ICFD_AnalysisType.STEADY_STATE_ANALYSIS)
icfdanalysis.set_output(messagelevel=ICFD_MessageLevel.FULL_OUTPUT_INFORMATION,iteration_interval=250)
icfdanalysis.set_steady_state(max_iteration=2500,momentum_tol_limit=1e-8,pressure_tol_limit=1e-8,velocity_relax_param=1,pressure_relax_param=1)
icfd.add(icfdanalysis)

#
###############################################################################
# Define the model
# ~~~~~~~~~~~~~~~~
# Create ICFD material using *ICFD_MAT, 
#
# | a) Fluid material with density=1 & Dynamic Viscosity=1e-03
mat = MatICFD(flow_density=1, dynamic_viscosity=0.001)
###############################################################################
# | 1) Create ICFD part 1.
# | 2) Set ICFD part 1 with the already created "mat".
# | 3) Set Inflow velocity (*ICFD_BOUNDARY_PRESCRIBED_VEL) in DOF X
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part1 = ICFDPart(1)
part1.set_material(mat)
part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part1)
###############################################################################
# | 1) Create ICFD part 2.
# | 2) Set ICFD part 2 with the already created "mat".
# | 3) Set a no slip boundary condition.
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
part2 = ICFDPart(2)
part2.set_material(mat)
part2.set_non_slip()
icfd.parts.add(part2)
###############################################################################
# Build the volume of the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define volume of the part using *ICFD_PART_VOL.
# Parts 1, 2 form the boundaries of the volume.
# Now, set the material properties for the volume by using "mat".
# Finally, define the volume space that will be meshed. The boundaries
# of the volume are the surfaces "spids".
partvol = ICFDVolumePart(surfaces=[1, 2])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2])
icfd.add(meshvol)
###############################################################################
# Request Output and Save the generated file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *DATABASE_BINARY_D3PLOT is used to request D3PLOT to visualize results in a postprocessing tool.
# The generated keyword file is saved to the output folder in the dyna_pre_server directory.
solution.create_database_binary(dt=250)
solution.save_file()
