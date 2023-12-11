"""
Internal 3D flow
================

This example shows a simple 3D ICFD problem. \n
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
    ICFD_SurfRemeshMethod
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
# sphinx_gallery_thumbnail_path = '_static/pre/icfd/internal_3d_flow.png'

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
path = examples.internal_3d_flow + os.sep
fns.append(path + "internal_3d_flow.k")
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
solution.set_termination(termination_time=10)
icfd = DynaICFD()
solution.add(icfd)
###############################################################################
# **ICFDAnalysis()** class is used to activate the analysis with important control parameters. 
# **set_timestep()** method is used to set the timestep size to it's default. 
# This change is added to the DynaSolution through the DynaICFD() class.
# Additionally, the volume mesh controls (*ICFD_CONTROL_MESH) are tweaked to change the mesh growth factor to 1.1. 
# And, the surface remesh method (*ICFD_CONTROL_SURFMESH) is changed to Laplacian smoothing.
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(timestep=0.05)
icfdanalysis.set_volume_mesh(mesh_growth_scale_factor=1.1)
icfdanalysis.set_surface_mesh(remesh_method = ICFD_SurfRemeshMethod.LAPLACIAN_SMOOTHING)
icfd.add(icfdanalysis)
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
# | 5) Set boundary layer symmetry for part 1.
# | 6) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
part_inflow.set_boundary_layer_symmetry_condition()
icfd.parts.add(part_inflow)
###############################################################################
# | 1) Create ICFD part 2.
# | 2) Set ICFD part 2 with the already created "mat".
# | 3) Set zero gauge pressure (*ICFD_BOUNDARY_PRESCRIBED_PRE) for the part 2.
# | 4) Set boundary layer symmetry for part 2.
# | 5) Compute flux (*ICFD_DATABASE_FLUX) at part 2.
# | 6) Add the part to **DynaSolution()** through **DynaICFD().parts**
part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
part_outflow.set_boundary_layer_symmetry_condition()
part_outflow.compute_flux()
icfd.parts.add(part_outflow)
###############################################################################
# | 1) Create ICFD part 3.
# | 2) Set ICFD part 3 with the already created "mat".
# | 3) Set no slip (*ICFD_BOUNDARY_FREESLIP) to part 3.
# | 4) Request a boundary layer mesh (*MESH_BL) of 3 layers on part 3.
# | 5) Add the part to **DynaSolution()** through **DynaICFD().parts**
#
part_wall = ICFDPart(3)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.set_boundary_layer(2)
icfd.parts.add(part_wall)
###############################################################################
# Build the volume of the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define volume of the part using *ICFD_PART_VOL.
# Parts 1, 2, 3 form the boundaries of the volume.
# Now, set the material properties for the volume by using "mat" that has already been created.
# Finally, define the volume space that will be meshed. The boundaries
# of the volume are the surfaces "spids".
partvol = ICFDVolumePart(surfaces=[1, 2, 3])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2, 3])
icfd.add(meshvol)
###############################################################################
# Request Output and Save the generated file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *DATABASE_BINARY_D3PLOT is used to request D3PLOT to visualize results in a postprocessing tool.
# The generated keyword file is saved to the output folder in the dyna_pre_server directory.
solution.create_database_binary(dt=1)
solution.save_file()
