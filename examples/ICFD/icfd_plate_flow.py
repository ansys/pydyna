# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# %% [markdown]
# # Plate Flow Simulation with LS-DYNA Python API
#
# This example demonstrates how to set up and solve a plate flow problem using the LS-DYNA Python API.
# Plate flow is a classic benchmark in computational fluid dynamics, used to study boundary layer development
# and flow separation around a flat plate. The workflow is organized into clear sections, with detailed explanations
# and notebook cell markers for educational use.

# %% [markdown]
# ## 1. Imports and Data Setup
# In this section, we import all required modules and LS-DYNA Python API classes. These imports provide access to
# the core solver, geometry, material, and boundary condition definitions needed for the simulation. The `examples`
# module provides access to example input files, while `launch_dynapre` is used to start the LS-DYNA pre-processor.
# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/plate_flow.png'

# %% [markdown]
# ## 2. LS-DYNA Executable and File Paths
# Here, we set up the LS-DYNA server hostname and input file paths. The hostname can be changed to point to a remote
# server if needed. The example input file is loaded, and the simulation is configured to run for a specified
# termination time. This step ensures the solver is ready and the model is loaded for further setup.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.plate_flow + os.sep
fns.append(path + "plate_flow.k")
solution.open_files(fns)
solution.set_termination(termination_time=100)

# %% [markdown]
# ## 3. ICFD Model Setup
# In this section, we create and configure the ICFD (Incompressible Computational Fluid Dynamics) model. The
# `DynaICFD` object is added to the solution, and the analysis settings are defined, including the time step size.
# This configuration is crucial for ensuring numerical stability and accuracy during the simulation.
# %%
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfd.add(icfdanalysis)

# %% [markdown]
# ## 4. Material and Part Definitions
# Here, we define the fluid material properties and assign them to different parts of the model. Each part represents
# a boundary or region in the simulation domain:
# - **Inflow**: Where fluid enters the domain, with prescribed velocity in the X direction.
# - **Outflow**: Where fluid exits the domain, with prescribed pressure.
# - **Symmetry**: Represents symmetry boundaries with free-slip conditions.
# - **Wall (Plate)**: The flat plate, modeled as a wall with non-slip condition and boundary layer settings for accurate near-wall resolution.
#
# Proper boundary conditions and material assignment are essential for capturing the correct flow physics around the plate.
# %%
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

# %% [markdown]
# ## 5. Volume and Plate Embedding Setup
# In this section, we define the volume region and set up the embedded plate. The `MeshedVolume` object defines the
# overall meshed region, and the `embed_shell` method specifies the part(s) representing the plate to be embedded
# within the mesh. This setup allows the mesh to conform to the plate geometry, improving solution accuracy near the wall.
# %%
partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2, 3])
meshvol.embed_shell([4])
icfd.add(meshvol)

# %% [markdown]
# ## 6. Output Requests and Save
# Here, we configure the output database and save the simulation setup. The output database is set to record results
# at a specified time interval, which is important for post-processing and analysis.
# %%
solution.create_database_binary(dt=1)
solution.save_file()

# %% [markdown]
# ## 7. Conclusion
# In this example, we demonstrated how to set up a plate flow simulation using the LS-DYNA Python API. The
# workflow included model setup, detailed material and part definitions, boundary conditions, plate embedding,
# and output setup. Plate flow is a fundamental CFD benchmark, and this script provides a robust starting point
# for further studies and educational use.
