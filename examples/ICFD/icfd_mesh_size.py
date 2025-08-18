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
# # Mesh Size Control Simulation with LS-DYNA Python API
#
# This example demonstrates how to set up and solve a mesh size control problem using the LS-DYNA Python API.
# Mesh size control allows you to specify local mesh refinement in certain regions, which is important for
# accurately capturing gradients and features in the solution. The workflow is organized into clear sections,
# with detailed explanations and notebook cell markers for educational use.

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

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/mesh_size.png'

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
path = examples.mesh_size + os.sep
fns.append(path + "mesh_size.k")
solution.open_files(fns)
solution.set_termination(termination_time=50)

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
# - **Wall**: A wall with non-slip condition and boundary layer settings for accurate near-wall resolution.
# - **Mesh Size Control**: A special part to control local mesh size in the region of interest.
#
# Local mesh refinement is important for resolving features such as boundary layers, wakes, or regions with high gradients.
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

part_meshsize = ICFDPart(5)
part_meshsize.set_material(mat)
icfd.parts.add(part_meshsize)

# %% [markdown]
# ## 5. Volume and Mesh Size Control Setup
# In this section, we define the volume region and set up mesh size control. The `MeshedVolume` object defines the
# overall meshed region, and the `set_meshsize` method specifies the part(s) where local mesh refinement is applied.
# This setup allows the mesh to be finer in regions of interest, improving solution accuracy without excessive
# computational cost.
# %%
partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
meshvol.set_meshsize([5])
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
# In this example, we demonstrated how to set up a mesh size control simulation using the LS-DYNA Python API. The
# workflow included model setup, detailed material and part definitions, boundary conditions, mesh size control
# configuration, and output setup. Mesh size control is a powerful technique for improving solution accuracy in
# regions of interest while keeping computational costs manageable.
