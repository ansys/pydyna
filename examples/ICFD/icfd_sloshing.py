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
# # Sloshing Simulation with LS-DYNA Python API
#
# This example demonstrates how to set up and solve a sloshing problem using the LS-DYNA Python API.
# Sloshing refers to the movement of liquid inside a container, which is important in many engineering
# applications such as tanks, ships, and vehicles. The workflow is organized into clear sections, with detailed
# explanations and notebook cell markers for educational use.

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
    Compressible,
    Curve,
    DynaICFD,
    Gravity,
    GravityOption,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/sloshing.png'

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
path = examples.sloshing + os.sep
fns.append(path + "sloshing.k")
solution.open_files(fns)
solution.set_termination(termination_time=1)

# %% [markdown]
# ## 3. ICFD Model Setup
# In this section, we create and configure the ICFD (Incompressible Computational Fluid Dynamics) model. The
# `DynaICFD` object is added to the solution, and the analysis settings are defined, including the time step size.
# This configuration is crucial for ensuring numerical stability and accuracy during the simulation.
# %%
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.02)
icfd.add(icfdanalysis)

# %% [markdown]
# ## 4. Material and Part Definitions
# Here, we define the fluid and vacuum material properties and assign them to different parts of the model. Each part represents
# a boundary or region in the simulation domain:
# - **Bottom**: The bottom of the tank, modeled as a wall with non-slip condition and filled with liquid.
# - **Top**: The top of the tank, modeled as a wall with non-slip condition and filled with vacuum (or air).
# - **Mid Interface**: The interface between the liquid and vacuum, allowing for fluid-structure interaction and sloshing.
#
# Proper material assignment and boundary conditions are essential for capturing the correct sloshing physics.
# %%
mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
mat2 = MatICFD(flag=Compressible.VACUUM)

part_bottom = ICFDPart(1)
part_bottom.set_material(mat1)
part_bottom.set_non_slip()
icfd.parts.add(part_bottom)

part_top = ICFDPart(2)
part_top.set_material(mat2)
part_top.set_non_slip()
icfd.parts.add(part_top)

part_mid = ICFDPart(3)
part_mid.set_material(mat1)
icfd.parts.add(part_mid)

# %% [markdown]
# ## 5. Gravity and Imposed Motion
# In this section, we add gravity to the model and impose a horizontal movement to simulate sloshing. The gravity
# is applied in the Z direction, and the imposed movement is defined as a time-dependent curve in the X direction.
# This setup mimics the effect of shaking or moving the container, causing the liquid to slosh.
# %%
g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0, 10000], y=[9.81, 9.81]))
icfd.add(g)

icfd.set_imposed_move(vx=Curve(x=[0, 0.5, 0.52, 0.8, 0.82, 2.0], y=[1, 1, -1, -1, 0, 0]))

# %% [markdown]
# ## 6. Volume and Fluid Interface Setup
# In this section, we define the volume regions and set up the fluid interface. The domain is split into two volume parts:
# - The bottom volume part includes the bottom and mid surfaces, filled with liquid.
# - The top volume part includes the top and mid surfaces, filled with vacuum.
#
# The `MeshedVolume` object defines the overall meshed region, and the `set_fluid_interfaces` method specifies the
# interface where sloshing occurs. This setup allows the mesh to adapt dynamically to the moving liquid interface.
# %%
partvol_bottom = ICFDVolumePart(surfaces=[1, 3])
partvol_bottom.set_material(mat1)
icfd.parts.add(partvol_bottom)

partvol_top = ICFDVolumePart(surfaces=[2, 3])
partvol_top.set_material(mat2)
icfd.parts.add(partvol_top)

meshvol = MeshedVolume(surfaces=[1, 2])
meshvol.set_fluid_interfaces([3])
icfd.add(meshvol)

# %% [markdown]
# ## 7. Output Requests and Save
# Here, we configure the output database and save the simulation setup. The output database is set to record results
# at a specified time interval, which is important for post-processing and analysis.
# %%
solution.create_database_binary(dt=0.02)
solution.save_file()

# %% [markdown]
# ## 8. Conclusion
# In this example, we demonstrated how to set up a sloshing simulation using the LS-DYNA Python API. The
# workflow included model setup, detailed material and part definitions, boundary conditions, gravity and imposed
# motion, fluid interface configuration, and output setup. Sloshing simulations are important for understanding
# liquid behavior in moving containers and are widely used in engineering applications.
#
