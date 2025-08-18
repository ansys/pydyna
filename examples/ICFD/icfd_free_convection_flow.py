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
# # Free Convection Flow Simulation with LS-DYNA Python API
#
# This example demonstrates how to set up and solve a free convection flow problem using the LS-DYNA Python API.
# The workflow is organized into clear sections, with explanations and notebook cell markers for educational use.

# %% [markdown]
# ## 1. Imports and Data Setup
# Import required modules and LS-DYNA Python API classes. This step ensures all necessary libraries and data are available for the simulation.
# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
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

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/free_convection_flow.png'

# %% [markdown]
# ## 2. LS-DYNA Executable and File Paths
# Set up the LS-DYNA server hostname and input file paths. This prepares the solver for launching and loads the model for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.free_convection_flow + os.sep
fns.append(path + "free_convection_flow.k")
solution.open_files(fns)
solution.set_termination(termination_time=30)

# %% [markdown]
# ## 3. ICFD Model Setup
# Create and configure the ICFD model. Set solver parameters for accurate and stable simulation of free convection.
# %%
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.01)
icfd.add(icfdanalysis)

# %% [markdown]
# ## 4. Material and Part Definitions
# Define and assign material properties to the parts. Set up boundary conditions for inflow, outflow, symmetry, and wall.
# %%
mat = MatICFD(
    flow_density=37.799999,
    dynamic_viscosity=1,
    heat_capacity=0.7,
    thermal_conductivity=1.0,
    thermal_expansion_coefficient=1,
)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_non_slip()
part_inflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_non_slip()
part_outflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)

part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_non_slip()
part_symmetric.compute_temperature()
icfd.parts.add(part_symmetric)

part_wall = ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.compute_temperature()
icfd.parts.add(part_wall)

# %% [markdown]
# ## 5. Initial Conditions and Gravity
# Set initial conditions and add gravity to the model.
# %%
icfd.set_initial()

g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[1, 1]))
icfd.add(g)

# %% [markdown]
# ## 6. Volume and Meshing
# Define the volume space to be meshed and assign the boundaries.
# %%
partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)

# %% [markdown]
# ## 7. Output Requests and Save
# Configure output requests and save the simulation setup.
# %%
solution.create_database_binary(dt=1)
solution.save_file()

# %% [markdown]
# ## 8. Conclusion
# In this example, we demonstrated how to set up a free convection flow simulation using the LS-DYNA Python API.
# The workflow included model setup, material and part definitions, boundary conditions, and output configuration.
