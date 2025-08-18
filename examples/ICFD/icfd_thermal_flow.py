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
# # Thermal Flow Simulation with LS-DYNA Python API
#
# This example demonstrates how to set up and solve a thermal flow (forced convection) problem using the LS-DYNA Python API.
#
#
# ## 1. Imports and Data Setup
# Import required modules and LS-DYNA Python API classes. This step ensures all necessary libraries and data are available for the simulation.
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

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/thermal_flow.png'

# %% [markdown]
# ## 2. LS-DYNA Executable and File Paths
# Set up the LS-DYNA server hostname and input file paths. This prepares the solver for launching and loads the model for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.thermal_flow + os.sep
fns.append(path + "thermal_flow.k")
solution.open_files(fns)

# %% [markdown]
# ## 3. Simulation Control and Output Database
# Set total simulation time and configure output database. This determines how long the simulation runs and how frequently results are saved for analysis.
# %%
solution.set_termination(termination_time=100)

# %% [markdown]
# ## 4. ICFD Model Setup
# Create and configure the ICFD model. Set solver parameters for accurate and stable simulation of thermal flow.
# %%
icfd = DynaICFD()
solution.add(icfd)
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfd.add(icfdanalysis)

# %% [markdown]
# ## 5. Material and Part Definitions
# Define and assign material properties to the parts. Set up boundary conditions for inflow, outflow, symmetry, and wall, including prescribed temperatures and velocities.
# %%
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005, heat_capacity=1000, thermal_conductivity=200)
part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
part_inflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[20, 20]))
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
part_wall.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[80, 80]))
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=3)
icfd.parts.add(part_wall)

# %% [markdown]
# ## 6. Initial Conditions
# Set the initial temperature for the entire domain. This is important for transient thermal simulations.
# %%
icfd.set_initial(temperature=10)

# %% [markdown]
# ## 7. Volume and Meshing
# Define the volume space to be meshed and assign the boundaries.
# %%
partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)
meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)

# %% [markdown]
# ## 8. Output Requests and Save
# Configure output requests and save the simulation setup.
# %%
solution.create_database_binary(dt=1)
solution.save_file()

# %% [markdown]
# ## 9. Conclusion
# In this example, we demonstrated how to set up a thermal flow (forced convection) simulation using the LS-DYNA Python API. The workflow included model setup, material and part definitions, boundary conditions, initial conditions, and output configuration. This approach can be adapted for a wide range of thermal-fluid problems in engineering and research.
