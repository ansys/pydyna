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
# # Dam Break Simulation with LS-DYNA Python API
#
# This notebook demonstrates how to use the PyDYNA ``pre`` service to set up and simulate a classic dam break
# scenario using the incompressible CFD (ICFD) solver. The workflow covers mesh import, material and part definition,
# gravity and boundary condition setup, and output configuration. Each section provides both code and theoretical
# context for the simulation steps.
#


# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules and LS-DYNA Python API classes. This step ensures all necessary libraries and data are
# available for the simulation.
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

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/dam_break.png'

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Set up the LS-DYNA server hostname and input file paths. This prepares the solver for launching and loads the model
# for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
# Import the initial mesh data (nodes and elements)
fns = []
path = examples.dam_break + os.sep
fns.append(path + "dam_break.k")
solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Control and Output Database
# Set total simulation time and configure output database. This determines how long the simulation runs and how
# frequently results are saved for analysis.
# %%
solution.set_termination(termination_time=50)

# %% [markdown]
# ### 4. ICFD Model and Analysis Setup
# Create and configure the ICFD model and analysis parameters. This includes timestep control and solver settings.
# %%
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep()
icfd.add(icfdanalysis)

# %% [markdown]
# ### 5. Material and Part Definitions
# Define the fluid and vacuum materials, and assign them to all boundary and volume parts. Set up free-slip and
# standard boundary conditions, as well as the main flow volume.
# %%
mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
mat2 = MatICFD(flag=Compressible.VACUUM)

part1 = ICFDPart(1)
part1.set_material(mat1)
part1.set_free_slip()
icfd.parts.add(part1)

part2 = ICFDPart(2)
part2.set_material(mat2)
part2.set_free_slip()
icfd.parts.add(part2)

part3 = ICFDPart(3)
part3.set_material(mat1)
icfd.parts.add(part3)

# %% [markdown]
# ### 6. Gravity Definition
# Apply gravity loading to the model using a time-history curve.
# %%
g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[9.81, 9.81]))
icfd.add(g)

# %% [markdown]
# ### 7. Volume and Mesh Definition
# Define the volume spaces that will be meshed. The boundaries of the volumes are the surfaces (spids).
# %%
partvol1 = ICFDVolumePart(surfaces=[1, 3])
partvol1.set_material(mat1)
icfd.parts.add(partvol1)

partvol2 = ICFDVolumePart(surfaces=[2, 3])
partvol2.set_material(mat2)
icfd.parts.add(partvol2)

meshvol = MeshedVolume(surfaces=[1, 2])
meshvol.set_fluid_interfaces([3])
icfd.add(meshvol)

# %% [markdown]
# ### 8. Output Requests and File Saving
# Configure output requests and save the model setup for LS-DYNA execution. This enables post-processing and analysis
# of the simulation results.
# %%
solution.create_database_binary(dt=0.2)
solution.save_file()

# %% [markdown]
# ### 9. Conclusion
# In this example, we demonstrated the setup and simulation of a dam break scenario using the LS-DYNA Python API.
# The workflow included mesh import, material and part definition, gravity and boundary condition setup, and output
# configuration. By leveraging the PyDYNA pre-service and ICFD solver, users can efficiently build and analyze free
# surface flow models for engineering research and design. This approach supports rapid prototyping, parametric studies,
# and multiphysics extensions for advanced fluid dynamics applications.
