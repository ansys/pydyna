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
# # Driven Cavity Flow Simulation with LS-DYNA ICFD
#
# This example demonstrates how to set up and solve the classic driven cavity problem using the LS-DYNA ICFD solver via PyDyna.
# The driven cavity is a standard benchmark in computational fluid dynamics (CFD), where a fluid is contained in a square cavity and the top lid moves at a constant velocity, driving the flow.
#
# This notebook-style script is formatted for Jupytext/Jupyter Lab compatibility and includes detailed explanations for educational use.
#
#
#
# **Contents:**
# 1. Problem Description and Theory
# 2. Environment Setup
# 3. Model Definition
# 4. Boundary and Initial Conditions
# 5. Meshing and Solution Setup
# 6. Running the Simulation
# 7. Results and Conclusion
#
#

# %% [markdown]
# ## 1. Problem Description and Theory
#
# The driven cavity problem consists of a square domain filled with a viscous fluid. The top boundary (lid) moves at a constant velocity, while the other walls are stationary and enforce a no-slip condition.
# This setup is widely used to test CFD codes due to its simple geometry and well-known flow features, such as the formation of primary and secondary vortices.
#
# **Governing equations:**
# - Incompressible Navier-Stokes equations
# - Steady-state solution
#
# **Key parameters:**
# - Fluid density: 1 kg/m³
# - Dynamic viscosity: 0.001 Pa·s
# - Lid velocity: 1 m/s
#
#

# %% [markdown]
# ## 2. Environment Setup
#
# Import required modules and set up the LS-DYNA pre-processor connection. The example uses the PyDyna API to interact with LS-DYNA.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFD_AnalysisType,
    ICFD_MessageLevel,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ### Set up the LS-DYNA pre-processor connection
#
# By default, the solver runs on localhost. You can specify a different host by passing an IP address as a command-line argument.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)

# %% [markdown]
# ## 3. Model Definition
#
# Import the mesh and define the ICFD model. The mesh file describes the geometry and discretization of the cavity.

# %%
# Import the initial mesh data (nodes and elements)
fns = []
path = examples.driven_cavity + os.sep
fns.append(path + "driven_cavity.k")
solution.open_files(fns)

icfd = DynaICFD()
solution.add(icfd)

# %% [markdown]
# ## 4. Boundary and Initial Conditions
#
# Define the material properties and boundary conditions for the cavity walls and lid.
# - The top wall (part 1) is assigned a prescribed velocity (lid-driven).
# - The other walls (part 2) are set as no-slip boundaries.

# %%
# Define fluid material properties
mat = MatICFD(flow_density=1, dynamic_viscosity=0.001)

# Top wall (lid) with prescribed velocity
part1 = ICFDPart(1)
part1.set_material(mat)
part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part1)

# Other walls (no-slip)
part2 = ICFDPart(2)
part2.set_material(mat)
part2.set_non_slip()
icfd.parts.add(part2)

# %% [markdown]
# ## 5. Meshing and Solution Setup
#
# Define the volume to be meshed and set up the analysis parameters for a steady-state incompressible flow.

# %%
# Define the volume space that will be meshed (the boundaries are the surfaces "spids")
partvol = ICFDVolumePart(surfaces=[1, 2])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[1, 2])
icfd.add(meshvol)

# Set up the analysis
icfdanalysis = ICFDAnalysis()
icfdanalysis.set_type(analysis_type=ICFD_AnalysisType.STEADY_STATE_ANALYSIS)
icfdanalysis.set_output(messagelevel=ICFD_MessageLevel.FULL_OUTPUT_INFORMATION, iteration_interval=250)
icfdanalysis.set_steady_state(
    max_iteration=2500,
    momentum_tol_limit=1e-8,
    pressure_tol_limit=1e-8,
    velocity_relax_param=1,
    pressure_relax_param=1,
)
icfd.add(icfdanalysis)

# %% [markdown]
# ## 6. Running the Simulation
#
# Create the binary database and save the setup. The simulation can then be run using LS-DYNA.

# %%
solution.create_database_binary(dt=250)
solution.save_file()

# %% [markdown]
# ## 7. Results and Conclusion
#
# The driven cavity simulation is now set up and ready to run in LS-DYNA. After running the solver, you can post-process the results to analyze the velocity and pressure fields, and compare the primary vortex structure with benchmark data.
#
# **Key takeaways:**
# - The driven cavity is a fundamental CFD benchmark for validating incompressible flow solvers.
# - PyDyna provides a high-level Python interface for setting up and automating LS-DYNA workflows.
# ---
#
