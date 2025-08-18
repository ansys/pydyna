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
# # SALE EFP Example: Explosively Formed Projectile Simulation
#
# This notebook demonstrates how to set up and solve an Explosively Formed Projectile (EFP) simulation using the SALE (Simplified Arbitrary Lagrangian Eulerian) solver in LS-DYNA via the PyDyna API. The workflow includes mesh import, material filling, mesh and control point definition, initial conditions, and output configuration. Each step is explained with theoretical background for educational use.
#
# **Background:**
# EFP simulations are used to study the formation and behavior of projectiles created by the detonation of high explosives. The SALE solver in LS-DYNA is designed for multi-material, multi-phase flow problems, making it ideal for simulating explosive events, air, vacuum, and liner interactions in a structured mesh.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the EFP simulation.
#
# The PyDyna API provides high-level abstractions for material, mesh, and solver setup. Here, we import modules for material database, mesh generation, and SALE solver control.

# %%
import os
import sys

from ansys.dyna.core.pre import dynamaterial as matDB
from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynasale import AdvectionMethod, ControlPoint, DynaSALE, FillDirection, Point, StructuredMesh
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ## 2. Start the Pre-Service
# Ensure the Docker container or local server for the `pre` service is running. Connect to it using the default hostname and port, or override via command line.
#
# The `pre` service is responsible for preprocessing and keyword generation. It must be running before you can connect and build your model programmatically.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

efp_solution = launch_dynapre(ip=hostname)

# %% [markdown]
# ## 3. Import Initial Mesh Data
# Import the initial mesh data (nodes and elements) for the EFP simulation.
#
# The mesh defines the computational domain for the simulation. For EFP problems, a structured mesh is often used to accurately capture the propagation of shock waves and material interfaces.

# %%
fns = []
path = examples.sale_efp + os.sep
fns.append(path + "efpcase.k")
efp_solution.open_files(fns)

# %% [markdown]
# ## 4. Set Termination Time
# Set the simulation end time to ensure the analysis runs for the desired physical duration.
#
# The termination time should be chosen to capture the full evolution of the projectile and shock wave propagation.

# %%
efp_solution.set_termination(280)

# %% [markdown]
# ## 5. Create SALE Object and Set Output Interval
# Create the SALE analysis object, add it to the solution, and set the output interval for post-processing.
#
# The output interval controls how frequently results are written, which is important for capturing transient phenomena in explosive simulations.

# %%
efp = DynaSALE()
efp_solution.add(efp)

efp.set_output_interval(5.0)

# %% [markdown]
# ## 6. Set Analysis Type
# Set the advection method for the SALE solver.
#
# The advection method determines how material interfaces and flow are handled. The Van Leer method with HIS (High-Resolution Interface Sharpening) is used for improved accuracy in multi-material flows.

# %%
efp.set_analysis_type(method=AdvectionMethod.VAN_LEER_WITH_HIS)

# %% [markdown]
# ## 7. Define Structured Mesh and Control Points
# Define the mesh using control points in the X, Y, and Z directions.
#
# Control points allow for non-uniform mesh spacing, which is useful for refining the mesh in regions of interest (e.g., near the explosive or liner). The mesh is then created as a structured grid.

# %%
control_points_x = [
    ControlPoint(number=1, position=0, ratio=1),
    ControlPoint(number=11, position=-2.5, ratio=0.5),
    ControlPoint(number=21, position=0, ratio=0.5),
    ControlPoint(number=31, position=0, ratio=1),
]

control_points_y = [
    ControlPoint(number=1, position=0, ratio=1),
    ControlPoint(number=11, position=-2.5, ratio=0.5),
    ControlPoint(number=21, position=0, ratio=0.5),
    ControlPoint(number=31, position=0, ratio=1),
]

control_points_z = [
    ControlPoint(number=1, position=0, ratio=0.5),
    ControlPoint(number=269, position=11, ratio=0.25),
    ControlPoint(number=309, position=21, ratio=0.25),
    ControlPoint(number=339, position=0, ratio=5),
]

mesh = StructuredMesh(control_points_x, control_points_y, control_points_z)
efp.add(mesh)

# %% [markdown]
# ## 8. Fill Mesh with Materials
# Fill the mesh with vacuum, air, high explosive, and liner materials.
#
# Multi-material filling is essential for EFP simulations. Vacuum and air are used for the background, while the high explosive and liner are filled in specific regions to model the projectile formation.

# %%
vacuum = matDB.Vacuum()
mesh.fill(vacuum)
air = matDB.Air()
mesh.fill(air, geometry_type="ALL", reference_pressure=1.01325e-6)
he = matDB.HighExplosive()
mesh.fill(
    he,
    geometry_type="PART",
    define_geometry_parameters=[23],
    inout=FillDirection.OUTSIDE_THE_GEOMETRY,
)
liner = matDB.Liner()
mesh.fill(
    liner,
    geometry_type="PART",
    define_geometry_parameters=[22],
    inout=FillDirection.OUTSIDE_THE_GEOMETRY,
)

# %% [markdown]
# ## 9. Set Initial Conditions
# Set the initial detonation point for the high explosive.
#
# The initial conditions define where the detonation starts, which is critical for accurately modeling the formation and acceleration of the projectile.

# %%
mesh.initial_detonation(Point(0, 0, 19.33))

# %% [markdown]
# ## 10. Set Output Database and Save Input File
# Configure output database frequencies and save the input file for LS-DYNA.
#
# Output database settings control how often results are written and which quantities are saved. This is important for post-processing and validation of the simulation results.

# %%
efp_solution.set_output_database(matsum=0.2, glstat=0.2)
efp_solution.save_file()

# %% [markdown]
# ## 11. Conclusion
#
# This notebook has demonstrated the setup and solution of an Explosively Formed Projectile (EFP) simulation using the SALE solver in LS-DYNA and PyDyna. The workflow included mesh import, material filling, mesh and control point definition, initial conditions, and output configuration. This approach can be adapted for other multi-material, multi-phase flow problems, providing a clear, modular, and scriptable workflow for advanced simulations.
