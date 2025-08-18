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
# # ISPH Rigid Body Test Example
#
# This notebook demonstrates how to set up and run a rigid body test using the Incompressible Smoothed Particle Hydrodynamics (ISPH) solver in LS-DYNA via the PyDyna API. The workflow includes mesh import, material and part definition, boundary and initial conditions, and output configuration. Each step is explained for clarity and educational use.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the ISPH simulation.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaisph import (
    DOF,
    Box,
    Curve,
    DynaISPH,
    Gravity,
    GravityOption,
    ISPHFluidPart,
    ISPHStructPart,
    Motion,
    PartSet,
    Point,
    ShellFormulation,
    ShellPart,
)
from ansys.dyna.core.pre.dynamaterial import MatRigid, MatSPHIncompressibleFluid, MatSPHIncompressibleStructure
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ## 2. Start the Pre-Service
# Ensure the Docker container or local server for the `pre` service is running. Connect to it using the default hostname and port, or override via command line.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
isphsolution = launch_dynapre(ip=hostname)

# %% [markdown]
# ## 3. Import Initial Mesh Data
# Import the mesh data for the rigid body test example.

# %%
fns = []
path = examples.isph_rigidtest + os.sep
fns.append(path + "rigidtest.k")
isphsolution.open_files(fns)

# %% [markdown]
# ## 4. Set Termination Time
# Define the simulation end time.

# %%
isphsolution.set_termination(0.5)

# %% [markdown]
# ## 5. Create ISPH Object and Set Analysis Parameters
# Create the ISPH analysis object, add it to the solution, and set timestep and box parameters.

# %%
isphobj = DynaISPH()
isphsolution.add(isphobj)
isphobj.set_timestep(tssfac=1, max_timestep=Curve(x=[0, 0.04, 0.05, 0.1, 100], y=[0.5, 0.5, 1, 1, 1]))
isphobj.isphanalysis.set_box(Box(-750, 800, -800, 800, -100, 3000))

# %% [markdown]
# ## 6. Define Materials
# Define all required material models for the simulation, including rigid and SPH fluid/structure materials.

# %%
platemat1 = MatRigid(
    mass_density=1e-9,
    young_modulus=10,
    center_of_mass_constraint=1,
    translational_constraint=5,
    rotational_constraint=4,
)
platemat2 = MatRigid(
    mass_density=1e-9,
    young_modulus=10,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
matsphfluid = MatSPHIncompressibleFluid(
    mass_density=1e-9, dynamic_viscosity=1e-9, tension_coefficient1=1e6, tension_coefficient2=1000
)
matsphstruct = MatSPHIncompressibleStructure(mass_density=1e-9)

# %% [markdown]
# ## 7. Define Parts and Assign Materials
# Create shell and SPH parts, assign materials, and set element formulations and thicknesses.

# %%
sensorplane = ShellPart(1)
sensorplane.set_material(platemat2)
sensorplane.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
sensorplane.set_thickness(0.1)
isphobj.parts.add(sensorplane)

movingcube = ShellPart(7)
movingcube.set_material(platemat1)
movingcube.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
movingcube.set_thickness(0.1)
isphobj.parts.add(movingcube)

wallsmesh = ShellPart(8)
wallsmesh.set_material(platemat2)
wallsmesh.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
wallsmesh.set_thickness(0.1)
isphobj.parts.add(wallsmesh)

sphwall = ISPHStructPart(4, PartSet([8]), 12)
sphwall.set_material(matsphstruct)
sphwall.set_smoothing_length(1, 1, 1, 12)
isphobj.parts.add(sphwall)

sphcube = ISPHStructPart(5, PartSet([7]), 12)
sphcube.set_material(matsphstruct)
sphcube.set_smoothing_length(1, 1, 1, 12)
isphobj.parts.add(sphcube)

sphwater = ISPHFluidPart(6, Point(-588, -588, 9), Point(1176, 1176, 204), 98, 98, 17)
sphwater.set_material(matsphfluid)
sphwater.set_smoothing_length(1, 1, 1, 12)
sphwater.create_massflow_plane(PartSet([1]))
isphobj.parts.add(sphwater)

# %% [markdown]
# ## 8. Define Constraints and Boundary Conditions
# Merge rigid bodies and apply imposed motions to the moving cube.

# %%
# Constraint
isphobj.constraints.merge_two_rigid_bodies(7, 1)

# Define boundary conditions
isphobj.boundaryconditions.create_imposed_motion(
    PartSet([7]),
    Curve(x=[0, 0.1, 0.11, 20], y=[3000, 3000, 0, 0]),
    dof=DOF.X_TRANSLATIONAL,
    motion=Motion.VELOCITY,
)
isphobj.boundaryconditions.create_imposed_motion(
    PartSet([7]),
    Curve(x=[0, 0.1, 0.11, 20], y=[500, 500, 500, 500]),
    dof=DOF.Z_ROTATIONAL,
    motion=Motion.VELOCITY,
    scalefactor=0.01,
)

# %% [markdown]
# ## 9. Apply Loads
# Add gravity to the simulation.

# %%
g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0, 100], y=[9810, 9810]))
isphobj.add(g)

# %% [markdown]
# ## 10. Set Output Database and Save Input File
# Configure output database frequencies and save the input file for LS-DYNA.

# %%
isphsolution.set_output_database(glstat=0.001, sphmassflow=0.001)
isphsolution.create_database_binary(dt=0.01)
isphsolution.save_file()

# %% [markdown]
# ## 11. Conclusion
#
# This notebook has demonstrated the setup of a rigid body test using the ISPH solver in LS-DYNA with PyDyna. The workflow included mesh import, material and part definition, boundary and initial conditions, and output configuration. This approach can be adapted for other ISPH analyses, providing a clear, modular, and scriptable workflow for advanced simulations.
#
