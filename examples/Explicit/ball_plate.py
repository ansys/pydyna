# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
# SPDX-License-Identifier: MIT
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
# # Ball Plate Impact Simulation with LS-DYNA Python API
#
# This notebook demonstrates how to use the PyDYNA ``pre`` service to create and simulate a ball plate
# impact scenario. The workflow covers model setup, material and section assignment, boundary and initial
# conditions, contact definition, and output configuration. Each section provides both code and theoretical
# context for the simulation steps.
#
# ---

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules and LS-DYNA Python API classes. This step ensures all necessary libraries and data
# are available for the simulation.
# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatPiecewiseLinearPlasticity, MatRigid
from ansys.dyna.core.pre.dynamech import (
    AnalysisType,
    Contact,
    ContactSurface,
    ContactType,
    DynaMech,
    NodeSet,
    PartSet,
    ShellFormulation,
    ShellPart,
    SolidFormulation,
    SolidPart,
    Velocity,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/explicit/ball_plate.png'

# %% [markdown]
# ### 2. Start the Pre-Service and Load Model
# Start the ``pre`` service and load the ball plate model from the input key file. This prepares the solver
# for launching and loads the geometry and mesh for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.ball_plate + os.sep
fns.append(path + "ball_plate.k")
solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Control and Output Database
# Set simulation termination time, timestep, and output frequency. This determines how long the simulation
# runs and how frequently results are saved for analysis.
# %%
solution.set_termination(termination_time=10)

ballplate = DynaMech(AnalysisType.NONE)
solution.add(ballplate)

# %% [markdown]
# ### 4. Material Definitions
# Define rigid and piecewise linear plasticity materials for the ball and plate. These properties control
# the mechanical response of each part during impact.
# %%
matrigid = MatRigid(mass_density=7.83e-6, young_modulus=207, poisson_ratio=0.3)
matplastic = MatPiecewiseLinearPlasticity(mass_density=7.83e-6, young_modulus=207, yield_stress=0.2, tangent_modulus=2)

# %% [markdown]
# ### 5. Section Properties and Part Assignment
# Assign materials to parts and define section properties, element formulations, and thickness. This step
# ensures the mesh and material models are correctly associated with the geometry.
# %%
plate = ShellPart(1)
plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
plate.set_material(matplastic)
plate.set_thickness(1)
plate.set_integration_points(5)
ballplate.parts.add(plate)

ball = SolidPart(2)
ball.set_material(matrigid)
ball.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
ballplate.parts.add(ball)

# %% [markdown]
# ### 6. Contact Definition
# Define a single-surface contact between the ball and plate using a predefined part set. This models the
# interaction and force transfer during impact.
# %%
selfcontact = Contact(type=ContactType.AUTOMATIC)
surf1 = ContactSurface(PartSet([1, 2]))
selfcontact.set_slave_surface(surf1)
ballplate.contacts.add(selfcontact)

# %% [markdown]
# ### 7. Boundary Conditions
# Constrain nodes in a list of single point constraints (spc) to fix the plate and prevent rigid body motion.
# %%
spc = [
    34,
    35,
    51,
    52,
    68,
    69,
    85,
    86,
    102,
    103,
    119,
    120,
    136,
    137,
    153,
    154,
    170,
    171,
    187,
    188,
    204,
    205,
    221,
    222,
    238,
    239,
    255,
    256,
]
for i in range(1, 19):
    spc.append(i)
for i in range(272, 290):
    spc.append(i)
ballplate.boundaryconditions.create_spc(NodeSet(spc), rx=False, ry=False, rz=False)

# %% [markdown]
# ### 8. Initial Conditions
# Initialize the velocity of all nodes to simulate the impact. This sets the initial downward velocity of the ball.
# %%
for i in range(1, 1652):
    ballplate.initialconditions.create_velocity_node(i, trans=Velocity(0, 0, -10))

# %% [markdown]
# ### 9. Output Requests and File Saving
# Configure output requests for global statistics, material summary, and element output. Save the model setup
# for LS-DYNA execution. This enables post-processing and analysis of the simulation results.
# %%
solution.set_output_database(glstat=0.1, matsum=0.1, sleout=0.1)
solution.create_database_binary(dt=1)
serverpath = solution.save_file()

# %% [markdown]
# ### 10. Download Output File
# Download the output file from the server to the local output directory for further analysis and visualization.
# %%
serveroutfile = "/".join((serverpath, "ball_plate.k"))
downloadpath = os.path.join(os.getcwd(), "output")
if not os.path.exists(downloadpath):
    os.makedirs(downloadpath)
downloadfile = os.path.join(downloadpath, "ball_plate.k")
solution.download(serveroutfile, downloadfile)

# %% [markdown]
# ### 11. Conclusion
# This example demonstrated the setup and simulation of a ball plate impact scenario using the LS-DYNA Python
# API. By defining materials, parts, contact, boundary and initial conditions, and output requests, we modeled
# the dynamic response of the system under impact loading. The workflow can be extended to more complex
# geometries, loading conditions, and multiphysics analyses, supporting advanced engineering and research
# applications.
