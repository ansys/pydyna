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
# # Belted Dummy Crash Simulation with LS-DYNA Python API
#
# This notebook demonstrates how to use the PyDYNA ``pre`` service to create and simulate a belted dummy crash scenario.
# The workflow covers model import, material and section assignment, contact and joint definition, boundary and initial
# conditions, and output configuration. Each section provides both code and theoretical context for the simulation steps.
#
# ---

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules, data arrays, and LS-DYNA Python API classes. This step ensures all necessary libraries and data
# are available for the simulation.
# %%
import os
import sys

from belted_dummy_data import (
    curvedata,
    dampingconst,
    elasticmats,
    extra_nodes,
    jointlist,
    lcidlist,
    motion_curve_x,
    motion_curve_y,
    motion_nodes,
    nlist,
    rigidmats,
    segments,
    shellsec,
    vector,
)

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import (
    MatDamperNonlinearViscous,
    MatDamperViscous,
    MatElastic,
    MatRigid,
    MatSpringNonlinearElastic,
)
from ansys.dyna.core.pre.dynamech import (
    DRO,
    Contact,
    ContactCategory,
    ContactSurface,
    Curve,
    DiscretePart,
    DynaMech,
    Gravity,
    GravityOption,
    Motion,
    NodeSet,
    SegmentSet,
    ShellFormulation,
    ShellPart,
    Velocity,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/explicit/belted_dummy.png'

# %% [markdown]
# ### 2. Start the Pre-Service and Load Model
# Start the ``pre`` service and load the belted dummy model from the input key file. This prepares the solver for launching
# and loads the geometry and mesh for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
dummy_solution = launch_dynapre(ip=hostname)
fns = []
path = examples.belted_dummy + os.sep
fns.append(path + "belted_dummy.k")
dummy_solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Control and Output Database
# Set simulation termination time, timestep, and output frequency. This determines how long the simulation runs and how
# frequently results are saved for analysis.
# %%
dummy_solution.set_termination(termination_time=0.12)
dummy_solution.create_database_binary(dt=2.5e-3)
dummy = DynaMech()
dummy_solution.add(dummy)
dummy.set_timestep(tssfac=0.8)
dummy.set_init_velocity(Velocity(14.8, 0, 0))

# %% [markdown]
# ### 4. Material Definitions
# Define rigid, elastic, spring, and damper materials for the dummy and restraint system. These properties control the
# mechanical response of each part during impact.
# %%
shellmatlist = []
for i in range(15):
    matrigid = MatRigid(mass_density=rigidmats[i][0], young_modulus=rigidmats[i][1], poisson_ratio=0.3)
    shellmatlist.append(matrigid)
for i in range(16, 23):
    index = i - 16
    matelastic = MatElastic(mass_density=elasticmats[index][0], young_modulus=elasticmats[index][1], poisson_ratio=0.3)
    shellmatlist.append(matelastic)
discmatlist = []
for i in range(101, 143):
    index = i - 101
    mat = MatSpringNonlinearElastic(curve=Curve(x=curvedata[index][0], y=curvedata[index][1]))
    discmatlist.append(mat)
for i in range(143, 185):
    index = i - 143
    mat = MatDamperViscous(damping_constant=dampingconst[index])
    discmatlist.append(mat)
for i in range(185, 209):
    index = i - 185
    mat = MatDamperNonlinearViscous(curve=Curve(x=curvedata[lcidlist[index]][0], y=curvedata[lcidlist[index]][1]))
    discmatlist.append(mat)

# %% [markdown]
# ### 5. Section Properties and Part Assignment
# Assign materials to parts and define section properties, element formulations, and thickness. This step ensures the mesh
# and material models are correctly associated with the geometry.
# %%
for i in range(1, 23):
    part = ShellPart(i)
    part.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    part.set_material(shellmatlist[i - 1])
    part.set_thickness(shellsec[i - 1][0])
    part.set_integration_points(shellsec[i - 1][1])
    if i in range(1, 16):
        part.set_extra_nodes(NodeSet(extra_nodes[i - 1]))
    dummy.parts.add(part)
for i in range(101, 209):
    index = i - 101
    part = DiscretePart(i)
    part.set_material(discmatlist[index])
    part.set_displacement_option(displacement_option=DRO.DESCRIBES_TORSIONAL_SPRING)
    dummy.parts.add(part)

# %% [markdown]
# ### 6. Contact Definition
# Define multiple surface-to-surface contacts between segment sets, each with a specific friction coefficient. This models
# the interaction and force transfer between dummy parts and restraints.
# %%
fslist = [0.62, 0.62, 0.62, 0.8, 1, 0.8, 0.88, 0.88, 0.16, 0.88, 0]
for i in range(11):
    contact = Contact(category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    contact.set_friction_coefficient(static=fslist[i])
    surf1 = ContactSurface(SegmentSet(segments[2 * i]))
    surf2 = ContactSurface(SegmentSet(segments[2 * i + 1]))
    contact.set_slave_surface(surf1)
    contact.set_master_surface(surf2)
    dummy.contacts.add(contact)

# %% [markdown]
# ### 7. Joint and Constraint Definition
# Define spherical joints and orientation constraints using node pairs. This models anatomical joints and mechanical
# connections in the dummy.
# %%
for i in range(42):
    id = i + 1
    dummy.create_defineorientation(vid=id, iop=2, vector=vector, node1=nlist[i][0], node2=nlist[i][1])
for i in range(14):
    dummy.constraints.create_joint_spherical(nodes=jointlist[i])

# %% [markdown]
# ### 8. Prescribed Motion and Boundary Conditions
# Apply imposed motion to a node set using a motion curve, and set up any additional boundary conditions.
# %%
dummy.boundaryconditions.create_imposed_motion(
    NodeSet(motion_nodes),
    Curve(x=motion_curve_x, y=motion_curve_y, sfo=0.1),
    motion=Motion.ACCELERATION,
    scalefactor=-1,
)

# %% [markdown]
# ### 9. Gravity Definition
# Apply gravity loading to the model using a time-history curve.
# %%
g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0, 0.152], y=[9.81, 9.81]))
dummy.add(g)

# %% [markdown]
# ### 10. Output Requests and File Saving
# Configure output requests and save the model setup for LS-DYNA execution. This enables post-processing and analysis of
# the simulation results.
# %%
dummy_solution.create_database_binary(dt=2.5e-3)
dummy_solution.save_file()

# %% [markdown]
# ### 11. Conclusion
# In this example, we demonstrated the setup and simulation of a belted dummy crash scenario using the LS-DYNA Python API.
# The workflow included model import, material and section assignment, contact and joint definition, boundary and initial
# conditions, and output configuration. By leveraging the PyDYNA pre-service, users can efficiently build complex occupant
# safety models, automate repetitive tasks, and ensure simulation repeatability. The approach supports rapid prototyping
# and design optimization for crashworthiness, restraint system evaluation, and injury prediction in automotive safety
# engineering.
