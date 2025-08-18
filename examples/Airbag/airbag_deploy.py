# %% [markdown]
# # Airbag Deployment Simulation with LS-DYNA and PyDyna
#
# This notebook demonstrates how to set up and solve an airbag deployment problem using LS-DYNA and PyDyna. The workflow includes mesh import, material and section definition, airbag and contact setup, rigid wall definition, and output configuration. Each step is explained for clarity and educational use.
#
# ## Theory and Background
#
# Airbag deployment simulations are critical in automotive safety engineering. They involve complex interactions between fabric materials, gas dynamics, and contact with rigid and deformable surfaces. LS-DYNA provides specialized airbag models and contact algorithms to capture these phenomena. This example uses the SIMPLE_AIRBAG_MODEL and demonstrates the setup of materials, parts, contacts, and output requests for a typical deployment scenario.

# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
# SPDX-License-Identifier: MIT
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the airbag deployment simulation.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatFabric, MatRigid
from ansys.dyna.core.pre.dynamech import (
    Airbag,
    Contact,
    ContactCategory,
    ContactSurface,
    Curve,
    DynaMech,
    PartSet,
    Point,
    RigidwallPlanar,
    ShellFormulation,
    ShellPart,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ## 2. Start the Pre-Service
# Start the LS-DYNA pre-service (locally or via Docker) and connect to it.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

# %% [markdown]
# ## 3. Import Mesh and Model Data
# Read nodes, elements, and part definitions from the input file.

# %%
airbag_solution = launch_dynapre(ip=hostname)
fns = []
path = examples.airbag_deploy + os.sep
fns.append(path + "airbag_deploy.k")
airbag_solution.open_files(fns)

# %% [markdown]
# ## 4. Set Simulation Termination Time and Add Solution Object
# Set the simulation termination time and add the DynaMech solution object.

# %%
airbag_solution.set_termination(0.03)
airbagdeploy = DynaMech()
airbag_solution.add(airbagdeploy)

# %% [markdown]
# ## 5. Define Airbag Model
# Use the Airbag function to define the SIMPLE_AIRBAG_MODEL keyword and set airbag parameters.

# %%
airbag = Airbag(
    set=PartSet([3]),
    heat_capacity_at_constant_volume=1.736e3,
    heat_capacity_at_constant_pressure=2.43e3,
    input_gas_temperature=1.2e3,
    input_mass_flow_rate=Curve(x=[0, 0.032, 0.045, 0.08], y=[0, 26, 0.6, 0.1]),
    shape_factor_for_exit_hole=0.7,
    ambient_pressure=14.7,
    ambient_density=3.821e-6,
)
airbagdeploy.add(airbag)

# %% [markdown]
# ## 6. Generate an Infinite Planar Rigid Wall
# Define the coordinates for the rigid wall.

# %%
rigidwall = RigidwallPlanar(Point(0, 0, 0), Point(0, 1, 0), coulomb_friction_coefficient=0.5)
airbagdeploy.add(rigidwall)

# %% [markdown]
# ## 7. Define Node-to-Surface Contact
# Set up contact between the airbag and other parts.

# %%
contact = Contact(category=ContactCategory.NODES_TO_SURFACE)
contact.set_friction_coefficient(static=0.5, dynamic=0.5)
surf1 = ContactSurface(PartSet([3]))
surf2 = ContactSurface(PartSet([2]))
surf2.set_penalty_stiffness_scale_factor(0.06667)
contact.set_slave_surface(surf1)
contact.set_master_surface(surf2)
airbagdeploy.contacts.add(contact)

# %% [markdown]
# ## 8. Define Material Cards
# Assign material properties to the plate, cylinder, and airbag parts.

# %%
platemat = MatRigid(
    mass_density=7.84e-4,
    young_modulus=30e6,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
cylindermat = MatRigid(mass_density=1.96e-4, young_modulus=30e6)
airbagmat = MatFabric(
    mass_density=1e-4,
    young_modulus_longitudinal_direction=2e6,
    young_modulus_transverse_direction=2e6,
    shear_modulus=1.53e6,
)

# %% [markdown]
# ## 9. Define Sectional Properties
# Set up shell parts and assign materials and formulations.

# %%
plate = ShellPart(1)
plate.set_material(platemat)
plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
plate.set_thickness(0.5)
airbagdeploy.parts.add(plate)

cylinder = ShellPart(2)
cylinder.set_material(cylindermat)
cylinder.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
cylinder.set_thickness(0.5)
airbagdeploy.parts.add(cylinder)

airbagpart = ShellPart(3)
airbagpart.set_material(airbagmat)
airbagpart.set_element_formulation(ShellFormulation.FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE)
airbagpart.set_thickness(0.015)
airbagpart.set_integration_points(4)
airbagdeploy.parts.add(airbagpart)

# %% [markdown]
# ## 10. Define Database Outputs and Save Input File
# Configure output frequencies and save the input deck to disk.

# %%
airbag_solution.set_output_database(
    abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4, rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4
)
airbag_solution.create_database_binary(dt=5e-4, ieverp=1)
airbag_solution.save_file()

# %% [markdown]
# ## 11. Conclusion
# This notebook demonstrated the setup of an airbag deployment simulation using LS-DYNA and PyDyna. The workflow included mesh import, material and section definition, airbag and contact setup, rigid wall definition, and output configuration. This approach provides a clear, modular, and scriptable workflow for advanced safety simulations.
