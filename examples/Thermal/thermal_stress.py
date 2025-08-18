# %% [markdown]
# # Thermal Stress Analysis with Temperature-Dependent Material Properties
#
# This notebook demonstrates how to set up and solve a thermal stress problem using LS-DYNA and PyDyna. The workflow includes mesh import, material and section definition with temperature-dependent properties, thermal and mechanical solver setup, initial condition assignment, and input deck export. Each step is explained for clarity and educational use.
#
# ## Theory and Background
#
# Thermal stress analysis is essential in engineering to predict the response of structures subjected to temperature changes. When a material is heated or cooled, it expands or contracts, generating internal stresses if deformation is constrained. Accurately modeling these effects requires temperature-dependent material properties, such as Young's modulus, Poisson's ratio, thermal expansion coefficient, and yield stress.
#
# This example demonstrates:
# - Importing a mesh and defining a part with empty material/section fields.
# - Assigning a temperature-dependent elastic-plastic material with thermal isotropic properties.
# - Setting up a transient thermal analysis and initializing nodal temperatures.
# - Configuring output requests and saving the input deck for LS-DYNA.
#
# This workflow provides a robust foundation for simulating coupled thermal-mechanical problems in research and engineering applications.

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
# Import all necessary modules and classes for the thermal stress simulation.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatElasticPlasticThermal
from ansys.dyna.core.pre.dynamech import (
    AnalysisType,
    DynaMech,
    NodeSet,
    SolidFormulation,
    SolidPart,
    ThermalAnalysis,
    ThermalAnalysisType,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ## 2. Start the Pre-Service
# Start the LS-DYNA pre-service (locally or via Docker) and connect to it.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)

# %% [markdown]
# ## 3. Import Mesh and Model Data
# Read nodes, elements, and part definitions from the input file.

# %%
fns = []
path = examples.thermal_stress + os.sep
fns.append(path + "thermal_stress.k")
solution.open_files(fns)

# %% [markdown]
# ## 4. Set Simulation Termination Time
# Set the simulation termination time for the analysis.

# %%
solution.set_termination(3.0)

# %% [markdown]
# ## 5. Configure Thermal and Mechanical Analysis
# Set up the transient thermal solver and explicit mechanical analysis.

# %%
ts = DynaMech(analysis=AnalysisType.EXPLICIT)
solution.add(ts)

tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=0.1)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
ts.add(tanalysis)

ts.set_timestep(timestep_size_for_mass_scaled=0.01)

# %% [markdown]
# ## 6. Define Material and Section Properties
# Assign a temperature-dependent elastic-plastic material and thermal isotropic properties to the part.

# %%
mat = MatElasticPlasticThermal(
    mass_density=1.0,
    temperatures=(0, 10, 20, 30, 40, 50),
    young_modulus=(1e10, 1e10, 1e10, 1e10, 1e10, 1e10),
    poisson_ratio=(0.3, 0.3, 0.3, 0.3, 0.3, 0.3),
    thermal_expansion=(0, 2e-6, 4e-6, 6e-6, 8e-6, 1e-5),
    yield_stress=(1e20, 1e20, 1e20, 1e20, 1e20, 1e20),
)
mat.set_thermal_isotropic(density=1, generation_rate_multiplier=10, specific_heat=1, conductivity=1)

slab = SolidPart(1)
slab.set_material(mat)
slab.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
ts.parts.add(slab)

# %% [markdown]
# ## 7. Set Initial Conditions
# Initialize nodal temperatures for the analysis.

# %%
for i in range(1, 9):
    ts.initialconditions.create_temperature(NodeSet([i]), temperature=10)

# %% [markdown]
# ## 8. Define Output Requests and Save Input File
# Configure output frequencies and save the input deck to disk.

# %%
solution.set_output_database(glstat=0.03)
solution.create_database_binary(dt=0.01)
solution.save_file()

# %% [markdown]
# ## 9. Conclusion
# In this example, we demonstrated how to set up a thermal stress simulation with temperature-dependent material properties using LS-DYNA and PyDyna. The workflow included model import, material and section definition, solver setup, initial condition assignment, and output configuration. This approach provides a clear, modular, and scriptable workflow for advanced thermal-mechanical simulations.
