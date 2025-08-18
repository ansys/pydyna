# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
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
# # EM Resistive Heating Simulation with LS-DYNA Python API
#
# This notebook demonstrates the minimum setup required to enable the
# electromagnetic (EM) resistive heating solver in LS-DYNA using the Python API.
#  Each section provides both code and theoretical context for
# the simulation steps.

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules, LS-DYNA Python API classes, and custom data for
# temperature boundary conditions. This step ensures all necessary libraries and
# data are available for the simulation.
# %%
import os
import sys

from em_set_data import resistive_heating_tmp

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    FEMSOLVER,
    Curve,
    DynaEM,
    EMType,
    NodeSet,
    SolidFormulation,
    SolidPart,
    ThermalAnalysis,
    ThermalAnalysisType,
)
from ansys.dyna.core.pre.dynamaterial import (
    EMMATTYPE,
    EMEOSTabulated1,
    MatElastic,
    MatThermalIsotropic,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/em/em_resistive_heating.png'

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Set up the LS-DYNA server hostname and input file paths. This step prepares the
# solver for launching and loads the model for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_resistive_heating + os.sep
fns.append(path + "em_resistive_heating.k")
solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Time and Output Database
# Configure the total simulation time and the interval for output data. This
# determines how long the simulation runs and how frequently results are saved
# for analysis.
# %%
solution.set_termination(termination_time=20)
solution.create_database_binary(dt=0.1)

# %% [markdown]
# ### 4. Electromagnetic (EM) Model Setup
# Create and configure the electromagnetic model. This includes setting the
# solver type, analysis dimension, and numerical parameters for the EM
# simulation.
# %%
emobj = DynaEM()
solution.add(emobj)
emobj.set_timestep(
    timestep_size_for_mass_scaled=0.01,
    max_timestep=Curve(x=[0, 9.9999997474e-5], y=[0.01, 0.01]),
)
emobj.analysis.set_timestep(timestep=0.01)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)
emobj.analysis.set_solver_fem(
    solver=FEMSOLVER.DIRECT_SOLVER,
    relative_tol=1e-3,
)

# %% [markdown]
# ### 5. Thermal Analysis Setup
# Add and configure a transient thermal analysis to the EM model. This enables
# simulation of temperature changes in the domain due to resistive heating.
# %%
tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=0.05)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
emobj.add(tanalysis)

# %% [markdown]
# ### 6. Material Definitions
# Define elastic and thermal isotropic materials with electrical and thermal
# properties. These materials are assigned to the solid parts to model their
# physical behavior in the simulation.
# %%
matelastic1 = MatElastic(
    mass_density=8000,
    young_modulus=1e11,
    poisson_ratio=0.33,
)
matelastic2 = MatElastic(
    mass_density=7000,
    young_modulus=1e11,
    poisson_ratio=0.33,
)
matelastic1.set_em_permeability_equal(
    material_type=EMMATTYPE.CONDUCTOR,
    initial_conductivity=6e7,
)
matelastic2.set_em_permeability_equal(
    material_type=EMMATTYPE.CONDUCTOR,
    initial_conductivity=4e6,
    eos=EMEOSTabulated1(
        Curve(x=[0, 25, 50, 100], y=[4e6, 4e6, 4e5, 4e5]),
    ),
)
matthermaliso1 = MatThermalIsotropic(
    density=8000,
    specific_heat=400,
    conductivity=400,
)
matthermaliso2 = MatThermalIsotropic(
    density=7000,
    specific_heat=450,
    conductivity=40,
)

# %% [markdown]
# ### 7. Part Definition and Assignment
# Create solid parts to represent different regions in the simulation domain.
# Assign material properties and set the element formulation for each part. This
# allows for simulation of interfaces and inclusions.
# %%
part2 = SolidPart(2)
part2.set_material(matelastic1, matthermaliso1)
part2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part2)

part3 = SolidPart(3)
part3.set_material(matelastic1, matthermaliso1)
part3.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part3)

part1 = SolidPart(1)
part1.set_material(matelastic2, matthermaliso2)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

# %% [markdown]
# ### 8. Boundary Conditions and Initial Conditions
# Apply temperature boundary conditions to node sets and set the initial
# temperature for the domain. These steps are crucial for realistic thermal
# analysis and simulation accuracy.
# %%
emobj.boundaryconditions.create_temperature(
    NodeSet(resistive_heating_tmp),
    scalefactor=50,
)
emobj.set_init_temperature(temp=25)

# %% [markdown]
# ### 9. Output Requests and File Saving
# Configure output requests for electromagnetic and thermal results. Save the
# model setup for LS-DYNA execution. This step enables post-processing,
# visualization, and analysis of the simulation results.
# %%
emobj.create_em_output(
    mats=2,
    matf=2,
    sols=2,
    solf=2,
)
solution.save_file()

# %% [markdown]
# ### 10. Conclusion
# This example demonstrated the setup and execution of an electromagnetic resistive heating simulation in
# LS-DYNA using the Python API. By defining materials, parts, boundary conditions, and solver parameters,
# we modeled the coupled thermal and electromagnetic response of a domain subject to resistive heating.
# This workflow enables users to analyze temperature rise, heat distribution, and material behavior under
# electrical loading. The approach can be extended to more complex geometries, materials, and multiphysics
# scenarios, supporting advanced engineering analysis and design.
