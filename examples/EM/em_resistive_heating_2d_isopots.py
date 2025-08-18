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
# # Resistive Heating 2D Isopotential Connections with LS-DYNA Python API
#
# This notebook demonstrates a 2D resistive heating simulation using LS-DYNA Python API, focusing on the use of
# `connect_isopotential()` to model electrical connections between regions.
#
# ## Background: Isopotential Connections in 2D Resistive Heating
#
# In this example, a metal bar is modeled with voltage sources and short circuits applied to various node sets,
# simulating current flow and resistive heating. The LS-DYNA EM solver couples electromagnetic and thermal effects,
# allowing for accurate prediction of temperature and current distribution. Rogowski coils are included for current
# measurement.
#
# The setup includes model definition, material assignment, boundary conditions, electrical connections, coil definition,
# and output requests.

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules and set up initial data for the simulation. This includes LS-DYNA Python API classes for
# electromagnetic and thermal analysis, and utility functions for file paths and IP checking.
#
# - Both electromagnetic and thermal modules are needed to capture the coupled physics.
# - Utility functions help manage file paths and remote/local solver execution.
# - Custom coil geometry is imported for Rogowski coil definition.
# %%
import os
import sys

from em_set_data import rogoseg

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    FEMSOLVER,
    Curve,
    DynaEM,
    EMDimension,
    EMType,
    Isopotential,
    Isopotential_ConnType,
    NodeSet,
    PartSet,
    RogoCoil,
    SegmentSet,
    ShellFormulation,
    ShellPart,
    ThermalAnalysis,
    ThermalAnalysisType,
)
from ansys.dyna.core.pre.dynamaterial import (
    EMMATTYPE,
    MatRigid,
    MatThermalIsotropic,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Set the LS-DYNA server hostname and input file path. This prepares the simulation environment for local or remote execution.
#
# - The LS-DYNA solver runs as a server process, and the Python API connects to it.
# - Input files contain geometry, mesh, and initial setup for the model.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_resistive_heating_2d_isopots + os.sep
fns.append(path + "em_resistive_heating_2d_isopots.k")
solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Time and Output Database
# Set the total simulation time and output interval. This determines how long the simulation runs and how frequently results are written for analysis.
#
# - Simulation time should be long enough to capture heating and diffusion effects.
# - Output intervals balance file size and temporal resolution.
# %%
solution.set_termination(termination_time=0.0101)
solution.create_database_binary(dt=1e-4)

# %% [markdown]
# ### 4. Electromagnetic (EM) Model Setup
# Create and configure the electromagnetic model. Set solver parameters for accurate and stable simulation of resistive heating.
#
# - The EM solver calculates electric fields and currents, which drive resistive heating.
# - Solver parameters affect accuracy and stability.
# %%
emobj = DynaEM()
solution.add(emobj)
emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)
emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.PLANAR_2D)
emobj.analysis.set_solver_fem(
    solver=FEMSOLVER.DIRECT_SOLVER,
    relative_tol=1e-3,
)

# %% [markdown]
# ### 5. Thermal Analysis Setup
# Add a transient thermal analysis to the EM model, enabling simulation of temperature changes in the metal bar due to resistive heating effects.
#
# - Thermal analysis solves the heat equation, predicting temperature evolution.
# - Transient analysis captures time-dependent effects.
# %%
tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=1e-4)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
emobj.add(tanalysis)

# %% [markdown]
# ### 6. Material Definitions
# Define and assign rigid and thermal isotropic materials to the shell part. These properties control the electrical conductivity and thermal response of the simulated metal bar.
#
# - Material properties (conductivity, density, specific heat) determine heating and diffusion.
# - Different materials can represent conductors, insulators, or composites.
# %%
matrigid = MatRigid(mass_density=1, young_modulus=2e11)
matrigid.set_em_resistive_heating_2d(
    material_type=EMMATTYPE.CONDUCTOR,
    initial_conductivity=1e4,
)
matthermaliso = MatThermalIsotropic(
    density=100,
    specific_heat=10,
    conductivity=7,
)

# %% [markdown]
# ### 7. Part Definition and Assignment
# Create the shell part representing the metal bar, assign the defined materials, and set the element formulation for 2D analysis.
#
# - The domain may consist of multiple regions with different materials and properties.
# - Element formulation controls how the mesh represents the physics.
# %%
part = ShellPart(1)
part.set_material(matrigid, matthermaliso)
part.set_element_formulation(ShellFormulation.PLANE_STRESS)
emobj.parts.add(part)

# %% [markdown]
# ### 8. Boundary Conditions and Initial Conditions
# Apply imposed motion and initial temperature to the part. These conditions define how the bar interacts with its environment and set the starting temperature for the simulation.
#
# - Boundary conditions define interaction with the environment (motion, temperature, etc).
# - Initial conditions set the starting temperature, affecting transient response.
# %%
emobj.boundaryconditions.create_imposed_motion(
    PartSet([1]),
    Curve(x=[0, 10], y=[10, 10]),
)
emobj.set_init_temperature(temp=25)

# %% [markdown]
# ### 9. Isopotential Connections (Electrical Boundary Conditions)
# Apply voltage sources and short circuits to node sets. This establishes the electrical boundary conditions required for simulating resistive heating and current flow.
#
# - Isopotential connections allow for flexible modeling of electrical networks and boundary conditions.
# - Voltage sources and short circuits establish the potential difference and conductive paths needed for current flow and heating.
# %%
emobj.connect_isopotential(
    contype=Isopotential_ConnType.VOLTAGE_SOURCE,
    isopotential1=Isopotential(NodeSet([521, 517, 513, 509, 525])),
    value=500,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.SHORT_CIRCUIT,
    isopotential1=Isopotential(NodeSet([642, 652, 661, 670, 643])),
    isopotential2=Isopotential(NodeSet([549, 548, 577, 597, 617])),
    value=0.01,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.VOLTAGE_SOURCE,
    isopotential1=Isopotential(NodeSet([653, 644, 626, 627, 662])),
)

# %% [markdown]
# ### 10. Coil Definition
# Add a Rogowski coil to the model for current measurement or induction, important for analyzing electromagnetic effects in the system.
# %%
emobj.add(RogoCoil(SegmentSet(rogoseg)))

# %% [markdown]
# ### 11. Output Requests and File Saving
# Configure output requests for materials and solutions, enabling post-processing and visualization. Save the model setup for LS-DYNA execution.
#
# - Output requests specify which results are saved for later analysis, such as temperature fields, electric currents, and material responses.
# - Saving the model setup allows for reproducibility and further study.
# %%
emobj.create_em_output(
    mats=2,
    matf=2,
    sols=2,
    solf=2,
)
solution.save_file()

# %% [markdown]
# ### 12. Conclusion
# This example demonstrated how to set up and simulate a 2D resistive heating problem with isopotential
# connections using the LS-DYNA Python API. By applying voltage sources and short circuits to node sets,
# we modeled current flow and resistive heating in a metal bar, capturing the coupled electromagnetic and
# thermal response. The workflow highlights the flexibility of the Python API for defining custom electrical
# boundary conditions and supports advanced analysis of multiphysics problems in engineering and research.
