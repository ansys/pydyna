# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
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
# # Resistive Heating 2D Multiple-Connect Isopotential
#
# This example shows how to define complex circuits as inlet boundary conditions.
# The executable file for LS-DYNA is
# ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.
#
# The model demonstrates a 2D resistive heating simulation with multiple isopotential connections
# using the LS-DYNA Python API.Each section provides both code and physical context for the simulation
# steps.
#
# ## Multi-Isopotential Resistive Heating Background
#
# In electromagnetic simulations, isopotentials are used to define electrical connections between
# different regions or node sets. By combining current sources and resistive connections, complex
# circuits can be modeled as boundary conditions. This is essential for simulating real-world
# electrical networks, such as coils, busbars, and multi-terminal devices.
#
# The LS-DYNA EM solver can simulate the coupled interaction between electromagnetic fields and
# thermal effects, allowing for accurate prediction of temperature distribution and current flow.
#
# This example demonstrates how to set up such a simulation using the Python API, including model
# definition, material assignment, boundary conditions, isopotential connections, and output requests.

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules, LS-DYNA Python API classes, and custom data for coil geometry and simulation
# parameters. This step ensures all necessary libraries and data are available for the simulation.
#
# - Both electromagnetic and thermal modules are needed to capture the coupled physics.
# - Custom data (such as coil geometry) defines the model's environment and initial state.
# %%
import os
import sys

from em_set_data import rogoseg

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
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
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatRigid, MatThermalIsotropic
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Define the location of the LS-DYNA executable and input files. This is necessary to launch the solver
# and load the model for simulation.
#
# - The LS-DYNA solver runs as a server process, and the Python API connects to it.
# - Input files contain geometry, mesh, and initial setup for the model.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_resistive_heating_2d_multi_isopots + os.sep
fns.append(path + "em_resistive_heating_2d_multi_isopots.k")
solution.open_files(fns)

# %% [markdown]
# ### 3. Simulation Time and Output Database
# Set the total simulation time and output interval. This determines how long the simulation runs and
# how frequently results are written for analysis.
#
# - Simulation time should be long enough to capture heating and diffusion effects.
# - Output intervals balance file size and temporal resolution.
# %%
solution.set_termination(termination_time=0.0101)
solution.create_database_binary(dt=1e-4)

# %% [markdown]
# ### 4. Electromagnetic (EM) Model Setup
# Create and configure the electromagnetic model. Set solver parameters for accurate and stable
# simulation of resistive heating.
#
# - The EM solver calculates electric fields and currents, which drive resistive heating.
# - Solver parameters affect accuracy and stability.
# %%
emobj = DynaEM()
solution.add(emobj)
emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)
emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.PLANAR_2D)

# %% [markdown]
# ### 5. Thermal Analysis Setup
# Add a transient thermal analysis to the EM model, enabling simulation of temperature changes in the
# metal bar due to resistive heating effects.
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
# Define and assign rigid and thermal isotropic materials to the shell part. These properties control
# the electrical conductivity and thermal response of the simulated metal bar.
#
# - Material properties (conductivity, density, specific heat) determine heating and diffusion.
# - Different materials can represent conductors, insulators, or composites.
# %%
matrigid = MatRigid(mass_density=1, young_modulus=2e11)
matrigid.set_em_resistive_heating_2d(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)
matthermaliso = MatThermalIsotropic(density=100, specific_heat=10, conductivity=7)

# %% [markdown]
# ### 7. Part Definition and Assignment
# Create the shell part representing the metal bar, assign the defined materials, and set the element
# formulation for 2D analysis.
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
# Apply imposed motion and initial temperature to the part. These conditions define how the bar
# interacts with its environment and set the starting temperature for the simulation.
#
# - Boundary conditions define interaction with the environment (motion, temperature, etc).
# - Initial conditions set the starting temperature, affecting transient response.
# %%
emobj.boundaryconditions.create_imposed_motion(PartSet([1]), Curve(x=[0, 10], y=[10, 10]))
emobj.set_init_temperature(temp=25)

# %% [markdown]
# ### 9. Isopotential Connections (Complex Electrical Boundary Conditions)
# Isopotentials are used to define electrical connections between nodes. In this example, multiple types
# of connections are used to model a complex circuit:
# - CURRENT_SOURCE: Applies a time-dependent current between node sets, simulating a current supply.
# - RESISTANCE: Connects node sets with a specified resistance, simulating resistive paths in the circuit.
# These are essential for modeling current flow and resistive heating in complex electrical networks.
#
# - Isopotential connections allow for flexible modeling of electrical networks and boundary conditions.
# - Voltage sources establish the potential difference needed for current flow and heating.
# %%
crv = Curve(func="-5./0.01*EXP(-TIME/((5.e-4+0.05+0.01)*0.04))")
nset1 = NodeSet([521, 517, 513, 509, 525])
nset2 = NodeSet([549, 548, 577, 597, 617])
nset3 = NodeSet([653, 644, 626, 627, 662])
nset4 = NodeSet([642, 652, 661, 670, 643])
emobj.connect_isopotential(
    contype=Isopotential_ConnType.CURRENT_SOURCE,
    isopotential1=Isopotential(nset4),
    isopotential2=Isopotential(nset2),
    curve=crv,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RESISTANCE,
    isopotential1=Isopotential(nset4),
    isopotential2=Isopotential(nset2),
    value=0.01,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RESISTANCE,
    isopotential1=Isopotential(nset3),
    isopotential2=Isopotential(nset1),
    value=0.05,
)

# %% [markdown]
# ### 10. Coil Definition
# Add a Rogowski coil to the model. This coil is used for current measurement or induction, which is
# important for analyzing electromagnetic effects in the system.
#
# - Rogowski coils are used to measure current in conductors and analyze induced electromagnetic effects.
# %%
emobj.add(RogoCoil(SegmentSet(rogoseg)))

# %% [markdown]
# ### 11. Output Requests and File Saving
# Specify which results to output from the simulation, such as material and solution data. This enables
# post-processing and analysis of the simulation results.
#
# - Output requests specify which results are saved for later analysis, such as temperature fields,
# electric currents, and material responses.
# Saving the model setup allows for reproducibility and further study.
# %%
emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)
solution.save_file()

# %% [markdown]
# ### 12. Conclusion
# This example illustrated how to set up and simulate a 2D resistive heating problem with multiple
# isopotential connections using the LS-DYNA Python API. By combining current sources and resistive
# connections, we modeled a complex electrical network as boundary conditions, enabling the simulation
# of realistic circuits and their thermal effects. The workflow demonstrated the flexibility of the
# Python API for defining custom electrical and thermal scenarios, supporting advanced analysis of
# coupled multiphysics problems in engineering and research.
