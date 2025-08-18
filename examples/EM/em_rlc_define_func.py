# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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
# # RLC Circuit Simulation with LS-DYNA Python API
#
# This example demonstrates the use of a custom Python function to model an RLC circuit as an inlet boundary
# condition in LS-DYNA. It highlights how to integrate user-defined logic for advanced electromagnetic simulation
# workflows, with clear explanations for each step.

# %% [markdown]
# ### 1. Imports and Data Setup
# Import required modules and LS-DYNA Python API classes. This step ensures all necessary libraries and data are
# available for the simulation.
# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    EMType,
    Function,
    Isopotential,
    Isopotential_ConnType,
    NodeSet,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatRigid
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/em/rlc_isopotential.png'

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Set up the LS-DYNA server hostname and input file paths. This prepares the solver for launching and loads the model
# for simulation.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_rlc_define_func + os.sep
fns.append(path + "em_rlc_define_func.k")
solution.open_files(fns)
solution.set_termination(termination_time=0.01)
solution.create_database_binary(dt=1e-4)

# %% [markdown]
# ### 3. Electromagnetic (EM) Model Setup
# Create and configure the electromagnetic model. Set solver parameters for accurate and stable simulation of
# resistive heating.
# %%
emobj = DynaEM()
solution.add(emobj)
emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)
emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

# %% [markdown]
# ### 4. Material Definitions
# Define and assign rigid material properties to the solid part. These properties control the electrical and
# mechanical response of the simulated domain.
# %%
matrigid = MatRigid(
    mass_density=7000,
    young_modulus=2e11,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
matrigid.set_em_permeability_equal(
    material_type=EMMATTYPE.CONDUCTOR,
    initial_conductivity=1e4,
)

# %% [markdown]
# ### 5. Part Definition and Assignment
# Create the solid part representing the domain and assign the defined material.
# %%
part1 = SolidPart(1)
part1.set_material(matrigid)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

# %% [markdown]
# ### 6. Node Sets and Isopotential Connections
# Define node sets and isopotential connections for the RLC circuit boundary conditions.
# %%
nset1 = NodeSet(
    [
        429,
        433,
        437,
        441,
        445,
        449,
        453,
        457,
        461,
        465,
        469,
        473,
        477,
        481,
        485,
        489,
        493,
        497,
        501,
        505,
        509,
        513,
        517,
        521,
        525,
    ]
)
nset2 = NodeSet(
    [
        26,
        31,
        36,
        41,
        46,
        51,
        56,
        61,
        66,
        71,
        76,
        81,
        86,
        91,
        96,
        101,
        106,
        111,
        116,
        121,
        126,
        131,
        136,
        141,
        146,
    ]
)
isopos_conn1 = Isopotential(nset1)
isopos_conn2 = Isopotential(nset2)

# %% [markdown]
# ### 7. RLC Circuit Function Definition
# Define the RLC circuit function for the voltage source boundary condition.
# %%
fn = (
    "float rlc(float time,float emdt,float curr,float curr1,"
    " float curr2,float pot1,float pot2, float rmesh)"
    " {"
    " float fac,R,C,Vc,L,xi ;"
    " R = 0.5e-3; L = 78.e-6;"
    " fac =1.e-6; C=363.e-4;"
    " float q= 181.5;"
    " if(time<emdt) return fac;"
    " q=q+emdt*curr;"
    " Vc=q/C;"
    " xi=(Vc*emdt-L*curr)/((R+rmesh)*emdt+L);"
    " return xi*rmesh;"
    " }"
)

# %% [markdown]
# ### 8. Apply Isopotential Connections and Output Requests
# Apply the voltage source boundary conditions using the RLC function and configure output requests for the simulation.
# %%
emobj.connect_isopotential(
    contype=Isopotential_ConnType.VOLTAGE_SOURCE,
    isopotential1=isopos_conn1,
    func=Function(fn),
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.VOLTAGE_SOURCE,
    isopotential1=isopos_conn2,
)
emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)
solution.save_file()

# %% [markdown]
# ### 9. Conclusion
# In this example, we demonstrated how to use a custom Python function to model an RLC circuit as an inlet
# boundary condition in LS-DYNA using the Python API. By integrating user-defined logic, we enabled advanced
# electromagnetic simulation workflows that go beyond standard boundary conditions. This approach allows for
# flexible and precise modeling of circuit behavior, supporting research and engineering applications that
# require custom circuit dynamics. The workflow shown here can be adapted to more complex circuits and
# simulation scenarios, providing a foundation for multiphysics analysis and innovation.
