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
# # RLC Circuit Simulation Using Isopotential Boundary in LS-DYNA Python API
#
# This example demonstrates how to use the `Isopotential_ConnType.RLC_CIRCUIT` connection type in the
# `connect_isopotential()` method to define an RLC circuit as an inlet boundary condition in LS-DYNA.
# The following notebook is organized into step-by-step, practical sections to help you understand and
# implement RLC circuit modeling with isopotential connections in LS-DYNA using the Python API.
#
# ---

# %% [markdown]
# ### 1. Imports and Data Setup
# In this section, we import all necessary Python modules and LS-DYNA Python API classes. These imports provide
# access to the simulation framework, material models, and utility functions required for setting up and running
# the RLC circuit example. The `em_set_data` import provides the geometry and segment data for the Rogowski coil
# used in the simulation.
# %%
import os
import sys

from em_set_data import rlc_rogoseg

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    EMType,
    Isopotential,
    Isopotential_ConnType,
    NodeSet,
    RogoCoil,
    SegmentSet,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatRigid
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/em/rlc_isopotential.png'

# %% [markdown]
# ### 2. LS-DYNA Executable and File Paths
# Here, we configure the LS-DYNA server connection and specify the input files for the simulation. The hostname
# can be set to a remote server or left as 'localhost' for local execution. The input key file contains the finite
# element model and boundary conditions for the RLC circuit example.
# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_rlc_isopotential + os.sep
fns.append(path + "em_rlc_isopotential.k")
solution.open_files(fns)
solution.set_termination(termination_time=0.01)
solution.create_database_binary(dt=1e-4)

# %% [markdown]
# ### 3. Electromagnetic (EM) Model Setup
# This section creates the electromagnetic model object and configures the solver for resistive heating analysis.
# The time step and solver type are set to ensure numerical stability and accurate results for the coupled RLC
# circuit and electromagnetic field simulation.
# %%
emobj = DynaEM()
solution.add(emobj)
emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)
emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

# %% [markdown]
# ### 4. Material Definitions
# In this step, we define the material properties for the solid part of the model. A rigid material is used with
# specified density and elastic properties. The electromagnetic permeability and conductivity are set to represent
# a conductor, which is essential for simulating current flow in the RLC circuit.
# %%
matrigid = MatRigid(
    mass_density=7000,
    young_modulus=2e11,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
matrigid.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

# %% [markdown]
# ### 5. Part Definition and Assignment
# The solid part representing the simulation domain is created and assigned the previously defined material. The
# element formulation is set to constant stress solid, which is suitable for electromagnetic and structural
# analyses involving rigid conductors.
# %%
part1 = SolidPart(1)
part1.set_material(matrigid)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

# %% [markdown]
# ### 6. Node Sets and Isopotential Connections
# Node sets are defined to specify the locations where isopotential boundary conditions will be applied. These
# node sets represent the terminals of the RLC circuit. Isopotential objects are created from these node sets to
# facilitate the application of circuit boundary conditions in the electromagnetic simulation.
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
# ### 7. RLC Circuit Isopotential Connection
# The RLC circuit is defined by connecting the first isopotential node set using the RLC_CIRCUIT connection type.
# The resistance, inductance, capacitance, and initial voltage are specified to model the circuit's electrical
# behavior. The second isopotential node set is connected as a voltage source. The Rogowski coil is added to the
# model to measure current.
# %%
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RLC_CIRCUIT,
    isopotential1=isopos_conn1,
    value=5e-4,
    inductance=7.8e-5,
    capacity=0.0363,
    initial_voltage=5000,
)
emobj.connect_isopotential(contype=Isopotential_ConnType.VOLTAGE_SOURCE, isopotential1=isopos_conn2)
emobj.add(RogoCoil(SegmentSet(rlc_rogoseg)))

# %% [markdown]
# ### 8. Output Requests and Save
# Finally, output requests are configured to record electromagnetic and circuit results during the simulation.
# The model setup is saved to file, completing the preparation for running the LS-DYNA simulation.
# %%
emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)
solution.save_file()

# %% [markdown]
# ### 9. Conclusion
# In this example, we demonstrated how to set up and simulate an RLC circuit using isopotential boundary
# conditions in LS-DYNA with the Python API. By defining node sets, material properties, and applying the
# RLC_CIRCUIT connection type, we modeled the dynamic electrical behavior of the circuit within a
# multiphysics simulation environment. This workflow enables users to couple electromagnetic and circuit
# analyses, providing valuable insights into current flow, voltage response, and the interaction between
# electrical and structural domains. The approach shown here can be extended to more complex circuits and
# geometries, supporting advanced engineering and research applications in electromagnetics.
