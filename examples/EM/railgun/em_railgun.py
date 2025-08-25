# %% [markdown]
# # EM Railgun Simulation Setup with LS-DYNA and PyDyna
#
# This notebook demonstrates how to create and set up an electromagnetic (EM) railgun simulation using
# LS-DYNA and PyDyna. The workflow includes geometry import, material definition, electromagnetic circuit
# setup, boundary conditions, contact definitions, and solver configuration. Each step is explained for
# clarity and educational use.
#
# ## Theory and Background
#
# Railguns are electromagnetic launchers that use Lorentz forces to accelerate projectiles to extremely
# high velocities. Current flows through parallel rails and a sliding conductor (armature), creating a
# magnetic field. The interaction between current and magnetic field generates forces that propel the
# projectile. This simulation requires coupled electromagnetic-structural analysis to capture the complex
# physics including Joule heating, material deformation, and dynamic contact between components.
#
# LS-DYNA's EM solver handles both BEM (Boundary Element Method) and FEM (Finite Element Method) approaches
# for electromagnetic field calculations, coupled with structural dynamics for comprehensive railgun analysis.
#
# **LS-DYNA version compatibility**: ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe

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
# ## 1. Import Required Modules and Data
# Import necessary modules for EM simulation setup and node/segment data from the railgun geometry.

# %%
import os
import sys

from em_railgun_data import cur, inlet, outlet, spc1, spc2

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    BEMSOLVER,
    FEMSOLVER,
    Circuit,
    CircuitType,
    Curve,
    DynaEM,
    EMContact,
    NodeSet,
    SegmentSet,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatElastic, MatRigid

# sphinx_gallery_thumbnail_path = '_static/pre/em/railgun.png'

# %% [markdown]
# ## 2. Initialize Solution and Load Geometry
# Connect to the PyDyna pre-service, load the railgun geometry, and set up basic simulation parameters.

# %%

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_railgun + os.sep
fns.append(path + "em_railgun.k")
solution.open_files(fns)
solution.set_termination(termination_time=3e-4)
solution.create_database_binary(dt=5e-6)

railgun = DynaEM()
solution.add(railgun)

# %% [markdown]
# ## 3. Configure EM Solver Settings
# Set up the electromagnetic analysis parameters including time step, BEM solver, and FEM solver
# configurations for optimal performance and accuracy.

# %%

railgun.analysis.set_timestep(timestep=5e-6)
railgun.analysis.set_solver_bem(solver=BEMSOLVER.PCG)
railgun.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

# %% [markdown]
# ## 4. Define Electromagnetic Circuit
# Create the circuit definition with imposed current vs time, including current path through segments
# and inlet/outlet definitions for the railgun current flow.

# %%

circuit = Circuit(
    circuit_type=CircuitType.IMPOSED_CURRENT_VS_TIME,
    loadcurve=Curve(x=[0, 8e-5, 2e-4, 4e-4, 6e-4, 1e-3], y=[0, 350, 450, 310, 230, 125], sfo=2e6),
)
circuit.set_current(current=SegmentSet(cur), current_inlet=SegmentSet(inlet), current_outlet=SegmentSet(outlet))
railgun.add(circuit)

# %% [markdown]
# ## 5. Define Materials with EM Properties
# Create elastic and rigid materials with electromagnetic properties for the railgun components
# (conductors with specified conductivity values).

# %%

matelastic = MatElastic(mass_density=2.64e-3, young_modulus=9.7e10, poisson_ratio=0.31)
matelastic.set_electromagnetic_property(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=25)

matrigid = MatRigid(
    mass_density=2.64e-3,
    young_modulus=9.7e10,
    poisson_ratio=0.31,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
matrigid.set_electromagnetic_property(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=25)

# %% [markdown]
# ## 6. Create Part Definitions
# Define the railgun parts including coil and workpieces with appropriate materials and element
# formulations for the electromagnetic-structural coupling.

# %%

coil = SolidPart(1)
coil.set_material(matelastic)
coil.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
railgun.parts.add(coil)

workpiece1 = SolidPart(2)
workpiece1.set_material(matrigid)
workpiece1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
railgun.parts.add(workpiece1)

workpiece2 = SolidPart(3)
workpiece2.set_material(matrigid)
workpiece2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
railgun.parts.add(workpiece2)

# %% [markdown]
# ## 7. Apply Boundary Conditions
# Set up single point constraints (SPC) to restrict motion of specific nodes in the railgun geometry
# for proper constraint of the system.

# %%

railgun.boundaryconditions.create_spc(NodeSet(spc1), tx=False, ty=False, rz=False, death=0)
railgun.boundaryconditions.create_spc(NodeSet(spc2), tx=False, ty=False, rz=False, death=0)

# %% [markdown]
# ## 8. Configure Contact and Output Settings
# Set up electromagnetic contact definitions, Rogowski coil for current measurement, and output
# databases for monitoring energy and field results.

# %%

contact = EMContact()
railgun.contacts.add(contact)

railgun.set_rogowsky_coil_to_output_current(SegmentSet(cur))
railgun.create_em_database_globalenergy(outlv=1)

railgun.create_em_output(mats=2, matf=2, sols=2, solf=2)

# %% [markdown]
# ## 9. Save the Input File
# Save the complete railgun simulation setup to the input file for LS-DYNA execution.

# %%
solution.save_file()

# %% [markdown]
# ## 10. Conclusion
# This notebook demonstrated the complete setup of an electromagnetic railgun simulation using LS-DYNA
# and PyDyna. The workflow included geometry loading, material definition with EM properties, circuit
# setup with time-dependent current, boundary conditions, contact definitions, and output configuration.
# This approach provides a comprehensive framework for analyzing electromagnetic launchers with coupled
# field-structure interactions in advanced engineering applications.
