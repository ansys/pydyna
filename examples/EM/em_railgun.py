"""
Railgun example
===============

This example demonstrates how to create an EM Railgun input deck. \n
LS-DYNA version : ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe
"""

import os
import sys

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    Circuit,
    CircuitType,
    SegmentSet,
    NodeSet,
    Curve,
    SolidPart,
    SolidFormulation,
    EMContact,
    BEMSOLVER,
    FEMSOLVER,
)
from ansys.dyna.core.pre.dynamaterial import MatElastic, MatRigid, EMMATTYPE
from em_railgun_data import *
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/em/railgun.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

em_solution = DynaSolution(hostname)
fns = []
path = examples.em_railgun + os.sep
fns.append(path + "em_railgun.k")
em_solution.open_files(fns)
em_solution.set_termination(termination_time=3e-4)
em_solution.create_database_binary(dt=5e-6)

railgun = DynaEM()
em_solution.add(railgun)

railgun.analysis.set_timestep(timestep=5e-6)
railgun.analysis.set_solver_bem(solver=BEMSOLVER.PCG)
railgun.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

circuit = Circuit(
    circuit_type=CircuitType.IMPOSED_CURRENT_VS_TIME,
    loadcurve=Curve(x=[0, 8e-5, 2e-4, 4e-4, 6e-4, 1e-3], y=[0, 350, 450, 310, 230, 125], sfo=2e6),
)
circuit.set_current(
    current=SegmentSet(cur), current_inlet=SegmentSet(inlet), current_outlet=SegmentSet(outlet)
)
railgun.add(circuit)

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

railgun.boundaryconditions.create_spc(NodeSet(spc1), tx=False, ty=False, rz=False, death=0)
railgun.boundaryconditions.create_spc(NodeSet(spc2), tx=False, ty=False, rz=False, death=0)

contact = EMContact()
railgun.contacts.add(contact)

railgun.set_rogowsky_coil_to_output_current(SegmentSet(cur))

em_solution.save_file()
