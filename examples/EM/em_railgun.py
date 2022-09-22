"""
Railgun example
=====================

This example demonstrates how to create an EM Railgun input deck.
"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynaem import *
from ansys.dyna.pre.dynamaterial import *
from em_railgun_data import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]

    em_solution = DynaSolution(hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "em_railgun" + os.sep
    fns.append(path + "em_railgun.k")
    em_solution.open_files(fns)
    em_solution.set_termination(termination_time=3e-4)
    em_solution.create_database_binary(dt=5e-6)

    railgun = DynaEM()    
    em_solution.add(railgun)

    analysis = EMAnalysis()
    analysis.set_timestep(timestep=5e-6)
    analysis.set_solver_bem(solver=BEMSOLVER.PCG)
    analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER,relative_tol=1e-3)
   
    circuit = Circuit(circuit_type=CircuitType.IMPOSED_CURRENT_VS_TIME,loadcurve=Curve(x=[0,8e-5,2e-4,4e-4,6e-4,1e-3],y=[0,350,450,310,230,125],sfo=2e6))
    circuit.set_current(current=SegmentSet(cur),current_inlet=SegmentSet(inlet),current_outlet=SegmentSet(outlet))

    matelastic = MatElastic(mass_density=2.64e-3,young_modulus=9.7e+10,poisson_ratio=0.31)
    matelastic.set_electromagnetic_property(material_type=EMMATTYPE.CONDUCTOR,initial_conductivity=25)
    matrigid = MatRigid(mass_density=2.64e-3,young_modulus=9.7e+10,poisson_ratio=0.31,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    matrigid.set_electromagnetic_property(material_type=EMMATTYPE.CONDUCTOR,initial_conductivity=25)
    
    coil = SolidPart(1)
    coil.set_material(matelastic)
    coil.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)

    workpiece1 = SolidPart(2)
    workpiece1.set_material(matrigid)
    workpiece1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)

    workpiece2 = SolidPart(3)
    workpiece2.set_material(matrigid)
    workpiece2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    
    bdy = BoundaryCondition()
    bdy.create_spc(NodeSet(spc1),tx=False,ty=False,rz=False,death=0)
    bdy.create_spc(NodeSet(spc2),tx=False,ty=False,rz=False,death=0)

    contact = EMContact()

    railgun.set_rogowsky_coil_to_output_current(SegmentSet(cur))
    
    em_solution.save_file()
