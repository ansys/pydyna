"""
Resistive heating
=================

This example introduces the user to the minimum number of keywords required to turn on the EM resistive heating solver. \n
LS-DYNA version : ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe
"""

import os
import sys

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    NodeSet,
    Curve,
    SolidPart,
    SolidFormulation,
    FEMSOLVER,
    ThermalAnalysis,
    ThermalAnalysisType,
    EMType
)
from ansys.dyna.core.pre.dynamaterial import MatElastic,MatThermalIsotropic,EMMATTYPE,EMEOSTabulated1
from em_set_data import *
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/em/em_resistive_heating.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = DynaSolution(hostname)
fns = []
path = examples.em_resistive_heating + os.sep
fns.append(path + "em_resistive_heating.k")
solution.open_files(fns)
solution.set_termination(termination_time=20)
solution.create_database_binary(dt=0.1)

emobj = DynaEM()
solution.add(emobj)

emobj.set_timestep(timestep_size_for_mass_scaled=0.01,max_timestep=Curve(x=[0,9.9999997474e-5],y=[0.01,0.01]))

emobj.analysis.set_timestep(timestep=0.01)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)
emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=0.05)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
emobj.add(tanalysis)

matelastic1 = MatElastic(mass_density=8000, young_modulus=1e11, poisson_ratio=0.33)
matelastic2 = MatElastic(mass_density=7000, young_modulus=1e11, poisson_ratio=0.33)
matelastic1.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=6e7)
matelastic2.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=4e6,eos=EMEOSTabulated1(Curve(x=[0, 25, 50, 100], y=[4e6, 4e6, 4e5, 4e5])))

matthermaliso1 = MatThermalIsotropic(density=8000,specific_heat=400, conductivity=400)
matthermaliso2 = MatThermalIsotropic(density=7000,specific_heat=450, conductivity=40)

part2 = SolidPart(2)
part2.set_material(matelastic1,matthermaliso1)
part2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part2)

part3 = SolidPart(3)
part3.set_material(matelastic1,matthermaliso1)
part3.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part3)

part1 = SolidPart(1)
part1.set_material(matelastic2,matthermaliso2)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

emobj.boundaryconditions.create_temperature(NodeSet(resistive_heating_tmp), scalefactor=50)
emobj.set_init_temperature(temp=25)

emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

solution.save_file()
