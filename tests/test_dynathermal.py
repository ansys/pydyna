import os

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    ThermalAnalysis,
    ThermalAnalysisType,
    SolidPart,
    SolidFormulation,
    NodeSet,
)
from ansys.dyna.core.pre.dynamaterial import MatElasticPlasticThermal


def comparefile(outputf, standardf):
    with open(outputf, "r") as fp1, open(standardf, "r") as fp2:
        line = fp1.readline()
        line = fp1.readline()
        while True:
            line1 = fp1.readline()
            line2 = fp2.readline()
            if line1 == "" or line2 == "":
                break
            if line1 != line2:
                return False
    return True


def test_thermal(thermal_initialfile, resolve_output_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(thermal_initialfile)
    solution.open_files(fns)
    solution.set_termination(3.0)
    ts = DynaMech()
    solution.add(ts)

    tanalysis = ThermalAnalysis()
    tanalysis.set_timestep(initial_timestep=0.1)
    tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
    ts.add(tanalysis)

    ts.set_timestep(timestep_size_for_mass_scaled=0.01)

    mat = MatElasticPlasticThermal(
        mass_density=1.0,
        temperatures=(0, 10, 20, 30, 40, 50),
        young_modulus=(1e10, 1e10, 1e10, 1e10, 1e10, 1e10),
        poisson_ratio=(0.3, 0.3, 0.3, 0.3, 0.3, 0.3),
        thermal_expansion=(0, 2e-6, 4e-6, 6e-6, 8e-6, 1e-5),
        yield_stress=(1e20, 1e20, 1e20, 1e20, 1e20, 1e20),
    )
    mat.set_thermal_isotropic(
        density=1, generation_rate_multiplier=10, specific_heat=1, conductivity=1
    )

    slab = SolidPart(1)
    slab.set_material(mat)
    slab.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    ts.parts.add(slab)

    # Define initial conddition
    for i in range(1, 9):
        ts.initialconditions.create_temperature(NodeSet([i]), temperature=10)

    solution.set_output_database(glstat=0.03)
    solution.create_database_binary(dt=0.01)

    outpath=solution.save_file()
    #serveroutfile = os.path.join(outpath,"test_thermal_stress.k")
    serveroutfile = '/'.join((outpath,"test_thermal_stress.k"))
    outputfile = os.path.join(resolve_output_path, "test_thermal_stress.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "thermal_stress.k")
    assert comparefile(outputfile, standardfile)
