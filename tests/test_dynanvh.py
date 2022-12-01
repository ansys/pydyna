import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynanvh import (
    DynaNVH,
    FrequencyDomain,
    Curve,
    NodeSet,
    ResponseDOF,
    ResponseType,
    ShellPart,
    ShellFormulation,
)
from ansys.dyna.core.pre.dynamaterial import MatElastic


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


def test_nvh(nvh_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(nvh_initialfile)
    solution.open_files(fns)
    nvhobj = DynaNVH()
    solution.add(nvhobj)

    nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
    nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
    nvhobj.implicitanalysis.set_solution(solution_method=1)

    fd = FrequencyDomain()
    crv = Curve(
        x=[1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 200],
        y=[0, 0, 0.0045, 0.00713, 0.00386, 0.00328, 0.0034, 0.00624, 7.2e-4, 8.3e-4, 0, 0],
    )
    fd.set_frequency_response_function(
        excitation_input_set=NodeSet([131]),
        max_natural_frequency=2000,
        modal_damping_coefficient_curve_type=1,
        modal_damping_coefficient_curve=crv,
        response_output_set=NodeSet([131, 651]),
        response_output_dof=ResponseDOF.Z,
        response_output_type=ResponseType.BASE_ACCELERATION,
        frf_output_min_frequency=1,
        frf_output_max_frequency=400,
        frf_output_num_frequency=400,
    )
    nvhobj.add(fd)

    matelastic = MatElastic(mass_density=7870, young_modulus=2.07e11, poisson_ratio=0.292)

    boxshell = ShellPart(1)
    boxshell.set_material(matelastic)
    boxshell.set_element_formulation(ShellFormulation.SR_HUGHES_LIU)
    boxshell.set_thickness(0.002)
    boxshell.set_shear_factor(0.833)
    boxshell.set_printout(3)
    nvhobj.parts.add(boxshell)

    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_nvh.k")
    standardfile = os.path.join(resolve_standard_path, "nvh.k")
    assert comparefile(outputfile, standardfile)
