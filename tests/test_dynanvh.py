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
    EnergyFlag,
    OutputEcho,
    ExcitationDOF,
    ExcitationType,
    SolidPart,
    SolidFormulation,
)
from ansys.dyna.core.pre.dynamaterial import (MatElastic,MatPiecewiseLinearPlasticity)


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


def test_nvh_frf_plate_damping(resolve_nvh_path, resolve_output_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    nvh_initialfile = os.path.join(resolve_nvh_path, "test_frf_plate_damping.k")
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

    outpath=solution.save_file()
    #serveroutfile = os.path.join(outpath,"test_frf_plate_damping.k")
    serveroutfile = '/'.join((outpath,"test_frf_plate_damping.k"))
    outputfile = os.path.join(resolve_output_path, "test_frf_plate_damping.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "nvh", "frf_plate_damping.k")
    assert comparefile(outputfile, standardfile)
    
def test_nvh_frf_solid(resolve_nvh_path, resolve_output_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    nvh_initialfile = os.path.join(resolve_nvh_path, "test_frf_solid.k")
    fns = []
    fns.append(nvh_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=1)

    nvhobj = DynaNVH()
    solution.add(nvhobj)

    nvhobj.set_energy(hourglass_energy=EnergyFlag.COMPUTED)
    nvhobj.set_output(print_suppression_d3hsp=True,print_suppression_echo=OutputEcho.SUPPRESSED_NODAL_AND_ELEMENT_PRINTING)

    nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
    nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
    nvhobj.implicitanalysis.set_solution(solution_method=1)

    fd = FrequencyDomain()

    outputset = [290,292,294,296,298,300,302,304,306,380,382,384,386,388,390,482,484,486,488,490,492,578,580,582,638,640,706,708]
    fd.set_frequency_response_function(
        excitation_input_dof=ExcitationDOF.X,
        excitation_input_type=ExcitationType.BASE_ACCELERATION,
        max_natural_frequency=20,
        modal_damping_coefficient=0.01,
        response_output_set=NodeSet(outputset),
        response_output_dof=ResponseDOF.X,
        response_output_type=ResponseType.BASE_ACCELERATION,
        frf_output_min_frequency=0.01,
        frf_output_max_frequency=10,
        frf_output_num_frequency=1000,
    )
    nvhobj.add(fd)

    plastic1 = MatPiecewiseLinearPlasticity(
        mass_density=4.99e-07, young_modulus=11.37,poisson_ratio=0.32, yield_stress=0.0468
    )
    plastic2 = MatPiecewiseLinearPlasticity(
        mass_density=4.99e-07, young_modulus=110.37,poisson_ratio=0.32, yield_stress=0.0468
    )

    lower = SolidPart(4)
    lower.set_material(plastic1)
    lower.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
    nvhobj.parts.add(lower)

    upper = SolidPart(5)
    upper.set_material(plastic2)
    upper.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
    nvhobj.parts.add(upper)

    spc = [163,166,169,172,175,178,181,184,187,307,310,313,316,319,322,391,394,397,400,403,406,493,496,499,589,592,645,648]
    nvhobj.boundaryconditions.create_spc(NodeSet(spc))

    solution.create_database_binary(dt=0.1)
    solution.set_output_database(
        glstat=0.1,
        matsum=0.1,
    )
    outpath=solution.save_file()
    #serveroutfile = os.path.join(outpath,"test_frf_solid.k")
    serveroutfile = '/'.join((outpath,"test_frf_solid.k"))
    outputfile = os.path.join(resolve_output_path, "test_frf_solid.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "nvh", "frf_solid.k")
    assert comparefile(outputfile, standardfile)
