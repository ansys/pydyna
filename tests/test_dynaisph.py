import os

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaisph import (
    DynaISPH,
    ShellPart,
    ISPHStructPart,
    ISPHFluidPart,
    Point,
    PartSet,
    ShellFormulation,
    Curve,
    Gravity,
    DOF,
    Motion,
    Box,
    GravityOption,
)
from ansys.dyna.core.pre.dynamaterial import (
    MatRigid,
    MatSPHIncompressibleFluid,
    MatSPHIncompressibleStructure,
)


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


def test_isph(isph_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(isph_initialfile)
    solution.open_files(fns)
    solution.set_termination(0.5)
    isphobj = DynaISPH()
    solution.add(isphobj)
    isphobj.set_timestep(
        tssfac=1, max_timestep=Curve(x=[0, 0.04, 0.05, 0.1, 100], y=[0.5, 0.5, 1, 1, 1])
    )
    isphobj.isphanalysis.set_box(Box(-750, 800, -800, 800, -100, 3000))

    platemat1 = MatRigid(
        mass_density=1e-9,
        young_modulus=10,
        center_of_mass_constraint=1,
        translational_constraint=5,
        rotational_constraint=4,
    )
    platemat2 = MatRigid(
        mass_density=1e-9,
        young_modulus=10,
        center_of_mass_constraint=1,
        translational_constraint=7,
        rotational_constraint=7,
    )
    matsphfluid = MatSPHIncompressibleFluid(
        mass_density=1e-9,
        dynamic_viscosity=1e-9,
        tension_coefficient1=1e6,
        tension_coefficient2=1000,
    )
    matsphstruct = MatSPHIncompressibleStructure(mass_density=1e-9)

    sensorplane = ShellPart(1)
    sensorplane.set_material(platemat2)
    sensorplane.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    sensorplane.set_thickness(0.1)
    isphobj.parts.add(sensorplane)

    movingcube = ShellPart(7)
    movingcube.set_material(platemat1)
    movingcube.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    movingcube.set_thickness(0.1)
    isphobj.parts.add(movingcube)

    wallsmesh = ShellPart(8)
    wallsmesh.set_material(platemat2)
    wallsmesh.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    wallsmesh.set_thickness(0.1)
    isphobj.parts.add(wallsmesh)

    sphwall = ISPHStructPart(4, PartSet([8]), 12)
    sphwall.set_material(matsphstruct)
    sphwall.set_smoothing_length(1, 1, 1, 12)
    isphobj.parts.add(sphwall)

    sphcube = ISPHStructPart(5, PartSet([7]), 12)
    sphcube.set_material(matsphstruct)
    sphcube.set_smoothing_length(1, 1, 1, 12)
    isphobj.parts.add(sphcube)

    sphwater = ISPHFluidPart(6, Point(-588, -588, 9), Point(1176, 1176, 204), 98, 98, 17)
    sphwater.set_material(matsphfluid)
    sphwater.set_smoothing_length(1, 1, 1, 12)
    sphwater.create_massflow_plane(PartSet([1]))
    isphobj.parts.add(sphwater)

    # Constraint
    isphobj.constraints.merge_two_rigid_bodies(7, 1)

    # Define boundary conddition
    isphobj.boundaryconditions.create_imposed_motion(
        PartSet([7]),
        Curve(x=[0, 0.1, 0.11, 20], y=[3000, 3000, 0, 0]),
        dof=DOF.X_TRANSLATIONAL,
        motion=Motion.VELOCITY,
    )
    isphobj.boundaryconditions.create_imposed_motion(
        PartSet([7]),
        Curve(x=[0, 0.1, 0.11, 20], y=[500, 500, 500, 500]),
        dof=DOF.Z_ROTATIONAL,
        motion=Motion.VELOCITY,
        scalefactor=0.01,
    )

    # Load
    g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0, 100], y=[9810, 9810]))
    isphobj.add(g)

    solution.set_output_database(glstat=0.001, sphmassflow=0.001)
    solution.create_database_binary(dt=0.01)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_isph.k")
    standardfile = os.path.join(resolve_standard_path, "isph.k")
    assert comparefile(outputfile, standardfile)
