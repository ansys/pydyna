import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    Airbag,
    PartSet,
    RigidwallPlanar,
    Contact,
    ContactSurface,
    ContactCategory,
    Curve,
    Point,
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


def test_mech(mech_initialfile, resolve_output_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(mech_initialfile)
    solution.open_files(fns)
    mech = DynaMech()
    solution.add(mech)
    solution.set_termination(0.03)
    airbag = Airbag(
        set=PartSet([3]),
        heat_capacity_at_constant_volume=1.736e3,
        heat_capacity_at_constant_pressure=2.43e3,
        input_gas_temperature=1.2e3,
        input_mass_flow_rate=Curve(x=[0, 0.032, 0.045, 0.08], y=[0, 26, 0.6, 0.1]),
        shape_factor_for_exit_hole=0.7,
        ambient_pressure=14.7,
        ambient_density=3.821e-6,
    )
    mech.add(airbag)
    rigidwall = RigidwallPlanar(Point(0, 0, 0), Point(0, 1, 0), coulomb_friction_coefficient=0.5)
    mech.add(rigidwall)

    contact = Contact(category=ContactCategory.NODES_TO_SURFACE)
    contact.set_friction_coefficient(static=0.5, dynamic=0.5)
    surf1 = ContactSurface(PartSet([3]))
    surf2 = ContactSurface(PartSet([2]))
    surf2.set_penalty_stiffness_scale_factor(0.06667)
    contact.set_slave_surface(surf1)
    contact.set_master_surface(surf2)
    mech.contacts.add(contact)
    solution.set_output_database(
        abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4, rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4
    )
    solution.create_database_binary(dt=5e-4, ieverp=1)
    outpath=solution.save_file()
    #serveroutfile = os.path.join(outpath,"test_mech.k")
    serveroutfile = '/'.join((outpath,"test_mech.k"))
    outputfile = os.path.join(resolve_output_path, "test_mech.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "mech.k")
    assert comparefile(outputfile, standardfile)
