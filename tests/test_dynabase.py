import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynabase import (
    DynaBase,
    Switch,
    InvariantNode,
    EnergyFlag,
    HourglassControl,
    BulkViscosity,
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


def test_dynabase(base_initialfile, resolve_standard_path,resolve_output_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(base_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=0.12)
    solution.create_database_binary(dt=2.5e-3)
    dbase = DynaBase()
    solution.add(dbase)
    dbase.set_timestep(tssfac=0.8)
    dbase.set_accuracy(
        objective_stress_updates=Switch.ON,
        invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,
        implicit_accuracy_flag=Switch.ON,
    )
    dbase.set_energy(
        hourglass_energy=EnergyFlag.COMPUTED,
        sliding_interface_energy=EnergyFlag.COMPUTED,
    )
    dbase.set_hourglass(
        controltype=HourglassControl.FLANAGAN_BELYTSCHKO_INTEGRATION_SOLID, coefficient=0
    )
    dbase.set_bulk_viscosity(bulk_viscosity_type=BulkViscosity.COMPUTE_INTERNAL_ENERGY_DISSIPATED)
    dbase.create_control_shell(
        wrpang=0,
        esort=1,
        irnxx=0,
        istupd=4,
        theory=0,
        bwc=1,
        miter=1,
        proj=1,
        irquad=0,
    )
    dbase.create_control_contact(rwpnal=1.0, ignore=1, igactc=0)
    outpath=solution.save_file()
    #serveroutfile = os.path.join(outpath,"test_base.k")
    serveroutfile = '/'.join((outpath,"test_base.k"))
    outputfile = os.path.join(resolve_output_path, "test_base.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "base.k")
    assert comparefile(outputfile, standardfile)
