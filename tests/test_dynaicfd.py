import os
import sys
import pytest


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre.dynaicfd import *


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
                print(line1)
                print(line2)
                return False
    return True


def test_icfd(icfd_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)
    icfd = DynaICFD()
    solution.add(icfd)
    solution.set_termination(termination_time=100)
    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    icfd.parts.add(partvol)
    meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
    icfd.add(meshvol)
    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_icfd.k")
    standardfile = os.path.join(resolve_standard_path, "icfd.k")
    assert comparefile(outputfile, standardfile)
