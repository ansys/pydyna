import os
import sys
import pytest


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre.dynadem import *


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


def test_dem(dem_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(dem_initialfile)
    solution.open_files(fns)
    dem = DynaDEM()
    solution.add(dem)
    dem.set_des(ndamp=0.99, tdamp=0.99, frics=0.9, fricr=0.9)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_dem.k")
    standardfile = os.path.join(resolve_standard_path, "dem.k")
    assert comparefile(outputfile, standardfile)
