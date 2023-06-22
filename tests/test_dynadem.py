import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynadem import DynaDEM


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


def test_dem(dem_initialfile, resolve_server_path, resolve_standard_path,resolve_output_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(dem_initialfile)
    solution.open_files(fns)
    dem = DynaDEM()
    solution.add(dem)
    dem.set_des(ndamp=0.99, tdamp=0.99, frics=0.9, fricr=0.9)
    outpath=solution.save_file()
    serveroutfile = os.path.join(outpath,"test_dem.k")
    outputfile = os.path.join(resolve_output_path, "test_dem.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path, "dem.k")
    assert comparefile(outputfile, standardfile)
