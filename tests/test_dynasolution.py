import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    Transform
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


def test_solution(resolve_solution_path, resolve_server_path, resolve_standard_path,resolve_output_path):
    solution = DynaSolution("localhost")
    solution_initialfile = os.path.join(resolve_solution_path, "test_solution.k")
    fns = []
    fns.append(solution_initialfile)
    solution.open_files(fns)
    solution.set_termination(0.03)
    solution.set_output_database(
        abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4, rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4
    )
    solution.create_database_binary(dt=5e-4, ieverp=1)
    outpath = solution.save_file()
    serveroutfile = os.path.join(outpath,"test_solution.k")
    outputfile = os.path.join(resolve_output_path, "test_solution.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path,"solution", "solution.k")
    assert comparefile(outputfile, standardfile)

def test_elementary_transform(resolve_solution_path, resolve_server_path, resolve_standard_path,resolve_output_path):
    """Create *INCLUDE_TRANSFORM and *DEFINE_TRANSFORMATION."""
    solution = DynaSolution("localhost")
    solution_initialfile = os.path.join(resolve_solution_path, "test_elementary_main.k")
    fns = []
    fns.append(solution_initialfile)
    solution.open_files(fns)
    obj = DynaMech()
    solution.add(obj)

    obj.set_transform(
        filename="transform.k",idnoff=100,ideoff=100,idpoff=100,idmoff=100,idsoff=100,idfoff=100,
        transform = Transform("MIRROR",param1=-4,param2=0,param3=0,param4=-5,param5=0,param6=0)
        )

    outpath=solution.save_file()
    serveroutfile = os.path.join(outpath,"test_elementary_main.k")
    outputfile = os.path.join(resolve_output_path, "test_elementary_main.k")
    solution.download(serveroutfile,outputfile)
    standardfile = os.path.join(resolve_standard_path,"solution", "elementary_main.k")
    assert comparefile(outputfile, standardfile)