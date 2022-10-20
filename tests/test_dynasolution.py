import os
import sys
import pytest

sys.path.append(os.path.join(sys.path[0],os.pardir))
from ansys.dyna.pre.dynasolution import *

def comparefile(outputf,standardf):
    with open(outputf,'r') as fp1,open (standardf,'r') as fp2:
        line = fp1.readline()
        line = fp1.readline()
        while True:
            line1 = fp1.readline()
            line2 = fp2.readline()
            if line1=='' or line2 == '':
                break
            if line1 != line2:
                return False
    return True


def test_solution(solution_initialfile,resolve_server_path,resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(solution_initialfile)
    solution.open_files(fns)
    solution.set_termination(0.03)
    solution.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)
    solution.create_database_binary(dt=5e-4, ieverp=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path,"output","test_solution.k")
    standardfile = os.path.join(resolve_standard_path,"solution.k")
    assert comparefile(outputfile,standardfile)

    