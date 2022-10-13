import pytest

sys.path.append(os.path.join(sys.path[0],'../'))
from ansys.dyna.pre.dynasolution import *


def test_solution(initialfile):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(initialfile)
    airbag_solution.open_files(fns)
    airbag_solution.set_termination(0.03)
    solution.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)
    solution.create_database_binary(dt=5e-4, ieverp=1)
    solution.save_file()