import os
import sys
import pytest

sys.path.append(os.path.join(sys.path[0],os.pardir))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynabase import *

    
def test_dynabase(initialfile,resolve_server_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(initialfile)
    solution.open_files(fns)
    solution.set_termination(0.03)
    solution.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)
    solution.create_database_binary(dt=5e-4, ieverp=1)
    solution.save_file()