"""This runs at the init of the pytest session

Launch or connect to a persistent local DPF service to be shared in
pytest as a sesson fixture
"""
import os
import sys
import pytest

sys.path.append(os.path.join(sys.path[0],os.pardir))
sys.path.append(os.path.join(sys.path[0],os.pardir,"ansys","dyna","pre","Server"))
from ansys.dyna import pre
from ansys.dyna.pre import examples
#from ansys.dyna.pre.Server.kwserver import *

def resolve_test_file(basename, additional_path=""):
    """Resolves a test file's full path based on the base name and the
    environment.
    """
    test_path = os.path.dirname(os.path.abspath(__file__))
    test_files_path = os.path.join(test_path, "testfiles")
    filename = os.path.join(test_files_path, additional_path, basename)
    if not os.path.isfile(filename):
        raise FileNotFoundError(f"Unable to locate {basename} at {test_files_path}")
    return filename

@pytest.fixture()   
def resolve_server_path():
    """Get the filepath of outputted files."""
    path = os.path.dirname(os.path.abspath(__file__))
    server_path = os.path.join(path, os.pardir,"ansys","dyna","pre","Server")
    return server_path

@pytest.fixture()   
def resolve_standard_path():
    """Get the filepath of standard files."""
    local_path = os.path.dirname(os.path.abspath(__file__))
    standard_files_path = os.path.join(local_path, "testfiles","standard")
    return standard_files_path

@pytest.fixture()
def initialfile():
    """Resolve the path of the "initial.k" file."""
    return resolve_test_file("initial.k")



    
