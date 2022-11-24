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
def base_initialfile():
    """Resolve the path for base initial file."""
    return resolve_test_file("test_base.k","initial")

@pytest.fixture()
def dem_initialfile():
    """Resolve the path for dem initial file."""
    return resolve_test_file("test_dem.k","initial")
    
@pytest.fixture()
def em_initialfile():
    """Resolve the path for em initial file."""
    return resolve_test_file("test_em.k","initial")
    
@pytest.fixture()
def icfd_initialfile():
    """Resolve the path for icfd initial file."""
    return resolve_test_file("test_icfd.k","initial")    
    
@pytest.fixture()
def iga_initialfile():
    """Resolve the path for iga initial file."""
    return resolve_test_file("test_iga.k","initial")       
    
@pytest.fixture()
def mech_initialfile():
    """Resolve the path for mech initial file."""
    return resolve_test_file("test_mech.k","initial")       
    
@pytest.fixture()
def sale_initialfile():
    """Resolve the path for sale initial file."""
    return resolve_test_file("test_sale.k","initial")       
    
@pytest.fixture()
def solution_initialfile():
    """Resolve the path for solution initial file."""
    return resolve_test_file("test_solution.k","initial")      
    
@pytest.fixture()
def isph_initialfile():
    """Resolve the path for isph initial file."""
    return resolve_test_file("test_isph.k","initial") 
    
@pytest.fixture()
def nvh_initialfile():
    """Resolve the path for nvh initial file."""
    return resolve_test_file("test_nvh.k","initial") 
    
    
    
