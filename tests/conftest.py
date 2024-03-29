"""This runs at the init of the pytest session

Launch or connect to a persistent local DPF service to be shared in
pytest as a sesson fixture
"""
import os
import pytest
from ansys.dyna.core.pre.launcher import ServerThread


# from ansys.dyna.core.pre.Server.kwserver import *

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

def get_server_path():
    """Get the filepath of server."""
    path = os.path.dirname(os.path.abspath(__file__))
    server_path = os.path.join(path, os.pardir, "src", "ansys", "dyna", "core", "pre", "Server")
    return server_path


@pytest.fixture()
def resolve_server_path():
    """Get the filepath of outputted files."""
    return get_server_path()


@pytest.fixture()
def resolve_standard_path():
    """Get the filepath of standard files."""
    local_path = os.path.dirname(os.path.abspath(__file__))
    standard_files_path = os.path.join(local_path, "testfiles", "standard")
    return standard_files_path

@pytest.fixture()
def resolve_standard_path_icfd():
    """Get the filepath of standard files."""
    local_path = os.path.dirname(os.path.abspath(__file__))
    standard_files_path_icfd = os.path.join(local_path, "testfiles", "standard","icfd")
    return standard_files_path_icfd

@pytest.fixture()
def resolve_output_path():
    """Get the filepath of output files."""
    local_path = os.path.dirname(os.path.abspath(__file__))
    output_files_path = os.path.join(local_path, "testfiles", "output")
    return output_files_path


@pytest.fixture()
def base_initialfile():
    """Resolve the path for base initial file."""
    return resolve_test_file("test_base.k", "initial")


@pytest.fixture()
def dem_initialfile():
    """Resolve the path for dem initial file."""
    return resolve_test_file("test_dem.k", "initial")


@pytest.fixture()
def em_initialfile():
    """Resolve the path for em initial file."""
    return resolve_test_file("test_em.k", "initial")

@pytest.fixture()
def resolve_solution_path():
    """Get the filepath of solution files."""
    path = os.path.dirname(os.path.abspath(__file__))
    solution_path = os.path.join(path, "testfiles", "initial", "solution")
    return solution_path

@pytest.fixture()
def resolve_icfd_path():
    """Get the filepath of icfd files."""
    path = os.path.dirname(os.path.abspath(__file__))
    icfd_path = os.path.join(path, "testfiles", "initial", "icfd")
    return icfd_path
    
@pytest.fixture()
def resolve_nvh_path():
    """Get the filepath of nvh files."""
    path = os.path.dirname(os.path.abspath(__file__))
    nvh_path = os.path.join(path, "testfiles", "initial", "nvh")
    return nvh_path
    
@pytest.fixture()
def resolve_em_path():
    """Get the filepath of em files."""
    path = os.path.dirname(os.path.abspath(__file__))
    em_path = os.path.join(path, "testfiles", "initial", "em")
    return em_path

@pytest.fixture()
def icfd_initialfile():
    """Resolve the path for icfd initial file."""
    return resolve_test_file("test_icfd.k", "initial")


@pytest.fixture()
def iga_initialfile():
    """Resolve the path for iga initial file."""
    return resolve_test_file("test_iga.k", "initial")


@pytest.fixture()
def mech_initialfile():
    """Resolve the path for mech initial file."""
    return resolve_test_file("test_mech.k", "initial")


@pytest.fixture()
def sale_initialfile():
    """Resolve the path for sale initial file."""
    return resolve_test_file("test_sale.k", "initial")


@pytest.fixture()
def isph_initialfile():
    """Resolve the path for isph initial file."""
    return resolve_test_file("test_isph.k", "initial")


@pytest.fixture()
def nvh_initialfile():
    """Resolve the path for nvh initial file."""
    return resolve_test_file("test_nvh.k", "initial")
    
@pytest.fixture()
def thermal_initialfile():
    """Resolve the path for thermal initial file."""
    return resolve_test_file("test_thermal_stress.k", "initial")
    
@pytest.fixture(scope = "session",autouse=True)
def Connect_Server():
    """Connect to the kwserver."""
    path = get_server_path()
    threadserver = ServerThread(1,port=50051,ip="127.0.0.1",server_path = path)
    threadserver.setDaemon(True)
    threadserver.start()
    