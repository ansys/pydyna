"""This runs at the init of the pytest session

Launch or connect to a persistent local DPF service to be shared in
pytest as a sesson fixture
"""
import os
import pytest

sys.path.append(os.path.join(sys.path[0],'../'))
from ansys.dyna import pre
from ansys.dyna.pre import examples

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
def initialfile():
    """Resolve the path of the "initial.k" file."""
    return resolve_test_file("initial.rst")


@pytest.fixture(scope="session", autouse=True)
def cleanup(request):
    """Cleanup a testing directory once we are finished."""

    def close_servers():
        core.server.shutdown_all_session_servers()

    request.addfinalizer(close_servers)
