"""This runs at the init of the pytest session

Launch or connect to a persistent local DPF service to be shared in
pytest as a sesson fixture
"""
import os

import pytest

from ansys.dpf import core


# currently running dpf on docker.  Used for testing on CI
running_docker = os.environ.get("DPF_DOCKER", False)


def resolve_test_file(basename, additional_path=""):
    """Resolves a test file's full path based on the base name and the
    environment.

    Normally returns local path unless server is running on docker and
    this repository has been mapped to the docker image at /dpf.
    """
    if running_docker:
        # assumes repository root is mounted at '/dpf'
        test_files_path = "/dpf/tests/testfiles"
        return os.path.join(test_files_path, additional_path, basename)
    else:
        # otherwise, assume file is local
        test_path = os.path.dirname(os.path.abspath(__file__))
        test_files_path = os.path.join(test_path, "testfiles")
        filename = os.path.join(test_files_path, additional_path, basename)
        if not os.path.isfile(filename):
            raise FileNotFoundError(f"Unable to locate {basename} at {test_files_path}")
        return filename

@pytest.fixture(scope="session", autouse=True)
def cleanup(request):
    """Cleanup a testing directory once we are finished."""

    def close_servers():
        core.server.shutdown_all_session_servers()

    request.addfinalizer(close_servers)
