import pytest
import os
from ansys.dyna.core.run.linux_runner import LinuxRunner

@pytest.fixture
def patch_ansys_paths(monkeypatch):
    monkeypatch.setattr("ansys.tools.path.path._read_config_file", lambda: {})
    monkeypatch.setattr("ansys.tools.path.get_dyna_path", lambda *args, **kwargs: None)
    monkeypatch.setattr("ansys.tools.path.path._get_unified_install_base_for_version",
                        lambda version, supported_versions=None: ("/opt/ansys/v231", None))
    monkeypatch.setattr("ansys.tools.path.path.get_available_ansys_installations",
                        lambda: {251: '/usr/ansys_inc/v251'})
    monkeypatch.setattr("ansys.tools.path.path.get_latest_ansys_installation",
                        lambda: (241, "/opt/ansys/v241"))

@pytest.fixture
def always_isfile(monkeypatch):
    monkeypatch.setattr(os.path, "isfile", lambda path: True)

@pytest.fixture
def instance():
    runner = LinuxRunner()
    runner.mpi_option = runner.mpi_option.SMP
    runner.precision = runner.precision.DOUBLE
    return runner

def test_find_solver_with_valid_executable(monkeypatch):
    fake_executable = "/fake/path/lsdyna_sp.e"
    monkeypatch.setattr(os.path, "isfile", lambda path: path == fake_executable)
    instance = LinuxRunner(executable=fake_executable)
    assert instance.solver == fake_executable

def test_find_solver_with_invalid_executable(monkeypatch):
    fake_executable = "/invalid/path/lsdyna_sp.e"
    monkeypatch.setattr(os.path, "isfile", lambda path: False)
    with pytest.raises(FileNotFoundError, match="LS-DYNA executable not found"):
        LinuxRunner(executable=fake_executable)

def test_find_solver_with_version(patch_ansys_paths, always_isfile):
    instance = LinuxRunner(version=231)
    expected_exe = "ansys/bin/linx64/lsdyna_dp.e"
    assert instance.solver == expected_exe

def test_find_solver_latest_install(patch_ansys_paths, always_isfile):
    instance = LinuxRunner()
    expected_exe = "/usr/ansys_inc/v251/ansys/bin/linx64/lsdyna_dp.e"
    assert instance.solver == expected_exe


# --- CASE command logic test ---
import subprocess
from unittest.mock import patch as mock_patch
from ansys.dyna.core.run.options import MpiOption, Precision

from unittest.mock import MagicMock

@pytest.mark.parametrize("activate_case,case_ids,expected_case", [
    (False, None, ""),  # No CASE
    (True, None, " CASE"),  # CASE only
    (True, [], " CASE"),    # CASE only
    (True, [1,2,3], " CASE=1,2,3"),  # CASE with IDs
])
def test_linuxrunner_case_command(patch_ansys_paths, always_isfile, activate_case, case_ids, expected_case):
    runner = LinuxRunner()
    runner.mpi_option = MpiOption.SMP
    runner.precision = Precision.SINGLE
    runner.ncpu = 2
    runner.get_memory_string = lambda: "100m"
    runner.set_input("input.k", "/tmp")
    runner.activate_case = activate_case
    runner.case_ids = case_ids

    with mock_patch("subprocess.run") as mock_subproc:
        mock_subproc.return_value = MagicMock()  # <-- Add this line
        runner.run()
    