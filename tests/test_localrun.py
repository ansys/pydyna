import pytest
import os
from pathlib import Path
from ansys.dyna.core.run.linux_runner import LinuxRunner


@pytest.fixture
def instance():
    # Set default precision and mpi_option to avoid _get_exe_name() KeyError
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


def test_find_solver_with_version(monkeypatch):
    monkeypatch.setattr("ansys.tools.path.path._read_config_file", lambda: {})
    monkeypatch.setattr("ansys.tools.path.get_dyna_path", lambda *args, **kwargs: None)
    monkeypatch.setattr("ansys.tools.path.path._get_unified_install_base_for_version",
                        lambda version, supported_versions=None: ("/opt/ansys/v231", None))
    monkeypatch.setattr(os.path, "isfile", lambda path: True)

    instance = LinuxRunner(version=231)

    expected_exe = "ansys/bin/linx64/lsdyna_dp.e"
    assert instance.solver == expected_exe


def test_find_solver_latest_install(monkeypatch):
    monkeypatch.setattr("ansys.tools.path.path._read_config_file", lambda: {})
    monkeypatch.setattr("ansys.tools.path.get_dyna_path", lambda *args, **kwargs: None)
    monkeypatch.setattr("ansys.tools.path.path.get_available_ansys_installations",
                        lambda: {251: '/usr/ansys_inc/v251'})

    monkeypatch.setattr(
        "ansys.tools.path.path.get_latest_ansys_installation",
        lambda: (241, "/opt/ansys/v241")
    )

    monkeypatch.setattr(os.path, "isfile", lambda path: True)

    instance = LinuxRunner()

    expected_exe = "/usr/ansys_inc/v251/ansys/bin/linx64/lsdyna_dp.e"
    assert instance.solver == expected_exe


