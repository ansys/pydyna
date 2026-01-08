import pytest
from pathlib import Path
from unittest.mock import MagicMock, patch

import ansys.dyna.core.run.windows_runner as windows_runner
from ansys.dyna.core.run.options import MpiOption, Precision

pytestmark = pytest.mark.run

@pytest.fixture
def tmp_workdir(tmp_path):
    return tmp_path


def make_runner(tmp_workdir, mpi=MpiOption.SMP, precision=Precision.SINGLE):
    # create dummy solver executable
    exe = tmp_workdir / "lsdyna.exe"
    exe.write_text("dummy")

    runner = windows_runner.WindowsRunner(executable=str(exe))
    runner.mpi_option = mpi
    runner.precision = precision
    runner.input_file = "input.k"
    runner.working_directory = str(tmp_workdir)
    runner.ncpu = 4
    runner.get_memory_string = MagicMock(return_value="2000m")
    return runner


def test_find_solver_with_explicit_executable(tmp_workdir):
    exe = tmp_workdir / "lsdyna.exe"
    exe.write_text("dummy")

    runner = windows_runner.WindowsRunner(executable=str(exe))
    assert runner.solver_location == str(tmp_workdir)
    assert runner.solver.endswith("lsdyna.exe\"")


@patch("ansys.dyna.core.run.windows_runner._get_unified_install_base_for_version")
def test_find_solver_with_version(mock_install_base, tmp_workdir):
    exe = tmp_workdir / "ansys" / "bin" / "winx64" / "lsdyna_sp.exe"
    exe.parent.mkdir(parents=True)
    exe.write_text("dummy")

    mock_install_base.return_value = (tmp_workdir, None)
    runner = windows_runner.WindowsRunner(version=241, precision=Precision.SINGLE)
    assert runner.solver.endswith("lsdyna_sp.exe\"")

def test_find_solver_executable_not_found(tmp_workdir):
    with pytest.raises(FileNotFoundError):
        windows_runner.WindowsRunner(executable=str(tmp_workdir / "missing.exe"))


@pytest.mark.parametrize(
    "mpi,prec,expected",
    [
        (MpiOption.SMP, Precision.SINGLE, "lsdyna_sp.exe"),
        (MpiOption.SMP, Precision.DOUBLE, "lsdyna_dp.exe"),
        (MpiOption.MPP_INTEL_MPI, Precision.SINGLE, "lsdyna_mpp_sp_impi.exe"),
        (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE, "lsdyna_mpp_dp_impi.exe"),
        (MpiOption.MPP_MS_MPI, Precision.SINGLE, "lsdyna_mpp_sp_msmpi.exe"),
        (MpiOption.MPP_MS_MPI, Precision.DOUBLE, "lsdyna_mpp_dp_msmpi.exe"),
    ],
)
def test_get_exe_name_variants(tmp_workdir, mpi, prec, expected):
    runner = make_runner(tmp_workdir, mpi, prec)
    assert runner._get_exe_name() == expected


def test_get_env_script_intel(tmp_workdir):
    runner = make_runner(tmp_workdir, MpiOption.MPP_INTEL_MPI)
    (tmp_workdir / "lsprepost_foo").mkdir()
    script = runner._get_env_script()
    assert script.endswith("lsdynaintelvar.bat")


def test_write_runscript(tmp_workdir):
    runner = make_runner(tmp_workdir)
    runner._get_command_line = MagicMock(return_value="echo hello")
    runner._write_runscript()
    script_path = tmp_workdir / runner._scriptname
    assert "echo hello" in script_path.read_text()


@patch("ansys.dyna.core.run.windows_runner.subprocess.Popen")
def test_run_success(mock_popen, tmp_workdir):
    runner = make_runner(tmp_workdir)
    runner._get_command_line = MagicMock(return_value="echo hello")

    process = MagicMock()
    process.poll.side_effect = [None, 0]
    process.wait.return_value = 0
    process.returncode = 0
    mock_popen.return_value = process

    # fake log file
    log_file = tmp_workdir / "lsrun.out.txt"
    log_file.write_text("all good\n")

    runner.run()
    assert process.wait.called


@patch("ansys.dyna.core.run.windows_runner.subprocess.Popen")
def test_run_with_warning_logs(mock_popen, tmp_workdir, caplog):
    runner = make_runner(tmp_workdir)
    runner._get_command_line = MagicMock(return_value="echo hello")

    process = MagicMock()
    process.poll.side_effect = [None, 0]
    process.wait.return_value = 0
    process.returncode = 0
    mock_popen.return_value = process

    log_file = tmp_workdir / "lsrun.out.txt"
    log_file.write_text("Warning: something\n")

    runner.run()
    assert "completed with warnings" in caplog.text


@patch("ansys.dyna.core.run.windows_runner.subprocess.Popen")
def test_run_failure(mock_popen, tmp_workdir):
    runner = make_runner(tmp_workdir)
    runner._get_command_line = MagicMock(return_value="echo hello")

    process = MagicMock()
    process.poll.side_effect = [0]
    process.wait.return_value = 1
    process.returncode = 1
    mock_popen.return_value = process

    log_file = tmp_workdir / "lsrun.out.txt"
    log_file.write_text("Error: fail\n")

    with pytest.raises(RuntimeError):
        runner.run()
