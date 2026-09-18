from unittest.mock import MagicMock, patch

import pytest

from ansys.dyna.core.run.options import MpiOption, Precision
import ansys.dyna.core.run.windows_runner as windows_runner

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
    assert runner.solver.endswith('lsdyna.exe"')


@patch("ansys.dyna.core.run.windows_runner._get_unified_install_base_for_version")
def test_find_solver_with_version(mock_install_base, tmp_workdir):
    exe = tmp_workdir / "ansys" / "bin" / "winx64" / "lsdyna_sp.exe"
    exe.parent.mkdir(parents=True)
    exe.write_text("dummy")

    mock_install_base.return_value = (tmp_workdir, None)
    runner = windows_runner.WindowsRunner(version=241, precision=Precision.SINGLE)
    assert runner.solver.endswith('lsdyna_sp.exe"')


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


@pytest.fixture()
def short_path_api(monkeypatch):
    """Provide a WinAPI mock on every test platform."""
    api = MagicMock()
    windll = MagicMock()
    windll.kernel32.GetShortPathNameW = api
    monkeypatch.setattr(windows_runner.ctypes, "windll", windll, raising=False)
    return api


def test_get_short_path_no_comma(short_path_api):
    path = r"C:\normal path\input.k"
    assert windows_runner._get_short_path(path) == path
    short_path_api.assert_not_called()


def test_get_short_path_with_comma_converts_long_path(short_path_api):
    path = "C:\\Company, Inc\\" + "long folder\\" * 30 + "input.k"
    short = "C:\\COMPAN~1\\" + "FOLDER~1\\" * 35 + "input.k"
    assert "," in path
    assert len(short) > 260

    def convert(value, buffer, size):
        assert value == path
        if buffer is None:
            assert size == 0
            return len(short) + 1
        assert size == len(short) + 1
        buffer.value = short
        return len(short)

    short_path_api.side_effect = convert
    assert windows_runner._get_short_path(path) == short
    assert short_path_api.call_count == 2


@pytest.mark.parametrize("results", ([0], [30, 0], [30, 40]))
def test_get_short_path_api_failure_preserves_path(short_path_api, caplog, results):
    path = r"C:\Company, Inc\input.k"
    short_path_api.side_effect = results
    assert windows_runner._get_short_path(path) == path
    assert short_path_api.call_count == len(results)
    assert "using original path" in caplog.text


def test_get_short_path_exception_preserves_path(short_path_api, caplog):
    path = r"C:\Company, Inc\input.k"
    short_path_api.side_effect = OSError("API unavailable")
    assert windows_runner._get_short_path(path) == path
    short_path_api.assert_called_once_with(path, None, 0)
    assert "API unavailable" in caplog.text


def test_get_short_path_without_short_name_preserves_path(short_path_api, caplog):
    path = r"C:\Company, Inc\input.k"

    def convert(value, buffer, size):
        if buffer is None:
            return len(path) + 1
        buffer.value = value
        return len(value)

    short_path_api.side_effect = convert
    assert windows_runner._get_short_path(path) == path
    assert short_path_api.call_count == 2
    assert "No comma-free short path" in caplog.text


def test_set_input_converts_file_and_directory(short_path_api):
    input_file = r"C:\Company, Inc\input.k"
    workdir = r"C:\Company, Inc"
    expected = {input_file: r"C:\COMPAN~1\input.k", workdir: r"C:\COMPAN~1"}

    def convert(value, buffer, size):
        short = expected[value]
        if buffer is None:
            return len(short) + 1
        buffer.value = short
        return len(short)

    short_path_api.side_effect = convert
    runner = windows_runner.WindowsRunner.__new__(windows_runner.WindowsRunner)
    runner.set_input(input_file, workdir)
    assert runner.input_file == expected[input_file]
    assert runner.working_directory == expected[workdir]
    assert short_path_api.call_count == 4
