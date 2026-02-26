# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import pytest
import os
from ansys.dyna.core.run.linux_runner import LinuxRunner
from unittest.mock import patch as mock_patch
from ansys.dyna.core.run.options import MpiOption, Precision

from ansys.dyna.core.run.windows_runner import WindowsRunner

from unittest.mock import MagicMock

pytestmark = pytest.mark.run

@pytest.fixture
def patch_ansys_paths(monkeypatch):
    monkeypatch.setattr("ansys.tools.common.path.path._read_config_file", lambda: {})
    monkeypatch.setattr("ansys.tools.common.path.get_dyna_path", lambda *args, **kwargs: None)
    monkeypatch.setattr("ansys.tools.common.path.path._get_unified_install_base_for_version",
                        lambda version, supported_versions=None: ("/opt/ansys/v231", None))
    monkeypatch.setattr("ansys.tools.common.path.path.get_available_ansys_installations",
                        lambda: {251: '/usr/ansys_inc/v251'})
    monkeypatch.setattr("ansys.tools.common.path.path.get_latest_ansys_installation",
                        lambda: (241, "/opt/ansys/v241"))

@pytest.fixture
def always_isfile(monkeypatch):
    monkeypatch.setattr("pathlib.Path.is_file", lambda self: True)

@pytest.fixture
def instance():
    runner = LinuxRunner()
    runner.mpi_option = runner.mpi_option.SMP
    runner.precision = runner.precision.DOUBLE
    return runner

def test_find_solver_with_valid_executable(monkeypatch):
    fake_executable = "/fake/path/lsdyna_sp.e"
    monkeypatch.setattr("pathlib.Path.is_file", lambda self: str(self) == fake_executable)
    instance = LinuxRunner(executable=fake_executable)
    assert instance.solver == fake_executable

def test_find_solver_with_invalid_executable(monkeypatch):
    fake_executable = "/invalid/path/lsdyna_sp.e"
    monkeypatch.setattr("pathlib.Path.is_file", lambda self: False)
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

@pytest.mark.parametrize("mpi_option", [MpiOption.SMP, MpiOption.MPP_INTEL_MPI])
@pytest.mark.parametrize("activate_case,case_ids,expected_case", [
    (False, None, ""),  # No CASE
    (True, None, " CASE"),  # CASE only
    (True, [], " CASE"),    # CASE only
    (True, [1,2,3], " CASE=1,2,3"),  # CASE with IDs
])
def test_linuxrunner_case_command(patch_ansys_paths, always_isfile, activate_case, case_ids, expected_case, mpi_option):
    runner = LinuxRunner()
    runner.mpi_option = MpiOption.SMP
    runner.precision = Precision.SINGLE
    runner.ncpu = 2
    runner.get_memory_string = lambda: "100m"
    runner.set_input("input.k", "/tmp")
    runner.activate_case = activate_case
    runner.case_ids = case_ids

    with mock_patch("subprocess.run") as mock_subproc:
        mock_subproc.return_value = MagicMock()
        runner.run()


# pytest mark for MSMPI, INTELMPI, OPENMPI

@pytest.mark.parametrize("mpi_option", [MpiOption.MPP_INTEL_MPI, MpiOption.MPP_MS_MPI, MpiOption.SMP])
@pytest.mark.parametrize("activate_case,case_ids,expected", [
    (False, None, "CASE not activated"),
    (True, None, " CASE"),
    (True, [], " CASE"),
    (True, [1, 2, 3], " CASE=1,2,3"),
])
def test_get_command_line_case_options(tmp_path, activate_case, case_ids, expected, mpi_option):
    with mock_patch.object(WindowsRunner, '_find_solver', return_value=None):
        runner = WindowsRunner(
            ncpu=4,
            memory=2000,
            mpi_option=mpi_option,
            precision=Precision.DOUBLE,
            activate_case=activate_case,
            case_ids=case_ids,
            version=2024,
        )
        runner.solver = 'lsdyna_dp.exe'
        runner.solver_location = str(tmp_path)
        runner.input_file = "input.k"
        runner.working_directory = str(tmp_path)
        with mock_patch.object(runner, '_get_env_script', return_value='env.bat'):
            cmd = runner._get_command_line()
            if not activate_case:
                assert "CASE" not in cmd
            elif case_ids and isinstance(case_ids, list) and case_ids:
                assert f"CASE={','.join(str(cid) for cid in case_ids)}" in cmd
            else:
                assert "CASE" in cmd
            assert "lsdyna_dp.exe" in cmd
            assert "input.k" in cmd
            