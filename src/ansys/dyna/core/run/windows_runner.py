# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""Windows implementation of LS-DYNA runner."""

import os
import subprocess

from ansys.tools.path import get_latest_ansys_installation
from ansys.tools.path.path import _get_unified_install_base_for_version

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision


class WindowsRunner(BaseRunner):
    """Windows implementation to Run LS-DYNA.

    Tested when LS-DYNA is installed as part of the
    unified Ansys installation.
    """

    def __init__(self, **kwargs):
        """Initialize WindowsRunner."""
        super().__init__(**kwargs)
        version = kwargs.get("version", None)
        self._find_solver(version)

    def set_input(self, input_file: str, working_directory: str) -> None:
        """Set input file and working directory."""
        self.input_file = input_file
        self.working_directory = working_directory

    def _find_solver(self, version: int) -> None:
        """Find LS-DYNA solver location."""
        if version is not None:
            install_loc, _ = _get_unified_install_base_for_version(version)
        else:
            _, install_loc = get_latest_ansys_installation()
        self.solver_location = os.path.join(install_loc, "ansys", "bin", "winx64")

    def _get_env_script(self) -> str:
        """Get env script when running using lsrun from workbench."""
        if self.mpi_option == MpiOption.MPP_INTEL_MPI:
            script_name = "lsdynaintelvar.bat"
        else:
            script_name = "lsdynamsvar.bat"
        lsprepost = [p for p in os.listdir(self.solver_location) if "lsprepost" in p][0]
        env_script_path = os.path.join(self.solver_location, lsprepost, "LS-Run", script_name)
        return env_script_path

    def _get_exe_name(self) -> str:
        """Get executable name based on MPI option and precision."""
        exe_name = {
            (MpiOption.SMP, Precision.SINGLE): "lsdyna_sp.exe",
            (MpiOption.SMP, Precision.DOUBLE): "lsdyna_dp.exe",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "lsdyna_mpp_sp_impi.exe",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "lsdyna_mpp_dp_impi.exe",
            (MpiOption.MPP_MS_MPI, Precision.SINGLE): "lsdyna_mpp_sp_msmpi.exe",
            (MpiOption.MPP_MS_MPI, Precision.DOUBLE): "lsdyna_mpp_dp_msmpi.exe",
        }[(self.mpi_option, self.precision)]
        return exe_name

    def _get_executable(self) -> str:
        return os.path.join(self.solver_location, self._get_exe_name())

    def _write_runscript(self) -> None:
        with open(os.path.join(self.working_directory, self._scriptname), "w") as f:
            f.write(self._get_command_line())

    @property
    def _scriptname(self) -> str:
        """Get script name to run LS-DYNA."""
        return "lsruncommand.bat"

    def run(self) -> None:
        """Run LS-DYNA."""
        self._write_runscript()
        subprocess.check_call(
            f"cmd /c {self._scriptname}",
            shell=False,
            universal_newlines=True,
            cwd=self.working_directory,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

    def _get_command_line(self) -> str:
        """Get the command line to run LS-DYNA."""
        script = f'call "{self._get_env_script()}"'
        solver = f'"{self._get_executable()}"'
        ncpu = self.ncpu
        mem = self.get_memory_string()
        input_file = self.input_file
        if self.mpi_option == MpiOption.SMP:
            command = f"{solver} i={input_file} ncpu={ncpu} memory={mem}"
        elif self.mpi_option == MpiOption.MPP_INTEL_MPI:
            # -wdir is used here because sometimes mpiexec does not pass its working directory
            # to dyna on windows when run from python subprocess
            command = (
                f'mpiexec -wdir "{self.working_directory}" -localonly -np {ncpu} {solver} i={input_file} memory={mem}'
            )
        elif self.mpi_option == MpiOption.MPP_MS_MPI:
            command = f'mpiexec -wdir "{self.working_directory}" -c {ncpu} -aa {solver} i={input_file} memory={mem}'

        return f"{script} && {command} > lsrun.out.txt 2>&1"
