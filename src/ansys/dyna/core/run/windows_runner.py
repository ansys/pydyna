# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

import logging
import os
from pathlib import Path
import subprocess
import time

from ansys.tools.path import get_latest_ansys_installation
from ansys.tools.path.path import _get_unified_install_base_for_version

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision

log = logging.getLogger(__name__)


class WindowsRunner(BaseRunner):
    """Windows implementation to Run LS-DYNA.

    Tested when LS-DYNA is installed as part of the
    unified Ansys installation.
    """

    def __init__(self, **kwargs):
        """Initialize WindowsRunner."""
        super().__init__(**kwargs)
        version = kwargs.get("version", None)
        executable = kwargs.get("executable", None)
        self._find_solver(version, executable)

    def set_input(self, input_file: str, working_directory: str) -> None:
        """Set input file and working directory."""
        self.input_file = input_file
        self.working_directory = working_directory

    def _find_solver(self, version: int, executable: str = None) -> None:
        """Find LS-DYNA solver location."""
        if executable:
            exe_path = Path(executable)
            if exe_path.is_file():
                self.solver_location = str(exe_path.parent)
                self.solver = f'"{str(exe_path)}"'
                return
            raise FileNotFoundError(f"Specified executable not found: {executable}")
        if version:
            install_base, _ = _get_unified_install_base_for_version(version)
        else:
            _, install_base = get_latest_ansys_installation()
        solver_dir = Path(install_base) / "ansys" / "bin" / "winx64"
        solver_exe = solver_dir / self._get_exe_name()
        if not solver_exe.is_file():
            raise FileNotFoundError(f"LS-DYNA executable not found: {solver_exe}")
        self.solver_location = str(solver_dir)
        self.solver = f'"{str(solver_exe)}"'

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
        script_path = Path(self.working_directory) / self._scriptname
        log_file = Path(self.working_directory) / "lsrun.out.txt"

        try:
            process = subprocess.Popen(
                ["cmd", "/c", str(script_path)],
                cwd=self.working_directory,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                universal_newlines=True,
                bufsize=1,
            )
            log.info("LS-DYNA execution started.")

            warning_detected = False

            while process.poll() is None:
                if log_file.exists():
                    with log_file.open("r", encoding="utf-8", errors="ignore") as f:
                        content = f.readlines()
                        for line in content:
                            if "warning" in line.lower() or "error" in line.lower():
                                warning_detected = True
                time.sleep(2)

            process.wait()
            if warning_detected:
                log.warning("LS-DYNA completed with warnings or errors in the log.")
                log.warning(f"Check the log file for details: {log_file}")

            if process.returncode != 0:
                log.error(f"LS-DYNA run failed with exit code {process.returncode}.")
                if log_file.exists():
                    log.error(f"See log file for details: {log_file}")
                raise RuntimeError(f"LS-DYNA failed with exit code {process.returncode}")

            log.info("LS-DYNA run completed successfully.")

        except subprocess.SubprocessError as e:
            msg = f"Subprocess execution failed: {e}"
            msg += f"to run LS-DYNA in {self.working_directory} with command: {self._get_command_line()}"
            if log_file.exists():
                msg += f"\nSee log file at: {log_file}"
            log.error(msg)
            raise RuntimeError(msg) from e

    def _get_command_line(self) -> str:
        """Get the command line to run LS-DYNA."""
        script = f'call "{self._get_env_script()}"'
        ncpu = self.ncpu
        mem = self.get_memory_string()
        input_file = self.input_file

        if not os.path.isabs(self.working_directory):
            self.working_directory = os.path.abspath(self.working_directory)

        if self.mpi_option == MpiOption.SMP:
            command = f"{self.solver} i={input_file} ncpu={ncpu} memory={mem}"
        elif self.mpi_option == MpiOption.MPP_INTEL_MPI:
            # -wdir is used here because sometimes mpiexec does not pass its working directory
            # to dyna on windows when run from python subprocess
            command = f'mpiexec -wdir "{self.working_directory}" -localonly -np {ncpu} {self.solver} i={input_file} memory={mem}'  # noqa:E501
        elif self.mpi_option == MpiOption.MPP_MS_MPI:
            command = (
                f'mpiexec -wdir "{self.working_directory}" -c {ncpu} -aa {self.solver} i={input_file} memory={mem}'
            )
        return f"{script} && {command} > lsrun.out.txt 2>&1"
