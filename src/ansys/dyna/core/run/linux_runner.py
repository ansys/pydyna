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

"""Module for defining the PyDyna Linux runner."""

import os
from pathlib import Path
from typing import Optional

from ansys.tools.common.path import get_dyna_path, get_latest_ansys_installation
from ansys.tools.common.path.path import _get_unified_install_base_for_version

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision


class LinuxRunner(BaseRunner):
    """Linux implementation to Run LS-DYNA.

    Tested with a custom exutable and when LS-DYNA is
    installed as part of the unified Ansys installation.
    """

    def __init__(self, **kwargs):
        """Initialize LinuxRunner.

        Parameters
        ----------
        case_ids : list[int] or None
            If provided, appends CASE or CASE=... to the LS-DYNA command line for *CASE support.
        """
        super().__init__(**kwargs)
        self.executable = kwargs.get("executable", None)
        version = kwargs.get("version", None)
        self.activate_case = kwargs.get("activate_case", False)
        self.case_ids = kwargs.get("case_ids", None)
        self._find_solver(version, self.executable)

    def set_input(self, input_file: str, working_directory: str) -> None:
        """Set the input file and working directory for the run."""
        self.input_file = input_file
        self.working_directory = working_directory

    def _find_solver(self, version: Optional[int], executable: Optional[str]) -> None:
        """Determine the appropriate LS-DYNA solver executable path."""
        if executable:
            # Use user-provided executable path
            if not Path(executable).is_file():
                raise FileNotFoundError(f"LS-DYNA executable not found at: {executable}")
            self.solver = executable
            return

        # Check if solver is available from ansys-tools-path
        atp_dyna_path = get_dyna_path(find=True, allow_input=False)
        if atp_dyna_path:
            self.solver = atp_dyna_path
            return

        # Resolve from specified version or fallback to latest
        if version:
            install_loc, _ = _get_unified_install_base_for_version(version)
        else:
            _, install_loc = get_latest_ansys_installation()

        self.solver = str(Path(install_loc) / "ansys" / "bin" / "linx64" / self._get_exe_name())

    def _get_exe_name(self) -> str:
        exe_name = {
            (MpiOption.SMP, Precision.SINGLE): "lsdyna_sp.e",
            (MpiOption.SMP, Precision.DOUBLE): "lsdyna_dp.e",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "lsdyna_sp_mpp.e",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "lsdyna_dp_mpp.e",
        }[(self.mpi_option, self.precision)]
        return exe_name

    def run(self) -> None:
        """Run LS-DYNA using the specified configuration."""
        os.chdir(str(Path(self.working_directory).resolve()))
        # CASE option logic
        case_option = ""
        if self.activate_case:
            if self.case_ids and isinstance(self.case_ids, list) and self.case_ids:
                case_option = f"CASE={','.join(str(cid) for cid in self.case_ids)}"
            else:
                case_option = "CASE"
        if self.mpi_option == MpiOption.MPP_INTEL_MPI:
            args = f"mpirun -np {self.ncpu} {self.solver} i={self.input_file} memory={self.get_memory_string()} {case_option}"  # noqa: E501
            os.system(args)  # nosec: B605
        else:
            args = f"{self.solver} i={self.input_file} ncpu={self.ncpu} memory={self.get_memory_string()} {case_option}"  # noqa: E501
            os.system(args)  # nosec: B605
