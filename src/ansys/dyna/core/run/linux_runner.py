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

import os

from ansys.tools.path import get_dyna_path, get_latest_ansys_installation
from ansys.tools.path.path import _get_unified_install_base_for_version

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision


class LinuxRunner(BaseRunner):
    """Linux implementation to Run LS-DYNA. Tested with a custom exutable
    and when LS-DYNA is installed as part of the unified Ansys installation
    """

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.executable = kwargs.get("executable", None)
        version = kwargs.get("version", None)
        self._find_solver(version, self.executable)

    def set_input(self, input_file: str, working_directory: str) -> None:
        self.input_file = input_file
        self.working_directory = working_directory

    def _find_solver(self, version: int, executable: str) -> None:
        atp_dyna_path = get_dyna_path(find=False, allow_input=False)
        if executable is not None:
            # User passed in executable directly. Use that.
            if os.path.isfile(executable):
                self.solver = executable
        elif atp_dyna_path is not None:
            # User stored dyna solver in ansys-tools-path, use that
            self.solver = atp_dyna_path
        elif version is not None:
            # User passed in the version, compute the path to the dyna solver from the
            # unified installation
            # of that version
            install_loc, _ = _get_unified_install_base_for_version(version)
            self.solver = os.path.join(install_loc, "ansys", "bin", "linx64", self._get_exe_name())
        else:
            # User passed nothing, find the dyna solver from the latest unified installation
            install_loc, _ = get_latest_ansys_installation()
            self.solver = os.path.join(install_loc, "ansys", "bin", "linx64", self._get_exe_name())

    def _get_exe_name(self) -> str:
        exe_name = {
            (MpiOption.SMP, Precision.SINGLE): "lsdyna_sp.e",
            (MpiOption.SMP, Precision.DOUBLE): "lsdyna_dp.e",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "lsdyna_sp_mpp.e",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "lsdyna_dp_mpp.e",
        }[(self.mpi_option, self.precision)]
        return exe_name

    def run(self) -> None:
        os.chdir(self.working_directory)
        if self.mpi_option == MpiOption.MPP_INTEL_MPI:
            os.system(
                f"mpirun -np {self.ncpu} {self.solver} i={self.input_file} memory={self.get_memory_string()}"  # noqa: E501
            )
        else:
            os.system(
                f"{self.solver} i={self.input_file} ncpu={self.ncpu} memory={self.get_memory_string()}"  # noqa: E501
            )
