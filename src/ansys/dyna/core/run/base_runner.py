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

"""Base runner class."""

from ansys.dyna.core.run.options import MemoryUnit, MpiOption, Precision


class BaseRunner:
    """Base class to Run LS-DYNA."""

    def __init__(self, **kwargs):
        """Initialize BaseRunner.

        Parameters
        ----------
        mpi_option : MpiOption
            MPI option to use for running LS-DYNA.
        ncpu : int
            Number of CPUs to use for running LS-DYNA.
        memory : int
            Memory amount to allocate for running LS-DYNA.
        memory_unit : MemoryUnit
            Memory unit to use for running LS-DYNA.
        precision : Precision
            Precision to use for running LS-DYNA.
        """
        # TODO - split mpi option into precision?
        self.mpi_option = kwargs.get("mpi_option", MpiOption.SMP)
        self.ncpu = kwargs.get("ncpu", 1)
        self.memory = kwargs.get("memory", 20)
        self.memory_unit = kwargs.get("memory_unit", MemoryUnit.MB)
        self.precision = kwargs.get("precision", Precision.DOUBLE)

    def get_memory_string(self) -> str:
        """Return memory string for the runner."""
        unit = {MemoryUnit.MB: "m", MemoryUnit.GB: "G"}[self.memory_unit]
        return f"{self.memory}{unit}"
