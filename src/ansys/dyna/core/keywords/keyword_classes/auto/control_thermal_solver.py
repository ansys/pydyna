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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlThermalSolver(KeywordBase):
    """DYNA CONTROL_THERMAL_SOLVER keyword"""

    keyword = "CONTROL"
    subkeyword = "THERMAL_SOLVER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "atype",
                        int,
                        0,
                        10,
                        kwargs.get("atype", 0)
                    ),
                    Field(
                        "ptype",
                        int,
                        10,
                        10,
                        kwargs.get("ptype", 0)
                    ),
                    Field(
                        "solver",
                        int,
                        20,
                        10,
                        kwargs.get("solver", 11)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "gpt",
                        int,
                        40,
                        10,
                        kwargs.get("gpt", 8)
                    ),
                    Field(
                        "eqheat",
                        float,
                        50,
                        10,
                        kwargs.get("eqheat", 1.0)
                    ),
                    Field(
                        "fwork",
                        float,
                        60,
                        10,
                        kwargs.get("fwork", 1.0)
                    ),
                    Field(
                        "sbc",
                        float,
                        70,
                        10,
                        kwargs.get("sbc", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "msglvl",
                        int,
                        0,
                        10,
                        kwargs.get("msglvl", 0)
                    ),
                    Field(
                        "maxitr",
                        int,
                        10,
                        10,
                        kwargs.get("maxitr", 500)
                    ),
                    Field(
                        "abstol",
                        float,
                        20,
                        10,
                        kwargs.get("abstol", 1.0e-10)
                    ),
                    Field(
                        "reltol",
                        float,
                        30,
                        10,
                        kwargs.get("reltol", 1.0E-06)
                    ),
                    Field(
                        "omega",
                        float,
                        40,
                        10,
                        kwargs.get("omega", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tsf",
                        float,
                        70,
                        10,
                        kwargs.get("tsf", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mxdmp",
                        int,
                        0,
                        10,
                        kwargs.get("mxdmp", 0)
                    ),
                    Field(
                        "dtvf",
                        float,
                        10,
                        10,
                        kwargs.get("dtvf", 0)
                    ),
                    Field(
                        "varden",
                        int,
                        20,
                        10,
                        kwargs.get("varden", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ncycl",
                        int,
                        40,
                        10,
                        kwargs.get("ncycl", 1)
                    ),
                ],
            ),
        ]

    @property
    def atype(self) -> int:
        """Get or set the Thermal analysis type:
        EQ.0: Steady state analysis,
        EQ.1: transient analysis.
        """ # nopep8
        return self._cards[0].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""atype must be one of {0,1}""")
        self._cards[0].set_value("atype", value)

    @property
    def ptype(self) -> int:
        """Get or set the Thermal problem type: (see *CONTROL_THERMAL_NONLINEAR if no-zero)
        EQ.0: linear problem,
        EQ.1: nonlinear problem with material properties evaluated at gauss point temperature,
        EQ.2: nonlinear problem with material properties evaluated at element average temperature.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ptype must be one of {0,1,2}""")
        self._cards[0].set_value("ptype", value)

    @property
    def solver(self) -> int:
        """Get or set the Thermal analysis solver type (see Remarks 1 and 2):
        EQ.11:	Direct solver
        EQ.12 : Diagonal scaling(default for MPP) conjugate gradient iterative
        EQ.13 : Symmetric Gauss - Seidel conjugate gradient iterative
        EQ.14 : SSOR conjugate gradient iterative
        EQ.15 : ILDLT0(incomplete factorization) conjugate gradient iterative
        EQ.16 : Modified ILDLT0(incomplete factorization) conjugate gradient iterative
        EQ.17 : GMRES solver for conjugate heat transfer problems
        EQ.18 : ILDLT(T) (incomplete factorization with threshold pivoting)
        EQ.19 : Preconditioned conjugate gradient with MUMPS(see Remark Error!Reference source not found.in * CONTROL_IMPlICIT_SOLVER)
        EQ.30 : Direct nonsymmetric factorization
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: int) -> None:
        if value not in [11, 12, 13, 14, 15, 16, 17, 18, 19, 30]:
            raise Exception("""solver must be one of {11,12,13,14,15,16,17,18,19,30}""")
        self._cards[0].set_value("solver", value)

    @property
    def gpt(self) -> int:
        """Get or set the Number of Gauss points to be used in the solid elements:
        EQ.8: Use default is set to 8,
        EQ. 1: one point quadrature is used.
        """ # nopep8
        return self._cards[0].get_value("gpt")

    @gpt.setter
    def gpt(self, value: int) -> None:
        self._cards[0].set_value("gpt", value)

    @property
    def eqheat(self) -> float:
        """Get or set the Mechanical equivalent of heat (default set to 1.0).
        """ # nopep8
        return self._cards[0].get_value("eqheat")

    @eqheat.setter
    def eqheat(self, value: float) -> None:
        self._cards[0].set_value("eqheat", value)

    @property
    def fwork(self) -> float:
        """Get or set the Fraction of mechnical work converted into heat (default set to 1.0).
        """ # nopep8
        return self._cards[0].get_value("fwork")

    @fwork.setter
    def fwork(self, value: float) -> None:
        self._cards[0].set_value("fwork", value)

    @property
    def sbc(self) -> float:
        """Get or set the Stefan Boltzmann constant. Value is used with enclosure radiation surfaces, see *BOUNDARY_RADIATION_...
        """ # nopep8
        return self._cards[0].get_value("sbc")

    @sbc.setter
    def sbc(self, value: float) -> None:
        self._cards[0].set_value("sbc", value)

    @property
    def msglvl(self) -> int:
        """Get or set the Output message level  (For SOLVER > 10)
        EQ.0:no output (default),
        EQ.1:summary information,
        EQ.2:detailed information, use only for debugging.
        """ # nopep8
        return self._cards[1].get_value("msglvl")

    @msglvl.setter
    def msglvl(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""msglvl must be one of {0,1,2}""")
        self._cards[1].set_value("msglvl", value)

    @property
    def maxitr(self) -> int:
        """Get or set the Maximum number of iterations.  For SOLVER >11.
        EQ.0:use default value 500.
        """ # nopep8
        return self._cards[1].get_value("maxitr")

    @maxitr.setter
    def maxitr(self, value: int) -> None:
        self._cards[1].set_value("maxitr", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute convergence tolerance.  For SOLVER >11.
        EQ.0.0:use default value 1.e-10.
        """ # nopep8
        return self._cards[1].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        self._cards[1].set_value("abstol", value)

    @property
    def reltol(self) -> float:
        """Get or set the Relative convergence tolerance.  Replaces CGTOL for SOLVER >11.
        EQ.0.0:use default value 1.e-06.
        """ # nopep8
        return self._cards[1].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        self._cards[1].set_value("reltol", value)

    @property
    def omega(self) -> float:
        """Get or set the Relaxation parameter omega for SOLVER 14 and 16.
        EQ.0.0:use default value 1.0 for Solver 14, use default value 0.0 for Solver 16..
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[1].set_value("omega", value)

    @property
    def tsf(self) -> float:
        """Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters with units of time in the denominator (e.g., thermal conductivity, convection heat transfer coefficients). It is used to artificially time scale the problem.
        EQ.0.0:	Default value 1.0,
        LT.0.0 : | TSF | is a load curve ID.Curve defines speedup factor as a function of time.
        Its main use is in metal stamping.If the velocity of the stamping punch is artificially increased by 1000, then set TSF = 1000 to scale the thermal parameters.
        """ # nopep8
        return self._cards[1].get_value("tsf")

    @tsf.setter
    def tsf(self, value: float) -> None:
        self._cards[1].set_value("tsf", value)

    @property
    def mxdmp(self) -> int:
        """Get or set the Matrix Dumping for SOLVER > 11
        EQ.0:	No Dumping
        GT.0:	Dump using ASCII format every MXDMP time steps.
        LT.0:	Dump using binary format every |MXDMP| time steps.
        """ # nopep8
        return self._cards[2].get_value("mxdmp")

    @mxdmp.setter
    def mxdmp(self, value: int) -> None:
        self._cards[2].set_value("mxdmp", value)

    @property
    def dtvf(self) -> float:
        """Get or set the Time interval between view factor updates.
        """ # nopep8
        return self._cards[2].get_value("dtvf")

    @dtvf.setter
    def dtvf(self, value: float) -> None:
        self._cards[2].set_value("dtvf", value)

    @property
    def varden(self) -> int:
        """Get or set the Variable thermal density for solid elements in a coupled thermal - structural analysis.Setting VARDEN to 1 or 2 will adjust the material density in the thermal solver to account for changes in element volume, for example, due to material compaction, thermal expansion, etc.In applications where volume changes are small, the default is recommended.
        EQ.0:	Thermal density remains constant and equal to TRO as given in * MAT_THERMAL_option(default).
        EQ.1 : Thermal density varies to account for change in volume.If an equation of state(*EOS) is used, the initial internal energy specified therein is taken into account.
        EQ.2 : Thermal density varies to account for change in volume.The initial internal energy is not considered.
        """ # nopep8
        return self._cards[2].get_value("varden")

    @varden.setter
    def varden(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""varden must be one of {0,1,2}""")
        self._cards[2].set_value("varden", value)

    @property
    def ncycl(self) -> int:
        """Get or set the Thermal matrix reassembly frequency. Default is at every thermal cycle.
        """ # nopep8
        return self._cards[2].get_value("ncycl")

    @ncycl.setter
    def ncycl(self, value: int) -> None:
        self._cards[2].set_value("ncycl", value)

