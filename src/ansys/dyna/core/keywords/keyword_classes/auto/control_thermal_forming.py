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

class ControlThermalForming(KeywordBase):
    """DYNA CONTROL_THERMAL_FORMING keyword"""

    keyword = "CONTROL"
    subkeyword = "THERMAL_FORMING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "its",
                        float,
                        0,
                        10,
                        kwargs.get("its")
                    ),
                    Field(
                        "ptype",
                        int,
                        10,
                        10,
                        kwargs.get("ptype", 0)
                    ),
                    Field(
                        "tsf",
                        float,
                        20,
                        10,
                        kwargs.get("tsf", 1.0)
                    ),
                    Field(
                        "thshel",
                        int,
                        30,
                        10,
                        kwargs.get("thshel", 0)
                    ),
                    Field(
                        "ithoff",
                        int,
                        40,
                        10,
                        kwargs.get("ithoff", 0)
                    ),
                    Field(
                        "solver",
                        int,
                        50,
                        10,
                        kwargs.get("solver", 3)
                    ),
                    Field(
                        "fwork",
                        float,
                        60,
                        10,
                        kwargs.get("fwork", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k",
                        float,
                        0,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "frad",
                        float,
                        10,
                        10,
                        kwargs.get("frad")
                    ),
                    Field(
                        "h0",
                        float,
                        20,
                        10,
                        kwargs.get("h0")
                    ),
                    Field(
                        "lmin",
                        float,
                        30,
                        10,
                        kwargs.get("lmin")
                    ),
                    Field(
                        "lmax",
                        float,
                        40,
                        10,
                        kwargs.get("lmax")
                    ),
                    Field(
                        "ftoslv",
                        float,
                        50,
                        10,
                        kwargs.get("ftoslv", 0.5)
                    ),
                    Field(
                        "bc_flg",
                        int,
                        60,
                        10,
                        kwargs.get("bc_flg", 0)
                    ),
                    Field(
                        "algo",
                        int,
                        70,
                        10,
                        kwargs.get("algo", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcfst",
                        int,
                        0,
                        10,
                        kwargs.get("lcfst")
                    ),
                    Field(
                        "lcfdt",
                        int,
                        10,
                        10,
                        kwargs.get("lcfdt")
                    ),
                    Field(
                        "formula",
                        int,
                        20,
                        10,
                        kwargs.get("formula", 1)
                    ),
                    Field(
                        "a",
                        int,
                        30,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        int,
                        40,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "c",
                        int,
                        50,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "d",
                        int,
                        60,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "lch",
                        int,
                        70,
                        10,
                        kwargs.get("lch")
                    ),
                ],
            ),
        ]

    @property
    def its(self) -> typing.Optional[float]:
        """Get or set the Initial thermal time step size.
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: float) -> None:
        self._cards[0].set_value("its", value)

    @property
    def ptype(self) -> int:
        """Get or set the Thermal problem type (see Remark 1 for determining the type of problem):
        EQ.0: linear problem
        EQ.1: nonlinear problem with material properties evaluated at
        the temperature of the gauss point
        EQ.2: nonlinear problem with material properties evaluated at
        the average temperature of the element.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ptype must be one of {0,1,2}""")
        self._cards[0].set_value("ptype", value)

    @property
    def tsf(self) -> float:
        """Get or set the Thermal Speedup Factor. This factor multiplies all thermal
        parameters with units of time in the denominator (such as
        thermal conductivity and convection heat transfer coefficients). It
        is used to artificially scale the problem in time. For example, if
        the velocity of the stamping punch is artificially increased by
        1000, then set TSF = 1000 to scale the thermal parameters.
        """ # nopep8
        return self._cards[0].get_value("tsf")

    @tsf.setter
    def tsf(self, value: float) -> None:
        self._cards[0].set_value("tsf", value)

    @property
    def thshel(self) -> int:
        """Get or set the Thermal shell option:
        EQ.0: no temperature gradient is considered through the shell
        thickness.
        EQ.1: a temperature gradient is calculated through the shell	thickness.
        """ # nopep8
        return self._cards[0].get_value("thshel")

    @thshel.setter
    def thshel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""thshel must be one of {0,1}""")
        self._cards[0].set_value("thshel", value)

    @property
    def ithoff(self) -> int:
        """Get or set the Flag for offsetting thermal contact surfaces for thick thermal shells:
        EQ.0: no offset; if thickness is not included in the contact, the
        heat will be transferred between the mid-surfaces of the
        corresponding contact segments (shells).
        EQ.1: offsets are applied so that contact heat transfer is always
        between the outer surfaces of the contact segments (shells).
        """ # nopep8
        return self._cards[0].get_value("ithoff")

    @ithoff.setter
    def ithoff(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ithoff must be one of {0,1}""")
        self._cards[0].set_value("ithoff", value)

    @property
    def solver(self) -> int:
        """Get or set the Thermal analysis solver type (see *CONTROL_THERMAL_SOLVER).
        For SMP only:
        EQ.1: using solver 11 (enter -1 to use the old ACTCOL solver)
        EQ.2: nonsymmetric direct solver
        EQ.3: diagonal scaled conjugate gradient iterative (default)
        EQ.4: incomplete choleski conjugate gradient iterativeEQ.5: nonsymmetric diagonal scaled bi-conjugate gradient
        For SMP or MPP:
        EQ.11: direct solver
        EQ.12: diagonal scaling (default for MPP) conjugate gradient iterative
        EQ.13: symmetric Gauss-Siedel conjugate gradient iterative
        EQ.14: SSOR conjugate gradient iterative
        EQ.15: ILDLT0 (incomplete factorization) conjugate gradient iterative
        EQ.16: modified ILDLT0 (incomplete factorization) conjugate gradient iterative
        For Conjugate Heat transfer problems in SMP or MPP:
        EQ.17: GMRES solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: int) -> None:
        if value not in [3, 1, 2, 4, 5, 11, 12, 13, 14, 15, 16, 17]:
            raise Exception("""solver must be one of {3,1,2,4,5,11,12,13,14,15,16,17}""")
        self._cards[0].set_value("solver", value)

    @property
    def fwork(self) -> float:
        """Get or set the Fraction of mechanical work converted into heat
        """ # nopep8
        return self._cards[0].get_value("fwork")

    @fwork.setter
    def fwork(self, value: float) -> None:
        self._cards[0].set_value("fwork", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity of fluid between the contact surfaces.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def frad(self) -> typing.Optional[float]:
        """Get or set the Radiation factor between the contact surfaces.
        """ # nopep8
        return self._cards[1].get_value("frad")

    @frad.setter
    def frad(self, value: float) -> None:
        self._cards[1].set_value("frad", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Heat transfer conductance for closed gaps. Use this heat transfer
        conductance for gaps in the range.
        """ # nopep8
        return self._cards[1].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        self._cards[1].set_value("h0", value)

    @property
    def lmin(self) -> typing.Optional[float]:
        """Get or set the Minimum gap, 𝑙min; use the heat transfer conductance defined
        (H0) for gap thicknesses less than this value.
        LT.0.0: -LMIN is a load curve ID defining 𝑙min as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        self._cards[1].set_value("lmin", value)

    @property
    def lmax(self) -> typing.Optional[float]:
        """Get or set the No thermal contact if gap is greater than this value (..max).
        """ # nopep8
        return self._cards[1].get_value("lmax")

    @lmax.setter
    def lmax(self, value: float) -> None:
        self._cards[1].set_value("lmax", value)

    @property
    def ftoslv(self) -> float:
        """Get or set the Fraction, .. , of sliding friction energy partitioned to the slave
        surface. Energy partitioned to the master surface is (1 . .. ).EQ.0: Default set to 0.5: The sliding friction energy is
        partitioned 50% - 50% to the slave and master surfaces in contact.
        """ # nopep8
        return self._cards[1].get_value("ftoslv")

    @ftoslv.setter
    def ftoslv(self, value: float) -> None:
        self._cards[1].set_value("ftoslv", value)

    @property
    def bc_flg(self) -> int:
        """Get or set the Thermal boundary condition flag:
        EQ.0: thermal boundary conditions are on when parts are in contact.t
        EQ.1: thermal boundary conditions are off when parts are in contact.
        """ # nopep8
        return self._cards[1].get_value("bc_flg")

    @bc_flg.setter
    def bc_flg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""bc_flg must be one of {0,1}""")
        self._cards[1].set_value("bc_flg", value)

    @property
    def algo(self) -> int:
        """Get or set the Contact algorithm type.
        EQ.0: two way contact; both surfaces change temperature due to contact.
        EQ.1: one way contact; master surface does not change
        temperature due to contact. Slave surface does change temperature
        """ # nopep8
        return self._cards[1].get_value("algo")

    @algo.setter
    def algo(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""algo must be one of {0,1}""")
        self._cards[1].set_value("algo", value)

    @property
    def lcfst(self) -> typing.Optional[int]:
        """Get or set the Load curve number for static coefficient of friction as a function of
        temperature. The load curve value multiplies the coefficient value FS
        """ # nopep8
        return self._cards[2].get_value("lcfst")

    @lcfst.setter
    def lcfst(self, value: int) -> None:
        self._cards[2].set_value("lcfst", value)

    @property
    def lcfdt(self) -> typing.Optional[int]:
        """Get or set the Load curve number for dynamic coefficient of friction as a
        function of temperature. The load curve value multiplies the coefficient value FD.
        """ # nopep8
        return self._cards[2].get_value("lcfdt")

    @lcfdt.setter
    def lcfdt(self, value: int) -> None:
        self._cards[2].set_value("lcfdt", value)

    @property
    def formula(self) -> int:
        """Get or set the Formula that defines the contact heat conductance as a function of
        temperature and pressure. See the manual
        This is equivalent to defining the keyword *USER_INTERFACE_CONDUCTIVITY. The user subroutine usrhcon will be called for this contact interface to define the contact heat transfer coefficient
        """ # nopep8
        return self._cards[2].get_value("formula")

    @formula.setter
    def formula(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""formula must be one of {1,2,3,4,5}""")
        self._cards[2].set_value("formula", value)

    @property
    def a(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the a coefficient used in the formula.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: int) -> None:
        self._cards[2].set_value("a", value)

    @property
    def b(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the b coefficient used in the formula
        """ # nopep8
        return self._cards[2].get_value("b")

    @b.setter
    def b(self, value: int) -> None:
        self._cards[2].set_value("b", value)

    @property
    def c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the c coefficient used in the formula
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: int) -> None:
        self._cards[2].set_value("c", value)

    @property
    def d(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the d coefficient used in the formula.
        """ # nopep8
        return self._cards[2].get_value("d")

    @d.setter
    def d(self, value: int) -> None:
        self._cards[2].set_value("d", value)

    @property
    def lch(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for h. This parameter can refer to a curve ID (see
        *DEFINE_CURVE) or a function ID (see *DEFINE_FUNCTION).
        When LCH is a curve ID (and a function ID) it is interpreted as follows:
        GT.0: the heat transfer coefficient is defined as a function of
        time, 𝑡, by a curve consisting of (𝑡, ℎ(𝑡)) data pairs.
        LT.0: the heat transfer coefficient is defined as a function of
        temperature, 𝑇, by a curve consisting of (𝑇, ℎ(𝑇)) data pairs
        """ # nopep8
        return self._cards[2].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        self._cards[2].set_value("lch", value)

