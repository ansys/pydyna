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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatTrip(KeywordBase):
    """DYNA MAT_TRIP keyword"""

    keyword = "MAT"
    subkeyword = "TRIP"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "cp",
                        float,
                        40,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "t0",
                        float,
                        50,
                        10,
                        kwargs.get("t0")
                    ),
                    Field(
                        "tref",
                        float,
                        60,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "ta0",
                        float,
                        70,
                        10,
                        kwargs.get("ta0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        10,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "c",
                        float,
                        20,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "d",
                        float,
                        30,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "p",
                        float,
                        40,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "q",
                        float,
                        50,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "e0mart",
                        float,
                        60,
                        10,
                        kwargs.get("e0mart")
                    ),
                    Field(
                        "vm0",
                        float,
                        70,
                        10,
                        kwargs.get("vm0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ahs",
                        float,
                        0,
                        10,
                        kwargs.get("ahs")
                    ),
                    Field(
                        "bhs",
                        float,
                        10,
                        10,
                        kwargs.get("bhs")
                    ),
                    Field(
                        "m",
                        float,
                        20,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "eps0",
                        float,
                        40,
                        10,
                        kwargs.get("eps0")
                    ),
                    Field(
                        "hmart",
                        float,
                        50,
                        10,
                        kwargs.get("hmart")
                    ),
                    Field(
                        "k1",
                        float,
                        60,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "k2",
                        float,
                        70,
                        10,
                        kwargs.get("k2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatTrip.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Adiabatic temperature calculation option:
        EQ. 0.0 Adiabatic temperature calculation is disabled.
        GT. 0.0 CP is the specific heat Cp. Adiabatic temperature calculation is enabled
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        self._cards[0].set_value("t0", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for output of the yield stress as history variable 1.
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[0].set_value("tref", value)

    @property
    def ta0(self) -> typing.Optional[float]:
        """Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
        """ # nopep8
        return self._cards[0].get_value("ta0")

    @ta0.setter
    def ta0(self, value: float) -> None:
        self._cards[0].set_value("ta0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter A, .
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter B,
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter C,
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter D, .
        """ # nopep8
        return self._cards[1].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[1].set_value("d", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter p,
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[1].set_value("p", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter q,
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[1].set_value("q", value)

    @property
    def e0mart(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter Eomart,
        """ # nopep8
        return self._cards[1].get_value("e0mart")

    @e0mart.setter
    def e0mart(self, value: float) -> None:
        self._cards[1].set_value("e0mart", value)

    @property
    def vm0(self) -> typing.Optional[float]:
        """Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
        GT.0.0: Vm0 is set to VM0.
        LT.0.0: Can be used only when there are initial plastic strains  p present, e.g. when using *INITIAL_STRESS_SHELL. The absolute value of VM0 is then the load curve ID for a function f that sets . The function f must be a monotonically nondecreasing function of
        """ # nopep8
        return self._cards[1].get_value("vm0")

    @vm0.setter
    def vm0(self, value: float) -> None:
        self._cards[1].set_value("vm0", value)

    @property
    def ahs(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter AHS.
        """ # nopep8
        return self._cards[2].get_value("ahs")

    @ahs.setter
    def ahs(self, value: float) -> None:
        self._cards[2].set_value("ahs", value)

    @property
    def bhs(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter BHS
        """ # nopep8
        return self._cards[2].get_value("bhs")

    @bhs.setter
    def bhs(self, value: float) -> None:
        self._cards[2].set_value("bhs", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter M
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[2].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter N.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[2].set_value("n", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter EPSO
        """ # nopep8
        return self._cards[2].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[2].set_value("eps0", value)

    @property
    def hmart(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter HMART
        """ # nopep8
        return self._cards[2].get_value("hmart")

    @hmart.setter
    def hmart(self, value: float) -> None:
        self._cards[2].set_value("hmart", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter K1
        """ # nopep8
        return self._cards[2].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[2].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter K2
        """ # nopep8
        return self._cards[2].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[2].set_value("k2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

