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

class Mat1222D(KeywordBase):
    """DYNA MAT_122_2D keyword"""

    keyword = "MAT"
    subkeyword = "122_2D"
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
                        "ex",
                        float,
                        20,
                        10,
                        kwargs.get("ex")
                    ),
                    Field(
                        "ey",
                        float,
                        30,
                        10,
                        kwargs.get("ey")
                    ),
                    Field(
                        "ez",
                        float,
                        40,
                        10,
                        kwargs.get("ez")
                    ),
                    Field(
                        "prxy",
                        float,
                        50,
                        10,
                        kwargs.get("prxy")
                    ),
                    Field(
                        "pryz",
                        float,
                        60,
                        10,
                        kwargs.get("pryz")
                    ),
                    Field(
                        "prxz",
                        float,
                        70,
                        10,
                        kwargs.get("prxz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gxy",
                        float,
                        0,
                        10,
                        kwargs.get("gxy")
                    ),
                    Field(
                        "gyz",
                        float,
                        10,
                        10,
                        kwargs.get("gyz")
                    ),
                    Field(
                        "gxz",
                        float,
                        20,
                        10,
                        kwargs.get("gxz")
                    ),
                    Field(
                        "f",
                        float,
                        30,
                        10,
                        kwargs.get("f")
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "h",
                        float,
                        50,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "l",
                        float,
                        60,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "m",
                        float,
                        70,
                        10,
                        kwargs.get("m")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n",
                        float,
                        0,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "hr",
                        int,
                        10,
                        10,
                        kwargs.get("hr", 1)
                    ),
                    Field(
                        "p1",
                        float,
                        20,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        30,
                        10,
                        kwargs.get("p2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        int,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat1222D.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
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
    def ex(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in x, y, and z directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ex")

    @ex.setter
    def ex(self, value: float) -> None:
        self._cards[0].set_value("ex", value)

    @property
    def ey(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in x, y, and z directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ey")

    @ey.setter
    def ey(self, value: float) -> None:
        self._cards[0].set_value("ey", value)

    @property
    def ez(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in x, y, and z directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ez")

    @ez.setter
    def ez(self, value: float) -> None:
        self._cards[0].set_value("ez", value)

    @property
    def prxy(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ν, in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("prxy")

    @prxy.setter
    def prxy(self, value: float) -> None:
        self._cards[0].set_value("prxy", value)

    @property
    def pryz(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ν, in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("pryz")

    @pryz.setter
    def pryz(self, value: float) -> None:
        self._cards[0].set_value("pryz", value)

    @property
    def prxz(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ν, in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("prxz")

    @prxz.setter
    def prxz(self, value: float) -> None:
        self._cards[0].set_value("prxz", value)

    @property
    def gxy(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("gxy")

    @gxy.setter
    def gxy(self, value: float) -> None:
        self._cards[1].set_value("gxy", value)

    @property
    def gyz(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("gyz")

    @gyz.setter
    def gyz(self, value: float) -> None:
        self._cards[1].set_value("gyz", value)

    @property
    def gxz(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in xy, yz and xz directions, respectively. Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("gxz")

    @gxz.setter
    def gxz(self, value: float) -> None:
        self._cards[1].set_value("gxz", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        self._cards[1].set_value("f", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[1].set_value("g", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[1].set_value("h", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[1].set_value("l", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Material constants in Hill's 1948 yield criterion (see Remarks). Negative values indicate (positive) curve numbers, where each curve is a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[2].set_value("n", value)

    @property
    def hr(self) -> int:
        """Get or set the Hardening rule:
        EQ.1:	Stress-strain relationship is defined by load curve or 2D-table ID with parameter P1. P2 is ignored.
        EQ.2:	Stress-strain relationship is defined by strength coefficient K (P1) and strain hardening coefficient n (P2), as in Swift's exponential hardening equation: σ_yield =k(ε+0.01)^n..
        """ # nopep8
        return self._cards[2].get_value("hr")

    @hr.setter
    def hr(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""hr must be one of {1,2}""")
        self._cards[2].set_value("hr", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1:	Load curve or 2D-table ID defining stress-strain curve. If 2D-table ID, the table gives stress-strain curves for different temperatures.
        HR.EQ.2:	k, strength coefficient in σ_yield =k(ε+0.01)^n.
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[2].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1:	not used.
        HR.EQ.2.0:	n, the exponent in σ_yield=k(ε+0.01)^n.
        """ # nopep8
        return self._cards[2].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[2].set_value("p2", value)

    @property
    def aopt(self) -> typing.Optional[int]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

