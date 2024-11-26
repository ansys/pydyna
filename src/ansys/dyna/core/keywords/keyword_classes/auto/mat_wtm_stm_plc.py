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

class MatWtmStmPlc(KeywordBase):
    """DYNA MAT_WTM_STM_PLC keyword"""

    keyword = "MAT"
    subkeyword = "WTM_STM_PLC"
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
                        "numfi",
                        float,
                        40,
                        10,
                        kwargs.get("numfi")
                    ),
                    Field(
                        "npsc",
                        float,
                        50,
                        10,
                        kwargs.get("npsc")
                    ),
                    Field(
                        "wc",
                        float,
                        60,
                        10,
                        kwargs.get("wc")
                    ),
                    Field(
                        "tauc",
                        float,
                        70,
                        10,
                        kwargs.get("tauc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigma0",
                        float,
                        0,
                        10,
                        kwargs.get("sigma0")
                    ),
                    Field(
                        "qr1",
                        float,
                        10,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        20,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        30,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        40,
                        10,
                        kwargs.get("cr2")
                    ),
                    Field(
                        "k",
                        float,
                        50,
                        10,
                        kwargs.get("k")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a1",
                        float,
                        0,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        10,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        20,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "a4",
                        float,
                        30,
                        10,
                        kwargs.get("a4")
                    ),
                    Field(
                        "a5",
                        float,
                        40,
                        10,
                        kwargs.get("a5")
                    ),
                    Field(
                        "a6",
                        float,
                        50,
                        10,
                        kwargs.get("a6")
                    ),
                    Field(
                        "a7",
                        float,
                        60,
                        10,
                        kwargs.get("a7")
                    ),
                    Field(
                        "a8",
                        float,
                        70,
                        10,
                        kwargs.get("a8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s",
                        float,
                        0,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "h",
                        float,
                        10,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "omega",
                        float,
                        20,
                        10,
                        kwargs.get("omega")
                    ),
                    Field(
                        "td",
                        float,
                        30,
                        10,
                        kwargs.get("td")
                    ),
                    Field(
                        "alpha",
                        float,
                        40,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "eps0",
                        float,
                        50,
                        10,
                        kwargs.get("eps0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
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
                ],
            ),
            OptionCardSet(
                option_spec = MatWtmStmPlc.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def numfi(self) -> typing.Optional[float]:
        """Get or set the Number of through thickness integration points that must fail before the element is deleted (remember to change this number if switching between full and reduced integration type of elements).
        """ # nopep8
        return self._cards[0].get_value("numfi")

    @numfi.setter
    def numfi(self, value: float) -> None:
        self._cards[0].set_value("numfi", value)

    @property
    def npsc(self) -> typing.Optional[float]:
        """Get or set the Critical value   of the plastic thickness strain (used in the CTS fracture criterion).
        """ # nopep8
        return self._cards[0].get_value("npsc")

    @npsc.setter
    def npsc(self, value: float) -> None:
        self._cards[0].set_value("npsc", value)

    @property
    def wc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Cockcroft-Latham fracture criterion
        """ # nopep8
        return self._cards[0].get_value("wc")

    @wc.setter
    def wc(self, value: float) -> None:
        self._cards[0].set_value("wc", value)

    @property
    def tauc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Bressan-Williams shear fracture criterion
        """ # nopep8
        return self._cards[0].get_value("tauc")

    @tauc.setter
    def tauc(self, value: float) -> None:
        self._cards[0].set_value("tauc", value)

    @property
    def sigma0(self) -> typing.Optional[float]:
        """Get or set the Initial mean value of yield stress  .
        """ # nopep8
        return self._cards[1].get_value("sigma0")

    @sigma0.setter
    def sigma0(self, value: float) -> None:
        self._cards[1].set_value("sigma0", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter .
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[1].set_value("cr2", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the equals half YLD2003 exponent  .  Recommended value for FCC materials is  , i.e.  .
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter  .
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter .
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        self._cards[2].set_value("a4", value)

    @property
    def a5(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        self._cards[2].set_value("a5", value)

    @property
    def a6(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a6")

    @a6.setter
    def a6(self, value: float) -> None:
        self._cards[2].set_value("a6", value)

    @property
    def a7(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a7")

    @a7.setter
    def a7(self, value: float) -> None:
        self._cards[2].set_value("a7", value)

    @property
    def a8(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a8")

    @a8.setter
    def a8(self, value: float) -> None:
        self._cards[2].set_value("a8", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter, S.
        """ # nopep8
        return self._cards[3].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[3].set_value("s", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter, H.
        """ # nopep8
        return self._cards[3].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[3].set_value("h", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[3].set_value("omega", value)

    @property
    def td(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("td")

    @td.setter
    def td(self, value: float) -> None:
        self._cards[3].set_value("td", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[3].set_value("alpha", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[3].set_value("eps0", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0:  Locally orthotropic with material axes determined by element nodes as shown in Figure M2-1, and then rotated
        about the shell element normal by the angle BETA.	Nodes 1, 2 and 4 of an element are identical to the nodes
        used for the definition of a coordinate system as by *DEFINE_COORDINATE_NODES..
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[4].set_value("aopt", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO..
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[6].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

