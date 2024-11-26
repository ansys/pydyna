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

class MatBarlatYld96(KeywordBase):
    """DYNA MAT_BARLAT_YLD96 keyword"""

    keyword = "MAT"
    subkeyword = "BARLAT_YLD96"
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
                        "k",
                        float,
                        40,
                        10,
                        kwargs.get("k")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e0",
                        float,
                        0,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "n",
                        float,
                        10,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "esr0",
                        float,
                        20,
                        10,
                        kwargs.get("esr0")
                    ),
                    Field(
                        "m",
                        float,
                        30,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "hard",
                        int,
                        40,
                        10,
                        kwargs.get("hard", 1)
                    ),
                    Field(
                        "a",
                        float,
                        50,
                        10,
                        kwargs.get("a")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        30,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "ax",
                        float,
                        40,
                        10,
                        kwargs.get("ax")
                    ),
                    Field(
                        "ay",
                        float,
                        50,
                        10,
                        kwargs.get("ay")
                    ),
                    Field(
                        "az0",
                        float,
                        60,
                        10,
                        kwargs.get("az0")
                    ),
                    Field(
                        "az1",
                        float,
                        70,
                        10,
                        kwargs.get("az1")
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
                        "offang",
                        float,
                        10,
                        10,
                        kwargs.get("offang")
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
                option_spec = MatBarlatYld96.option_specs[0],
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
    def k(self) -> typing.Optional[float]:
        """Get or set the k, strength coefficient or alpha in Voce.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the epsilon-0, strain corresponding to the initial yield or b in Voce.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the n, hardening exponent for yield strength or c in Voce.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def esr0(self) -> typing.Optional[float]:
        """Get or set the epsilon-SR0, in powerlaw rate sensitivity.
        """ # nopep8
        return self._cards[1].get_value("esr0")

    @esr0.setter
    def esr0(self, value: float) -> None:
        self._cards[1].set_value("esr0", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the m, exponent for strain rate effects.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def hard(self) -> int:
        """Get or set the Hardening option:
        LT.0: absolute value defines the load curve ID.
        EQ.1: powerlaw (default)
        EQ.2: Voce
        """ # nopep8
        return self._cards[1].get_value("hard")

    @hard.setter
    def hard(self, value: int) -> None:
        self._cards[1].set_value("hard", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Flow potential exponent.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Coefficient c1. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Coefficient c2. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[2].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Coefficient c3. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[2].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Coefficient c4. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[2].set_value("c4", value)

    @property
    def ax(self) -> typing.Optional[float]:
        """Get or set the Coefficient ax. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        self._cards[2].set_value("ax", value)

    @property
    def ay(self) -> typing.Optional[float]:
        """Get or set the Coefficient ay. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        self._cards[2].set_value("ay", value)

    @property
    def az0(self) -> typing.Optional[float]:
        """Get or set the Coefficient az0. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("az0")

    @az0.setter
    def az0(self, value: float) -> None:
        self._cards[2].set_value("az0", value)

    @property
    def az1(self) -> typing.Optional[float]:
        """Get or set the Coefficient az1. For further eplanation please see keyword manual page 137 (volume two).
        """ # nopep8
        return self._cards[2].get_value("az1")

    @az1.setter
    def az1(self, value: float) -> None:
        self._cards[2].set_value("az1", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.  0.0: locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0:  locally orthotropic material axes determined by offsetting
        the material axes by an angle, BETA, from a line determined by taking the cross product of the element.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR).
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def offang(self) -> typing.Optional[float]:
        """Get or set the Offset angle for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("offang")

    @offang.setter
    def offang(self, value: float) -> None:
        self._cards[3].set_value("offang", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
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
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

