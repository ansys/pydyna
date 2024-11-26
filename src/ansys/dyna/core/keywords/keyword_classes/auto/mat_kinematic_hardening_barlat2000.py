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

class MatKinematicHardeningBarlat2000(KeywordBase):
    """DYNA MAT_KINEMATIC_HARDENING_BARLAT2000 keyword"""

    keyword = "MAT"
    subkeyword = "KINEMATIC_HARDENING_BARLAT2000"
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
                        "ea",
                        float,
                        40,
                        10,
                        kwargs.get("ea")
                    ),
                    Field(
                        "coe",
                        float,
                        50,
                        10,
                        kwargs.get("coe")
                    ),
                    Field(
                        "m",
                        float,
                        60,
                        10,
                        kwargs.get("m")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "alpha3",
                        float,
                        20,
                        10,
                        kwargs.get("alpha3")
                    ),
                    Field(
                        "alpha4",
                        float,
                        30,
                        10,
                        kwargs.get("alpha4")
                    ),
                    Field(
                        "alpha5",
                        float,
                        40,
                        10,
                        kwargs.get("alpha5")
                    ),
                    Field(
                        "alpha6",
                        float,
                        50,
                        10,
                        kwargs.get("alpha6")
                    ),
                    Field(
                        "alpha7",
                        float,
                        60,
                        10,
                        kwargs.get("alpha7")
                    ),
                    Field(
                        "alpha8",
                        float,
                        70,
                        10,
                        kwargs.get("alpha8")
                    ),
                ],
            ),
            Card(
                [
                ],
            ),
            Card(
                [
                ],
            ),
            Card(
                [
                    Field(
                        "cb",
                        float,
                        0,
                        10,
                        kwargs.get("cb")
                    ),
                    Field(
                        "y",
                        float,
                        10,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "sc1",
                        float,
                        20,
                        10,
                        kwargs.get("sc1")
                    ),
                    Field(
                        "k",
                        float,
                        30,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "rsat",
                        float,
                        40,
                        10,
                        kwargs.get("rsat")
                    ),
                    Field(
                        "sb",
                        float,
                        50,
                        10,
                        kwargs.get("sb")
                    ),
                    Field(
                        "h",
                        float,
                        60,
                        10,
                        kwargs.get("h")
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
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "iopt",
                        int,
                        20,
                        10,
                        kwargs.get("iopt", 0)
                    ),
                    Field(
                        "c1",
                        float,
                        30,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        40,
                        10,
                        kwargs.get("c2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
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
                option_spec = MatKinematicHardeningBarlat2000.option_specs[0],
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
        """Get or set the Material identification. A unique number must be specified.
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
        """Get or set the Young's Modulus.
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
    def ea(self) -> typing.Optional[float]:
        """Get or set the parameter controlling the change of Young’s modulus;
        LT.0.0:	|EA| is a curve ID giving the change of Young’s modulus as a function of effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def coe(self) -> typing.Optional[float]:
        """Get or set the parameter controlling the change of Young’s modulus; see the remarks of *MAT_125.
        """ # nopep8
        return self._cards[0].get_value("coe")

    @coe.setter
    def coe(self, value: float) -> None:
        self._cards[0].set_value("coe", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the m, Flow potential exponent.  For face centered cubic (FCC) materials m=8 is recommended and for body centered cubic (BCC) materials m=6 may be used.
        LT.0.0: |M | is a load curve ID specifying the flow potential exponent as a function of effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[0].set_value("m", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the a1, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the a2, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[1].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the a3, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[1].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the a4, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        self._cards[1].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the a5, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        self._cards[1].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the a6, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        self._cards[1].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the a7, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        self._cards[1].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the a8, material constant in Barlat's yield equation.
        LT.0.0:	|ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        self._cards[1].set_value("alpha8", value)

    @property
    def cb(self) -> typing.Optional[float]:
        """Get or set the The uppercase B defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("cb")

    @cb.setter
    def cb(self, value: float) -> None:
        self._cards[4].set_value("cb", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[4].set_value("y", value)

    @property
    def sc1(self) -> typing.Optional[float]:
        """Get or set the The lowercase c defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("sc1")

    @sc1.setter
    def sc1(self, value: float) -> None:
        self._cards[4].set_value("sc1", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[4].set_value("k", value)

    @property
    def rsat(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("rsat")

    @rsat.setter
    def rsat(self, value: float) -> None:
        self._cards[4].set_value("rsat", value)

    @property
    def sb(self) -> typing.Optional[float]:
        """Get or set the The lowercase b as defined in the Yoshida's equations
        """ # nopep8
        return self._cards[4].get_value("sb")

    @sb.setter
    def sb(self, value: float) -> None:
        self._cards[4].set_value("sb", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameter associated with work-hardening stagnation, defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[4].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[4].set_value("h", value)

    @property
    def aopt(self) -> typing.Optional[int]:
        """Get or set the Material axes option:
        EQ.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
        EQ.2: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[5].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        self._cards[5].set_value("aopt", value)

    @property
    def iopt(self) -> int:
        """Get or set the Kinematic hardening rule flag:
        EQ.0: Original Yoshida formulation,
        EQ.1: Modified formulation. Define C1, C2 below.
        """ # nopep8
        return self._cards[5].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iopt must be one of {0,1}""")
        self._cards[5].set_value("iopt", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R:
        """ # nopep8
        return self._cards[5].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[5].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R:
        """ # nopep8
        return self._cards[5].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[5].set_value("c2", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[6].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[7].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[7].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[7].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[7].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[7].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[7].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

