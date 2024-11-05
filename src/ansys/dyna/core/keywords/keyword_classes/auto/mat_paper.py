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

class MatPaper(KeywordBase):
    """DYNA MAT_PAPER keyword"""

    keyword = "MAT"
    subkeyword = "PAPER"
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
                        "e1",
                        float,
                        20,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        float,
                        30,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "e3",
                        float,
                        40,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "pr21",
                        float,
                        50,
                        10,
                        kwargs.get("pr21")
                    ),
                    Field(
                        "pr32",
                        float,
                        60,
                        10,
                        kwargs.get("pr32")
                    ),
                    Field(
                        "pr31",
                        float,
                        70,
                        10,
                        kwargs.get("pr31")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g12",
                        float,
                        0,
                        10,
                        kwargs.get("g12")
                    ),
                    Field(
                        "g23",
                        float,
                        10,
                        10,
                        kwargs.get("g23")
                    ),
                    Field(
                        "g13",
                        float,
                        20,
                        10,
                        kwargs.get("g13")
                    ),
                    Field(
                        "e3c",
                        float,
                        30,
                        10,
                        kwargs.get("e3c")
                    ),
                    Field(
                        "cc",
                        float,
                        40,
                        10,
                        kwargs.get("cc")
                    ),
                    Field(
                        "twok",
                        float,
                        50,
                        10,
                        kwargs.get("twok")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s01",
                        float,
                        0,
                        10,
                        kwargs.get("s01")
                    ),
                    Field(
                        "a01",
                        float,
                        10,
                        10,
                        kwargs.get("a01")
                    ),
                    Field(
                        "b01",
                        float,
                        20,
                        10,
                        kwargs.get("b01")
                    ),
                    Field(
                        "c01",
                        float,
                        30,
                        10,
                        kwargs.get("c01")
                    ),
                    Field(
                        "s02",
                        float,
                        40,
                        10,
                        kwargs.get("s02")
                    ),
                    Field(
                        "a02",
                        float,
                        50,
                        10,
                        kwargs.get("a02")
                    ),
                    Field(
                        "b02",
                        float,
                        60,
                        10,
                        kwargs.get("b02")
                    ),
                    Field(
                        "c02",
                        float,
                        70,
                        10,
                        kwargs.get("c02")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s03",
                        float,
                        0,
                        10,
                        kwargs.get("s03")
                    ),
                    Field(
                        "a03",
                        float,
                        10,
                        10,
                        kwargs.get("a03")
                    ),
                    Field(
                        "b03",
                        float,
                        20,
                        10,
                        kwargs.get("b03")
                    ),
                    Field(
                        "c03",
                        float,
                        30,
                        10,
                        kwargs.get("c03")
                    ),
                    Field(
                        "s04",
                        float,
                        40,
                        10,
                        kwargs.get("s04")
                    ),
                    Field(
                        "a04",
                        float,
                        50,
                        10,
                        kwargs.get("a04")
                    ),
                    Field(
                        "b04",
                        float,
                        60,
                        10,
                        kwargs.get("b04")
                    ),
                    Field(
                        "c04",
                        float,
                        70,
                        10,
                        kwargs.get("c04")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s05",
                        float,
                        0,
                        10,
                        kwargs.get("s05")
                    ),
                    Field(
                        "a05",
                        float,
                        10,
                        10,
                        kwargs.get("a05")
                    ),
                    Field(
                        "b05",
                        float,
                        20,
                        10,
                        kwargs.get("b05")
                    ),
                    Field(
                        "c05",
                        float,
                        30,
                        10,
                        kwargs.get("c05")
                    ),
                    Field(
                        "prp1",
                        float,
                        40,
                        10,
                        kwargs.get("prp1", 0.5)
                    ),
                    Field(
                        "prp2",
                        float,
                        50,
                        10,
                        kwargs.get("prp2", 0.133)
                    ),
                    Field(
                        "prp4",
                        float,
                        60,
                        10,
                        kwargs.get("prp4", 0.5)
                    ),
                    Field(
                        "prp5",
                        float,
                        70,
                        10,
                        kwargs.get("prp5", 0.133)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "asig",
                        float,
                        0,
                        10,
                        kwargs.get("asig")
                    ),
                    Field(
                        "bsig",
                        float,
                        10,
                        10,
                        kwargs.get("bsig")
                    ),
                    Field(
                        "csig",
                        float,
                        20,
                        10,
                        kwargs.get("csig")
                    ),
                    Field(
                        "tau0",
                        float,
                        30,
                        10,
                        kwargs.get("tau0")
                    ),
                    Field(
                        "atau",
                        float,
                        40,
                        10,
                        kwargs.get("atau")
                    ),
                    Field(
                        "btau",
                        float,
                        50,
                        10,
                        kwargs.get("btau")
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
                        "macf",
                        int,
                        10,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
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
                option_spec = MatPaper.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
    def e1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E1.
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E2.
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[0].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E3
        """ # nopep8
        return self._cards[0].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        self._cards[0].set_value("e3", value)

    @property
    def pr21(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V21
        """ # nopep8
        return self._cards[0].get_value("pr21")

    @pr21.setter
    def pr21(self, value: float) -> None:
        self._cards[0].set_value("pr21", value)

    @property
    def pr32(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V32
        """ # nopep8
        return self._cards[0].get_value("pr32")

    @pr32.setter
    def pr32(self, value: float) -> None:
        self._cards[0].set_value("pr32", value)

    @property
    def pr31(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V31
        """ # nopep8
        return self._cards[0].get_value("pr31")

    @pr31.setter
    def pr31(self, value: float) -> None:
        self._cards[0].set_value("pr31", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G12.
        """ # nopep8
        return self._cards[1].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[1].set_value("g12", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G23.
        """ # nopep8
        return self._cards[1].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        self._cards[1].set_value("g23", value)

    @property
    def g13(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G13.
        """ # nopep8
        return self._cards[1].get_value("g13")

    @g13.setter
    def g13(self, value: float) -> None:
        self._cards[1].set_value("g13", value)

    @property
    def e3c(self) -> typing.Optional[float]:
        """Get or set the Elastic compression parameter.
        """ # nopep8
        return self._cards[1].get_value("e3c")

    @e3c.setter
    def e3c(self, value: float) -> None:
        self._cards[1].set_value("e3c", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Elastic compression exponent
        """ # nopep8
        return self._cards[1].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        self._cards[1].set_value("cc", value)

    @property
    def twok(self) -> typing.Optional[float]:
        """Get or set the Exponent in in-plane yield surface
        """ # nopep8
        return self._cards[1].get_value("twok")

    @twok.setter
    def twok(self, value: float) -> None:
        self._cards[1].set_value("twok", value)

    @property
    def s01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[2].get_value("s01")

    @s01.setter
    def s01(self, value: float) -> None:
        self._cards[2].set_value("s01", value)

    @property
    def a01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("a01")

    @a01.setter
    def a01(self, value: float) -> None:
        self._cards[2].set_value("a01", value)

    @property
    def b01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("b01")

    @b01.setter
    def b01(self, value: float) -> None:
        self._cards[2].set_value("b01", value)

    @property
    def c01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("c01")

    @c01.setter
    def c01(self, value: float) -> None:
        self._cards[2].set_value("c01", value)

    @property
    def s02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
        """ # nopep8
        return self._cards[2].get_value("s02")

    @s02.setter
    def s02(self, value: float) -> None:
        self._cards[2].set_value("s02", value)

    @property
    def a02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("a02")

    @a02.setter
    def a02(self, value: float) -> None:
        self._cards[2].set_value("a02", value)

    @property
    def b02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("b02")

    @b02.setter
    def b02(self, value: float) -> None:
        self._cards[2].set_value("b02", value)

    @property
    def c02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("c02")

    @c02.setter
    def c02(self, value: float) -> None:
        self._cards[2].set_value("c02", value)

    @property
    def s03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[3].get_value("s03")

    @s03.setter
    def s03(self, value: float) -> None:
        self._cards[3].set_value("s03", value)

    @property
    def a03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("a03")

    @a03.setter
    def a03(self, value: float) -> None:
        self._cards[3].set_value("a03", value)

    @property
    def b03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("b03")

    @b03.setter
    def b03(self, value: float) -> None:
        self._cards[3].set_value("b03", value)

    @property
    def c03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("c03")

    @c03.setter
    def c03(self, value: float) -> None:
        self._cards[3].set_value("c03", value)

    @property
    def s04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
        """ # nopep8
        return self._cards[3].get_value("s04")

    @s04.setter
    def s04(self, value: float) -> None:
        self._cards[3].set_value("s04", value)

    @property
    def a04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("a04")

    @a04.setter
    def a04(self, value: float) -> None:
        self._cards[3].set_value("a04", value)

    @property
    def b04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("b04")

    @b04.setter
    def b04(self, value: float) -> None:
        self._cards[3].set_value("b04", value)

    @property
    def c04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("c04")

    @c04.setter
    def c04(self, value: float) -> None:
        self._cards[3].set_value("c04", value)

    @property
    def s05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[4].get_value("s05")

    @s05.setter
    def s05(self, value: float) -> None:
        self._cards[4].set_value("s05", value)

    @property
    def a05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("a05")

    @a05.setter
    def a05(self, value: float) -> None:
        self._cards[4].set_value("a05", value)

    @property
    def b05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("b05")

    @b05.setter
    def b05(self, value: float) -> None:
        self._cards[4].set_value("b05", value)

    @property
    def c05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("c05")

    @c05.setter
    def c05(self, value: float) -> None:
        self._cards[4].set_value("c05", value)

    @property
    def prp1(self) -> float:
        """Get or set the Tensile plastic Poisson's ratio in direction 1
        """ # nopep8
        return self._cards[4].get_value("prp1")

    @prp1.setter
    def prp1(self, value: float) -> None:
        self._cards[4].set_value("prp1", value)

    @property
    def prp2(self) -> float:
        """Get or set the Tensile plastic Poisson's ratio in direction 2
        """ # nopep8
        return self._cards[4].get_value("prp2")

    @prp2.setter
    def prp2(self, value: float) -> None:
        self._cards[4].set_value("prp2", value)

    @property
    def prp4(self) -> float:
        """Get or set the Compressive plastic Poisson's ratio in direction 1
        """ # nopep8
        return self._cards[4].get_value("prp4")

    @prp4.setter
    def prp4(self, value: float) -> None:
        self._cards[4].set_value("prp4", value)

    @property
    def prp5(self) -> float:
        """Get or set the Compressive plastic Poisson's ratio in direction 2
        """ # nopep8
        return self._cards[4].get_value("prp5")

    @prp5.setter
    def prp5(self, value: float) -> None:
        self._cards[4].set_value("prp5", value)

    @property
    def asig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity yield parameter.
        """ # nopep8
        return self._cards[5].get_value("asig")

    @asig.setter
    def asig(self, value: float) -> None:
        self._cards[5].set_value("asig", value)

    @property
    def bsig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[5].get_value("bsig")

    @bsig.setter
    def bsig(self, value: float) -> None:
        self._cards[5].set_value("bsig", value)

    @property
    def csig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[5].get_value("csig")

    @csig.setter
    def csig(self, value: float) -> None:
        self._cards[5].set_value("csig", value)

    @property
    def tau0(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity yield parameter.
        """ # nopep8
        return self._cards[5].get_value("tau0")

    @tau0.setter
    def tau0(self, value: float) -> None:
        self._cards[5].set_value("tau0", value)

    @property
    def atau(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity hardening parameter
        """ # nopep8
        return self._cards[5].get_value("atau")

    @atau.setter
    def atau(self, value: float) -> None:
        self._cards[5].set_value("atau", value)

    @property
    def btau(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity hardening parameter
        """ # nopep8
        return self._cards[5].get_value("btau")

    @btau.setter
    def btau(self, value: float) -> None:
        self._cards[5].set_value("btau", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[6].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[6].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        EQ. - 4 : Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[6].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[6].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[6].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[6].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[6].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[6].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[6].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[7].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[7].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[7].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[7].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[7].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[7].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO
        """ # nopep8
        return self._cards[7].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[7].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

