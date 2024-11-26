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

class Mat263(KeywordBase):
    """DYNA MAT_263 keyword"""

    keyword = "MAT"
    subkeyword = "263"
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
                        "hr",
                        float,
                        40,
                        10,
                        kwargs.get("hr", 1.0)
                    ),
                    Field(
                        "p1",
                        float,
                        50,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        60,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "iter",
                        float,
                        70,
                        10,
                        kwargs.get("iter", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "afr",
                        int,
                        0,
                        10,
                        kwargs.get("afr")
                    ),
                    Field(
                        "nfunc",
                        int,
                        10,
                        10,
                        kwargs.get("nfunc", 1)
                    ),
                    Field(
                        "aopt",
                        float,
                        20,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "e0",
                        float,
                        50,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "lcf",
                        int,
                        60,
                        10,
                        kwargs.get("lcf", 0)
                    ),
                    Field(
                        "p3",
                        float,
                        70,
                        10,
                        kwargs.get("p3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
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
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
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
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
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
                        "c5",
                        float,
                        40,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        50,
                        10,
                        kwargs.get("c6")
                    ),
                    Field(
                        "cc",
                        float,
                        60,
                        10,
                        kwargs.get("cc")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pc1",
                        float,
                        0,
                        10,
                        kwargs.get("pc1")
                    ),
                    Field(
                        "pc2",
                        float,
                        10,
                        10,
                        kwargs.get("pc2")
                    ),
                    Field(
                        "pc3",
                        float,
                        20,
                        10,
                        kwargs.get("pc3")
                    ),
                    Field(
                        "pc4",
                        float,
                        30,
                        10,
                        kwargs.get("pc4")
                    ),
                    Field(
                        "pc5",
                        float,
                        40,
                        10,
                        kwargs.get("pc5")
                    ),
                    Field(
                        "pc6",
                        float,
                        50,
                        10,
                        kwargs.get("pc6")
                    ),
                    Field(
                        "pcc",
                        float,
                        60,
                        10,
                        kwargs.get("pcc")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vf1",
                        float,
                        0,
                        10,
                        kwargs.get("vf1")
                    ),
                    Field(
                        "vf2",
                        float,
                        10,
                        10,
                        kwargs.get("vf2")
                    ),
                    Field(
                        "vf3",
                        float,
                        20,
                        10,
                        kwargs.get("vf3")
                    ),
                    Field(
                        "vf4",
                        float,
                        30,
                        10,
                        kwargs.get("vf4")
                    ),
                    Field(
                        "vf5",
                        float,
                        40,
                        10,
                        kwargs.get("vf5")
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
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat263.option_specs[0],
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
        """Get or set the Poisson’s ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def hr(self) -> float:
        """Get or set the Hardening rules (see section Hardening laws below):
        EQ.1.0:	Linear hardening(default)
        EQ.2.0 : Exponential hardening(Swift)
        EQ.3.0 : Load curve
        EQ.4.0 : Exponential hardening(Voce)
        EQ.5.0 : Exponential hardening(Gosh)
        EQ.6.0 : Exponential hardening(Hocken - Sherby)
        """ # nopep8
        return self._cards[0].get_value("hr")

    @hr.setter
    def hr(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]:
            raise Exception("""hr must be one of {1.0,2.0,3.0,4.0,5.0,6.0}""")
        self._cards[0].set_value("hr", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1.0:	Tangent modulus
        HR.EQ.2.0 : q, coefficient for exponential hardening law(Swift)
        HR.EQ.4.0 : a, coefficient for exponential hardening law(Voce)
        HR.EQ.5.0 : q, coefficient for exponential hardening law(Gosh)
        HR.EQ.6.0 : a, coefficient for exponential hardening law(Hocket - Sherby)
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[0].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1.0:	Yield stress for the linear hardening law
        HR.EQ.2.0 : n, coefficient for (Swift) exponential hardening
        HR.EQ.4.0 : c, coefficient for exponential hardening law(Voce)
        HR.EQ.5.0 : n, coefficient for exponential hardening law(Gosh)
        HR.EQ.6.0 : c, coefficient for exponential hardening law(Hocket - Sherby)
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[0].set_value("p2", value)

    @property
    def iter(self) -> float:
        """Get or set the Iteration flag for speed:
        EQ.0.0:	Fully iterative
        EQ.1.0 : Fixed at three iterations.Generally, ITER = 0.0 is recommended.However, ITER = 1.0 is faster and may give acceptable results in most problems
        """ # nopep8
        return self._cards[0].get_value("iter")

    @iter.setter
    def iter(self, value: float) -> None:
        self._cards[0].set_value("iter", value)

    @property
    def afr(self) -> typing.Optional[int]:
        """Get or set the Flag to use associated flow rule (AFR):
        EQ.0:	Use non - AFR.
        EQ.1 : Use AFR.
        """ # nopep8
        return self._cards[1].get_value("afr")

    @afr.setter
    def afr(self, value: int) -> None:
        self._cards[1].set_value("afr", value)

    @property
    def nfunc(self) -> int:
        """Get or set the Number of Drucker function components. Currently NFUNC is always set to 1
        """ # nopep8
        return self._cards[1].get_value("nfunc")

    @nfunc.setter
    def nfunc(self, value: int) -> None:
        self._cards[1].set_value("nfunc", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for more complete description).
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.The shells only the material axes are rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined a and d defined below, as with* DEFINED_COORDINATE_VECTOR.
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.The material directions are determined as follows : a is the cross product of v with the normal vector, b is the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM, or *DEFINE_COORDINATE_VECTOR).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the hardening law for HR = 3
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.2.0:	ε_0, initial yield strain for exponential hardening law(Swift) (default = 0.0)
        HR.EQ.4.0 : b, coefficient for exponential hardening(Voce)
        HR.EQ.5.0 : ε_0, initial yield strain for exponential hardening(Gosh), Default = 0.0
        HR.EQ.6.0 : b, coefficient for exponential hardening law(Hocket - Sherby)
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def lcf(self) -> int:
        """Get or set the Fracture curve:
        EQ.0:	No fracture curves(default)
        GT.0 : Load curve or table ID of customized fracture curve / surface.If referring to a load curve ID, the fracture curve is defined as effective plastic strain as a function of triaxiality.If referring to a table ID, for each load parameter, an effective plastic strain as a function of.triaxiality curve can be defined(only applicable to solids
        EQ. - 1:	Drucker ductile fracture criterion.Optional Card 7 is needed in this case.VF1, VF2and VF3 in Card 7 will be used as a, band c in the Drucker ductile fracture criterion.See section Fracture criteria for more details.
        EQ. - 2:	DF2016 fracture criterion.Optional card 7 is needed in this case.VF1, VF2, VF3, VF4and VF5 in Card 7 will be used as C1, C2, C3and C in DF2016 criterion.See section Fracture criteria for more details.
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        self._cards[1].set_value("lcf", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Material parameter:HR.EQ.5.0:	p, coefficient for exponential hardening(Gosh)HR.EQ.6.0 : n, exponent for exponential hardening law(Hocket - Sherby)
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[1].set_value("p3", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.0
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.0
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.0
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.0
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.0
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.0
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.0
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[4].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[4].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[4].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[4].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[4].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c_1^' through c_6^' that defines the fourth order linear transformation tensor L'
        """ # nopep8
        return self._cards[4].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[4].set_value("c6", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Material constant c in Drucker yield function. c is recommended to be 1.226 for BCC metals and 2 for FCC metals
        """ # nopep8
        return self._cards[4].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        self._cards[4].set_value("cc", value)

    @property
    def pc1(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc1")

    @pc1.setter
    def pc1(self, value: float) -> None:
        self._cards[5].set_value("pc1", value)

    @property
    def pc2(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc2")

    @pc2.setter
    def pc2(self, value: float) -> None:
        self._cards[5].set_value("pc2", value)

    @property
    def pc3(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc3")

    @pc3.setter
    def pc3(self, value: float) -> None:
        self._cards[5].set_value("pc3", value)

    @property
    def pc4(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc4")

    @pc4.setter
    def pc4(self, value: float) -> None:
        self._cards[5].set_value("pc4", value)

    @property
    def pc5(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc5")

    @pc5.setter
    def pc5(self, value: float) -> None:
        self._cards[5].set_value("pc5", value)

    @property
    def pc6(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameters c ̂_1 through c ̂_6 that defines the fourth order linear transformation tensor L ̂ for the plastic potential in the non-AFR case (see field AFR which is input on Card 2).
        """ # nopep8
        return self._cards[5].get_value("pc6")

    @pc6.setter
    def pc6(self, value: float) -> None:
        self._cards[5].set_value("pc6", value)

    @property
    def pcc(self) -> typing.Optional[float]:
        """Get or set the Material constant c ̂ in Drucker function for the plastic potential. c ̂ is recommended to be 1.226 for BCC metals and 2 for FCC metals unless calibrated otherwise.
        """ # nopep8
        return self._cards[5].get_value("pcc")

    @pcc.setter
    def pcc(self, value: float) -> None:
        self._cards[5].set_value("pcc", value)

    @property
    def vf1(self) -> typing.Optional[float]:
        """Get or set the Components of the fracture criterion included for LCF < 0. See LCF (input on Card 2) for a description.
        """ # nopep8
        return self._cards[6].get_value("vf1")

    @vf1.setter
    def vf1(self, value: float) -> None:
        self._cards[6].set_value("vf1", value)

    @property
    def vf2(self) -> typing.Optional[float]:
        """Get or set the Components of the fracture criterion included for LCF < 0. See LCF (input on Card 2) for a description.
        """ # nopep8
        return self._cards[6].get_value("vf2")

    @vf2.setter
    def vf2(self, value: float) -> None:
        self._cards[6].set_value("vf2", value)

    @property
    def vf3(self) -> typing.Optional[float]:
        """Get or set the Components of the fracture criterion included for LCF < 0. See LCF (input on Card 2) for a description.
        """ # nopep8
        return self._cards[6].get_value("vf3")

    @vf3.setter
    def vf3(self, value: float) -> None:
        self._cards[6].set_value("vf3", value)

    @property
    def vf4(self) -> typing.Optional[float]:
        """Get or set the Components of the fracture criterion included for LCF < 0. See LCF (input on Card 2) for a description.
        """ # nopep8
        return self._cards[6].get_value("vf4")

    @vf4.setter
    def vf4(self, value: float) -> None:
        self._cards[6].set_value("vf4", value)

    @property
    def vf5(self) -> typing.Optional[float]:
        """Get or set the Components of the fracture criterion included for LCF < 0. See LCF (input on Card 2) for a description.
        """ # nopep8
        return self._cards[6].get_value("vf5")

    @vf5.setter
    def vf5(self, value: float) -> None:
        self._cards[6].set_value("vf5", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

