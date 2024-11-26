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

class MatAnisotropicElasticPlastic(KeywordBase):
    """DYNA MAT_ANISOTROPIC_ELASTIC_PLASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ANISOTROPIC_ELASTIC_PLASTIC"
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
                        "sigy",
                        float,
                        20,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "lcss",
                        int,
                        30,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "qr1",
                        float,
                        40,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        50,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        60,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        70,
                        10,
                        kwargs.get("cr2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c11",
                        float,
                        0,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c12",
                        float,
                        10,
                        10,
                        kwargs.get("c12")
                    ),
                    Field(
                        "c13",
                        float,
                        20,
                        10,
                        kwargs.get("c13")
                    ),
                    Field(
                        "c14",
                        float,
                        30,
                        10,
                        kwargs.get("c14")
                    ),
                    Field(
                        "c15",
                        float,
                        40,
                        10,
                        kwargs.get("c15")
                    ),
                    Field(
                        "c16",
                        float,
                        50,
                        10,
                        kwargs.get("c16")
                    ),
                    Field(
                        "c22",
                        float,
                        60,
                        10,
                        kwargs.get("c22")
                    ),
                    Field(
                        "c23",
                        float,
                        70,
                        10,
                        kwargs.get("c23")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c24",
                        float,
                        0,
                        10,
                        kwargs.get("c24")
                    ),
                    Field(
                        "c25",
                        float,
                        10,
                        10,
                        kwargs.get("c25")
                    ),
                    Field(
                        "c26",
                        float,
                        20,
                        10,
                        kwargs.get("c26")
                    ),
                    Field(
                        "c33",
                        float,
                        30,
                        10,
                        kwargs.get("c33")
                    ),
                    Field(
                        "c34",
                        float,
                        40,
                        10,
                        kwargs.get("c34")
                    ),
                    Field(
                        "c35",
                        float,
                        50,
                        10,
                        kwargs.get("c35")
                    ),
                    Field(
                        "c36",
                        float,
                        60,
                        10,
                        kwargs.get("c36")
                    ),
                    Field(
                        "c44",
                        float,
                        70,
                        10,
                        kwargs.get("c44")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c45",
                        float,
                        0,
                        10,
                        kwargs.get("c45")
                    ),
                    Field(
                        "c46",
                        float,
                        10,
                        10,
                        kwargs.get("c46")
                    ),
                    Field(
                        "c55",
                        float,
                        20,
                        10,
                        kwargs.get("c55")
                    ),
                    Field(
                        "c56",
                        float,
                        30,
                        10,
                        kwargs.get("c56")
                    ),
                    Field(
                        "c66",
                        float,
                        40,
                        10,
                        kwargs.get("c66")
                    ),
                    Field(
                        "r00/f",
                        float,
                        50,
                        10,
                        kwargs.get("r00/f")
                    ),
                    Field(
                        "r45/g",
                        float,
                        60,
                        10,
                        kwargs.get("r45/g")
                    ),
                    Field(
                        "r90/h",
                        float,
                        70,
                        10,
                        kwargs.get("r90/h")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s11/l",
                        float,
                        0,
                        10,
                        kwargs.get("s11/l")
                    ),
                    Field(
                        "s22/m",
                        float,
                        10,
                        10,
                        kwargs.get("s22/m")
                    ),
                    Field(
                        "s33/n",
                        float,
                        20,
                        10,
                        kwargs.get("s33/n")
                    ),
                    Field(
                        "s12",
                        float,
                        30,
                        10,
                        kwargs.get("s12")
                    ),
                    Field(
                        "aopt",
                        float,
                        40,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "vp",
                        float,
                        50,
                        10,
                        kwargs.get("vp", 0.0)
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "macf",
                        int,
                        70,
                        10,
                        kwargs.get("macf", 1)
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
                    Field(
                        "id3upd",
                        float,
                        60,
                        10,
                        kwargs.get("id3upd")
                    ),
                    Field(
                        "extra",
                        float,
                        70,
                        10,
                        kwargs.get("extra")
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
                    Field(
                        "ihis",
                        float,
                        70,
                        10,
                        kwargs.get("ihis")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xt",
                        float,
                        0,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "xc",
                        float,
                        10,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "yt",
                        float,
                        20,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "yc",
                        float,
                        30,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "sxy",
                        float,
                        40,
                        10,
                        kwargs.get("sxy")
                    ),
                    Field(
                        "ff12",
                        float,
                        50,
                        10,
                        kwargs.get("ff12")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ncfail",
                        float,
                        70,
                        10,
                        kwargs.get("ncfail", 10)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "zt",
                        float,
                        0,
                        10,
                        kwargs.get("zt")
                    ),
                    Field(
                        "zc",
                        float,
                        10,
                        10,
                        kwargs.get("zc")
                    ),
                    Field(
                        "syz",
                        float,
                        20,
                        10,
                        kwargs.get("syz")
                    ),
                    Field(
                        "szx",
                        float,
                        30,
                        10,
                        kwargs.get("szx")
                    ),
                    Field(
                        "ff23",
                        float,
                        40,
                        10,
                        kwargs.get("ff23")
                    ),
                    Field(
                        "ff31",
                        float,
                        50,
                        10,
                        kwargs.get("ff31")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAnisotropicElasticPlastic.option_specs[0],
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[0].set_value("lcss", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr1.
        """ # nopep8
        return self._cards[0].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[0].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr1.
        """ # nopep8
        return self._cards[0].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[0].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr2.
        """ # nopep8
        return self._cards[0].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[0].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr2.
        """ # nopep8
        return self._cards[0].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[0].set_value("cr2", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6 x 6 anisotropic constitutive matrix. Note that 1 corresponds to the a material direction
        """ # nopep8
        return self._cards[1].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[1].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6 x 6 anisotropic constitutive matrix. Note that 2 corresponds to the b material direction
        """ # nopep8
        return self._cards[1].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        self._cards[1].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        self._cards[1].set_value("c13", value)

    @property
    def c14(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c14")

    @c14.setter
    def c14(self, value: float) -> None:
        self._cards[1].set_value("c14", value)

    @property
    def c15(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c15")

    @c15.setter
    def c15(self, value: float) -> None:
        self._cards[1].set_value("c15", value)

    @property
    def c16(self) -> typing.Optional[float]:
        """Get or set the The 1,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c16")

    @c16.setter
    def c16(self, value: float) -> None:
        self._cards[1].set_value("c16", value)

    @property
    def c22(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c22")

    @c22.setter
    def c22(self, value: float) -> None:
        self._cards[1].set_value("c22", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[1].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[1].set_value("c23", value)

    @property
    def c24(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c24")

    @c24.setter
    def c24(self, value: float) -> None:
        self._cards[2].set_value("c24", value)

    @property
    def c25(self) -> typing.Optional[float]:
        """Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c25")

    @c25.setter
    def c25(self, value: float) -> None:
        self._cards[2].set_value("c25", value)

    @property
    def c26(self) -> typing.Optional[float]:
        """Get or set the The 2,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c26")

    @c26.setter
    def c26(self, value: float) -> None:
        self._cards[2].set_value("c26", value)

    @property
    def c33(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c33")

    @c33.setter
    def c33(self, value: float) -> None:
        self._cards[2].set_value("c33", value)

    @property
    def c34(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c34")

    @c34.setter
    def c34(self, value: float) -> None:
        self._cards[2].set_value("c34", value)

    @property
    def c35(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c35")

    @c35.setter
    def c35(self, value: float) -> None:
        self._cards[2].set_value("c35", value)

    @property
    def c36(self) -> typing.Optional[float]:
        """Get or set the The 3,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c36")

    @c36.setter
    def c36(self, value: float) -> None:
        self._cards[2].set_value("c36", value)

    @property
    def c44(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[2].get_value("c44")

    @c44.setter
    def c44(self, value: float) -> None:
        self._cards[2].set_value("c44", value)

    @property
    def c45(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("c45")

    @c45.setter
    def c45(self, value: float) -> None:
        self._cards[3].set_value("c45", value)

    @property
    def c46(self) -> typing.Optional[float]:
        """Get or set the The 4,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("c46")

    @c46.setter
    def c46(self, value: float) -> None:
        self._cards[3].set_value("c46", value)

    @property
    def c55(self) -> typing.Optional[float]:
        """Get or set the The 5,5 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("c55")

    @c55.setter
    def c55(self, value: float) -> None:
        self._cards[3].set_value("c55", value)

    @property
    def c56(self) -> typing.Optional[float]:
        """Get or set the The 5,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("c56")

    @c56.setter
    def c56(self, value: float) -> None:
        self._cards[3].set_value("c56", value)

    @property
    def c66(self) -> typing.Optional[float]:
        """Get or set the The 6,6 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("c66")

    @c66.setter
    def c66(self, value: float) -> None:
        self._cards[3].set_value("c66", value)

    @property
    def r00_f(self) -> typing.Optional[float]:
        """Get or set the The 0,0 term in the 6 x 6 anisotropic constitutive matrix.
        """ # nopep8
        return self._cards[3].get_value("r00/f")

    @r00_f.setter
    def r00_f(self, value: float) -> None:
        self._cards[3].set_value("r00/f", value)

    @property
    def r45_g(self) -> typing.Optional[float]:
        """Get or set the R45, Lankford parmeter determined from experiments
        """ # nopep8
        return self._cards[3].get_value("r45/g")

    @r45_g.setter
    def r45_g(self, value: float) -> None:
        self._cards[3].set_value("r45/g", value)

    @property
    def r90_h(self) -> typing.Optional[float]:
        """Get or set the R90 , Lankford parmeter determined from experiments
        """ # nopep8
        return self._cards[3].get_value("r90/h")

    @r90_h.setter
    def r90_h(self, value: float) -> None:
        self._cards[3].set_value("r90/h", value)

    @property
    def s11_l(self) -> typing.Optional[float]:
        """Get or set the Yield stress in local x-direction. This input is ignored if (R00,R45,R90)>0.
        """ # nopep8
        return self._cards[4].get_value("s11/l")

    @s11_l.setter
    def s11_l(self, value: float) -> None:
        self._cards[4].set_value("s11/l", value)

    @property
    def s22_m(self) -> typing.Optional[float]:
        """Get or set the Yield stress in local y-direction. This input is ignored if (R00,R45,R90)>0.
        """ # nopep8
        return self._cards[4].get_value("s22/m")

    @s22_m.setter
    def s22_m(self, value: float) -> None:
        self._cards[4].set_value("s22/m", value)

    @property
    def s33_n(self) -> typing.Optional[float]:
        """Get or set the Yield stress in local z-direction. This input is ignored if (R00,R45,R90)>0.
        """ # nopep8
        return self._cards[4].get_value("s33/n")

    @s33_n.setter
    def s33_n(self, value: float) -> None:
        self._cards[4].set_value("s33/n", value)

    @property
    def s12(self) -> typing.Optional[float]:
        """Get or set the Yield stress in local xy-direction. This input is ignored if (R00,R45,R90)>0.
        """ # nopep8
        return self._cards[4].get_value("s12")

    @s12.setter
    def s12(self, value: float) -> None:
        self._cards[4].set_value("s12", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[4].set_value("aopt", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: scale yield stress (default),
        EQ.1.0: viscoplastic formulation.
        """ # nopep8
        return self._cards[4].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""vp must be one of {0.0,1.0}""")
        self._cards[4].set_value("vp", value)

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
        return self._cards[4].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[4].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def id3upd(self) -> typing.Optional[float]:
        """Get or set the Flag for transverse through thickness strain update (thin shells only):
        EQ.0.0:	reflects R - values by splitting the strain tensor into elastic and plastic components
        EQ.1.0 : elastic update using total strain tensor
        """ # nopep8
        return self._cards[5].get_value("id3upd")

    @id3upd.setter
    def id3upd(self, value: float) -> None:
        self._cards[5].set_value("id3upd", value)

    @property
    def extra(self) -> typing.Optional[float]:
        """Get or set the Flag to input further data:
        EQ.1.0:Tsai-Wu failure criterion parameters (cards 8 and 9)
        """ # nopep8
        return self._cards[5].get_value("extra")

    @extra.setter
    def extra(self, value: float) -> None:
        self._cards[5].set_value("extra", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[6].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def ihis(self) -> typing.Optional[float]:
        """Get or set the Flag for material properties initialization.
        EQ.0:	material properties defined in Cards 1-5 are used
        GE.1:	Use *INITIAL_STRESS_SOLID/SHELL to initialize material properties on an element-by-element basis for solid or shell elements, respectively (see Remarks below).
        """ # nopep8
        return self._cards[6].get_value("ihis")

    @ihis.setter
    def ihis(self, value: float) -> None:
        self._cards[6].set_value("ihis", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Longitudinal tensile strength, a-axis.
        """ # nopep8
        return self._cards[7].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[7].set_value("xt", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Longitudinal compressive strength, a-axis (positive value).
        """ # nopep8
        return self._cards[7].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[7].set_value("xc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, b-axis.
        """ # nopep8
        return self._cards[7].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[7].set_value("yt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, b-axis (positive value).
        """ # nopep8
        return self._cards[7].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[7].set_value("yc", value)

    @property
    def sxy(self) -> typing.Optional[float]:
        """Get or set the Shear strength, ab-plane.
        """ # nopep8
        return self._cards[7].get_value("sxy")

    @sxy.setter
    def sxy(self, value: float) -> None:
        self._cards[7].set_value("sxy", value)

    @property
    def ff12(self) -> typing.Optional[float]:
        """Get or set the Scale factor between -1 and +1 for interaction term F12, see Remarks.
        """ # nopep8
        return self._cards[7].get_value("ff12")

    @ff12.setter
    def ff12(self, value: float) -> None:
        self._cards[7].set_value("ff12", value)

    @property
    def ncfail(self) -> float:
        """Get or set the Number of timesteps to reduce stresses until element deletion.The default is NCFAIL=10..
        """ # nopep8
        return self._cards[7].get_value("ncfail")

    @ncfail.setter
    def ncfail(self, value: float) -> None:
        self._cards[7].set_value("ncfail", value)

    @property
    def zt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, c-axis (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[8].set_value("zt", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, c-axis (positive value) (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[8].set_value("zc", value)

    @property
    def syz(self) -> typing.Optional[float]:
        """Get or set the Shear strength, bc-plane (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("syz")

    @syz.setter
    def syz(self, value: float) -> None:
        self._cards[8].set_value("syz", value)

    @property
    def szx(self) -> typing.Optional[float]:
        """Get or set the Shear strength, ca-plane (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("szx")

    @szx.setter
    def szx(self, value: float) -> None:
        self._cards[8].set_value("szx", value)

    @property
    def ff23(self) -> typing.Optional[float]:
        """Get or set the Scale factor between -1 and +1 for interaction term F23, see Remarks (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("ff23")

    @ff23.setter
    def ff23(self, value: float) -> None:
        self._cards[8].set_value("ff23", value)

    @property
    def ff31(self) -> typing.Optional[float]:
        """Get or set the Scale factor between -1 and +1 for interaction term F31, see Remarks (solid elements only).
        """ # nopep8
        return self._cards[8].get_value("ff31")

    @ff31.setter
    def ff31(self, value: float) -> None:
        self._cards[8].set_value("ff31", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

