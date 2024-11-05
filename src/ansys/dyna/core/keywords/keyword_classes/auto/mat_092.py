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

class Mat092(KeywordBase):
    """DYNA MAT_092 keyword"""

    keyword = "MAT"
    subkeyword = "092"
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
                        "c1",
                        float,
                        20,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        40,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        50,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        60,
                        10,
                        kwargs.get("c5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xk",
                        float,
                        0,
                        10,
                        kwargs.get("xk")
                    ),
                    Field(
                        "xlam",
                        float,
                        10,
                        10,
                        kwargs.get("xlam")
                    ),
                    Field(
                        "fang",
                        float,
                        20,
                        10,
                        kwargs.get("fang")
                    ),
                    Field(
                        "xlam0",
                        float,
                        30,
                        10,
                        kwargs.get("xlam0")
                    ),
                    Field(
                        "failsf",
                        float,
                        40,
                        10,
                        kwargs.get("failsf")
                    ),
                    Field(
                        "failsm",
                        float,
                        50,
                        10,
                        kwargs.get("failsm")
                    ),
                    Field(
                        "failshr",
                        float,
                        60,
                        10,
                        kwargs.get("failshr")
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
                        "ax",
                        float,
                        10,
                        10,
                        kwargs.get("ax")
                    ),
                    Field(
                        "ay",
                        float,
                        20,
                        10,
                        kwargs.get("ay")
                    ),
                    Field(
                        "az",
                        float,
                        30,
                        10,
                        kwargs.get("az")
                    ),
                    Field(
                        "bx",
                        float,
                        40,
                        10,
                        kwargs.get("bx")
                    ),
                    Field(
                        "by",
                        float,
                        50,
                        10,
                        kwargs.get("by")
                    ),
                    Field(
                        "bz",
                        float,
                        60,
                        10,
                        kwargs.get("bz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "la1",
                        float,
                        0,
                        10,
                        kwargs.get("la1")
                    ),
                    Field(
                        "la2",
                        float,
                        10,
                        10,
                        kwargs.get("la2")
                    ),
                    Field(
                        "la3",
                        float,
                        20,
                        10,
                        kwargs.get("la3")
                    ),
                    Field(
                        "macf",
                        int,
                        30,
                        10,
                        kwargs.get("macf", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s1",
                        float,
                        0,
                        10,
                        kwargs.get("s1")
                    ),
                    Field(
                        "s2",
                        float,
                        10,
                        10,
                        kwargs.get("s2")
                    ),
                    Field(
                        "s3",
                        float,
                        20,
                        10,
                        kwargs.get("s3")
                    ),
                    Field(
                        "s4",
                        float,
                        30,
                        10,
                        kwargs.get("s4")
                    ),
                    Field(
                        "s5",
                        float,
                        40,
                        10,
                        kwargs.get("s5")
                    ),
                    Field(
                        "s6",
                        float,
                        50,
                        10,
                        kwargs.get("s6")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        10,
                        kwargs.get("t1")
                    ),
                    Field(
                        "t2",
                        float,
                        10,
                        10,
                        kwargs.get("t2")
                    ),
                    Field(
                        "t3",
                        float,
                        20,
                        10,
                        kwargs.get("t3")
                    ),
                    Field(
                        "t4",
                        float,
                        30,
                        10,
                        kwargs.get("t4")
                    ),
                    Field(
                        "t5",
                        float,
                        40,
                        10,
                        kwargs.get("t5")
                    ),
                    Field(
                        "t6",
                        float,
                        50,
                        10,
                        kwargs.get("t6")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat092.option_specs[0],
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
        """Get or set the Mass Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Hyperelastic coefficients
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Hyperelastic coefficients
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[0].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Hyperelastic coefficients
        """ # nopep8
        return self._cards[0].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[0].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Hyperelastic coefficients
        """ # nopep8
        return self._cards[0].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[0].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Hyperelastic coefficients
        """ # nopep8
        return self._cards[0].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[0].set_value("c5", value)

    @property
    def xk(self) -> typing.Optional[float]:
        """Get or set the Bulk Modulus
        """ # nopep8
        return self._cards[1].get_value("xk")

    @xk.setter
    def xk(self, value: float) -> None:
        self._cards[1].set_value("xk", value)

    @property
    def xlam(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio at which fibers are straightened
        """ # nopep8
        return self._cards[1].get_value("xlam")

    @xlam.setter
    def xlam(self, value: float) -> None:
        self._cards[1].set_value("xlam", value)

    @property
    def fang(self) -> typing.Optional[float]:
        """Get or set the Angle in degrees of a material rotation about the c-axis, available for AOPT = 0 (shells only) and AOPT = 3 (all element types).  This angle may be overridden on the element card; see *ELEMENT_SHELL_BETA and *ELEMENT_SOLID_ORTHO
        """ # nopep8
        return self._cards[1].get_value("fang")

    @fang.setter
    def fang(self, value: float) -> None:
        self._cards[1].set_value("fang", value)

    @property
    def xlam0(self) -> typing.Optional[float]:
        """Get or set the Initial fiber stretch (optional)
        """ # nopep8
        return self._cards[1].get_value("xlam0")

    @xlam0.setter
    def xlam0(self, value: float) -> None:
        self._cards[1].set_value("xlam0", value)

    @property
    def failsf(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio for ligament fibers at failure (applies to shell elements only).  If zero, failure is not considered.
        """ # nopep8
        return self._cards[1].get_value("failsf")

    @failsf.setter
    def failsf(self, value: float) -> None:
        self._cards[1].set_value("failsf", value)

    @property
    def failsm(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio for surrounding matrix material at failure (applies to shell elements only).  If zero, failure is not considered
        """ # nopep8
        return self._cards[1].get_value("failsm")

    @failsm.setter
    def failsm(self, value: float) -> None:
        self._cards[1].set_value("failsm", value)

    @property
    def failshr(self) -> typing.Optional[float]:
        """Get or set the Shear strain at failure at a material point (applies to shell elements only).  If zero, failure is not considered.  This failure value is independent of FAILSF and FAILSM
        """ # nopep8
        return self._cards[1].get_value("failshr")

    @failshr.setter
    def failshr(self, value: float) -> None:
        self._cards[1].set_value("failshr", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_‌OPTIONTROPIC_‌ELASTIC particularly the Material Directions section, for details). The fiber direction depends on this coordinate system (see Remark 1).
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle FANG on this keyword or BETA on the * ELEMENT_SHELL_{OPTION} input.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or with FANG on this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying the angle rotation depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def ax(self) -> typing.Optional[float]:
        """Get or set the First material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        self._cards[2].set_value("ax", value)

    @property
    def ay(self) -> typing.Optional[float]:
        """Get or set the First material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        self._cards[2].set_value("ay", value)

    @property
    def az(self) -> typing.Optional[float]:
        """Get or set the First material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        self._cards[2].set_value("az", value)

    @property
    def bx(self) -> typing.Optional[float]:
        """Get or set the Second material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("bx")

    @bx.setter
    def bx(self, value: float) -> None:
        self._cards[2].set_value("bx", value)

    @property
    def by(self) -> typing.Optional[float]:
        """Get or set the Second material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("by")

    @by.setter
    def by(self, value: float) -> None:
        self._cards[2].set_value("by", value)

    @property
    def bz(self) -> typing.Optional[float]:
        """Get or set the Second material axis point or vector (bricks only)
        """ # nopep8
        return self._cards[2].get_value("bz")

    @bz.setter
    def bz(self, value: float) -> None:
        self._cards[2].set_value("bz", value)

    @property
    def la1(self) -> typing.Optional[float]:
        """Get or set the Local fiber orientation vector (bricks only)
        """ # nopep8
        return self._cards[3].get_value("la1")

    @la1.setter
    def la1(self, value: float) -> None:
        self._cards[3].set_value("la1", value)

    @property
    def la2(self) -> typing.Optional[float]:
        """Get or set the Local fiber orientation vector (bricks only)
        """ # nopep8
        return self._cards[3].get_value("la2")

    @la2.setter
    def la2(self, value: float) -> None:
        self._cards[3].set_value("la2", value)

    @property
    def la3(self) -> typing.Optional[float]:
        """Get or set the Local fiber orientation vector (bricks only)
        """ # nopep8
        return self._cards[3].get_value("la3")

    @la3.setter
    def la3(self, value: float) -> None:
        self._cards[3].set_value("la3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA or FANG rotation
        EQ.3 : Switch material axes a and c after BETA or FANG rotation
        EQ.4 : Switch material axes b and c after BETA or FANG rotation
        EQ. -4 : Switch material axes b and c before BETA or FANG rotation
        EQ. -3 : Switch material axes a and c before BETA or FANG rotation
        EQ. -2 : Switch material axes a and b before BETA or FANG rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.The BETA on * ELEMENT_SOLID_{OPTION} if defined is used for the rotation for all AOPT options.If BETA is not used for the element, then a rotation only occurs for AOPT = 3 where FANG is applied
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[3].set_value("macf", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[4].set_value("s1", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        self._cards[4].set_value("s2", value)

    @property
    def s3(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s3")

    @s3.setter
    def s3(self, value: float) -> None:
        self._cards[4].set_value("s3", value)

    @property
    def s4(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s4")

    @s4.setter
    def s4(self, value: float) -> None:
        self._cards[4].set_value("s4", value)

    @property
    def s5(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s5")

    @s5.setter
    def s5(self, value: float) -> None:
        self._cards[4].set_value("s5", value)

    @property
    def s6(self) -> typing.Optional[float]:
        """Get or set the Factors in the Prony series
        """ # nopep8
        return self._cards[4].get_value("s6")

    @s6.setter
    def s6(self, value: float) -> None:
        self._cards[4].set_value("s6", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[5].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[5].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[5].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[5].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[5].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Characteristic times for Prony series relaxation kernel
        """ # nopep8
        return self._cards[5].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        self._cards[5].set_value("t6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

