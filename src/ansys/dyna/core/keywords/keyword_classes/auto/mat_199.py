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

class Mat199(KeywordBase):
    """DYNA MAT_199 keyword"""

    keyword = "MAT"
    subkeyword = "199"
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
                ],
            ),
            Card(
                [
                    Field(
                        "cp12",
                        float,
                        0,
                        10,
                        kwargs.get("cp12")
                    ),
                    Field(
                        "cp13",
                        float,
                        10,
                        10,
                        kwargs.get("cp13")
                    ),
                    Field(
                        "cp21",
                        float,
                        20,
                        10,
                        kwargs.get("cp21")
                    ),
                    Field(
                        "cp23",
                        float,
                        30,
                        10,
                        kwargs.get("cp23")
                    ),
                    Field(
                        "cp31",
                        float,
                        40,
                        10,
                        kwargs.get("cp31")
                    ),
                    Field(
                        "cp32",
                        float,
                        50,
                        10,
                        kwargs.get("cp32")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cpp12",
                        float,
                        0,
                        10,
                        kwargs.get("cpp12")
                    ),
                    Field(
                        "cpp13",
                        float,
                        10,
                        10,
                        kwargs.get("cpp13")
                    ),
                    Field(
                        "cpp21",
                        float,
                        20,
                        10,
                        kwargs.get("cpp21")
                    ),
                    Field(
                        "cpp23",
                        float,
                        30,
                        10,
                        kwargs.get("cpp23")
                    ),
                    Field(
                        "cpp31",
                        float,
                        40,
                        10,
                        kwargs.get("cpp31")
                    ),
                    Field(
                        "cpp32",
                        float,
                        50,
                        10,
                        kwargs.get("cpp32")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cp44",
                        float,
                        0,
                        10,
                        kwargs.get("cp44")
                    ),
                    Field(
                        "cp55",
                        float,
                        10,
                        10,
                        kwargs.get("cp55")
                    ),
                    Field(
                        "cp66",
                        float,
                        20,
                        10,
                        kwargs.get("cp66")
                    ),
                    Field(
                        "cpp44",
                        float,
                        30,
                        10,
                        kwargs.get("cpp44")
                    ),
                    Field(
                        "cpp55",
                        float,
                        40,
                        10,
                        kwargs.get("cpp55")
                    ),
                    Field(
                        "cpp66",
                        float,
                        50,
                        10,
                        kwargs.get("cpp66")
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
                        "a",
                        float,
                        10,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "lcss",
                        int,
                        20,
                        10,
                        kwargs.get("lcss")
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
                        "macf",
                        float,
                        60,
                        10,
                        kwargs.get("macf", 1)
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
                option_spec = Mat199.option_specs[0],
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
        LT.0.0:	-E is either a load curve ID for Young’s modulus as a function of plastic strain or a table ID for Young’s modulus as a function of plastic strain and temperature.
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
    def cp12(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp12")

    @cp12.setter
    def cp12(self, value: float) -> None:
        self._cards[1].set_value("cp12", value)

    @property
    def cp13(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp13")

    @cp13.setter
    def cp13(self, value: float) -> None:
        self._cards[1].set_value("cp13", value)

    @property
    def cp21(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp21")

    @cp21.setter
    def cp21(self, value: float) -> None:
        self._cards[1].set_value("cp21", value)

    @property
    def cp23(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp23")

    @cp23.setter
    def cp23(self, value: float) -> None:
        self._cards[1].set_value("cp23", value)

    @property
    def cp31(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp31")

    @cp31.setter
    def cp31(self, value: float) -> None:
        self._cards[1].set_value("cp31", value)

    @property
    def cp32(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[1].get_value("cp32")

    @cp32.setter
    def cp32(self, value: float) -> None:
        self._cards[1].set_value("cp32", value)

    @property
    def cpp12(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp12")

    @cpp12.setter
    def cpp12(self, value: float) -> None:
        self._cards[2].set_value("cpp12", value)

    @property
    def cpp13(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp13")

    @cpp13.setter
    def cpp13(self, value: float) -> None:
        self._cards[2].set_value("cpp13", value)

    @property
    def cpp21(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp21")

    @cpp21.setter
    def cpp21(self, value: float) -> None:
        self._cards[2].set_value("cpp21", value)

    @property
    def cpp23(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp23")

    @cpp23.setter
    def cpp23(self, value: float) -> None:
        self._cards[2].set_value("cpp23", value)

    @property
    def cpp31(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp31")

    @cpp31.setter
    def cpp31(self, value: float) -> None:
        self._cards[2].set_value("cpp31", value)

    @property
    def cpp32(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[2].get_value("cpp32")

    @cpp32.setter
    def cpp32(self, value: float) -> None:
        self._cards[2].set_value("cpp32", value)

    @property
    def cp44(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[3].get_value("cp44")

    @cp44.setter
    def cp44(self, value: float) -> None:
        self._cards[3].set_value("cp44", value)

    @property
    def cp55(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[3].get_value("cp55")

    @cp55.setter
    def cp55(self, value: float) -> None:
        self._cards[3].set_value("cp55", value)

    @property
    def cp66(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
        """ # nopep8
        return self._cards[3].get_value("cp66")

    @cp66.setter
    def cp66(self, value: float) -> None:
        self._cards[3].set_value("cp66", value)

    @property
    def cpp44(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[3].get_value("cpp44")

    @cpp44.setter
    def cpp44(self, value: float) -> None:
        self._cards[3].set_value("cpp44", value)

    @property
    def cpp55(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[3].get_value("cpp55")

    @cpp55.setter
    def cpp55(self, value: float) -> None:
        self._cards[3].set_value("cpp55", value)

    @property
    def cpp66(self) -> typing.Optional[float]:
        """Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
        """ # nopep8
        return self._cards[3].get_value("cpp66")

    @cpp66.setter
    def cpp66(self, value: float) -> None:
        self._cards[3].set_value("cpp66", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface betwen the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[4].set_value("aopt", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Flow potential exponent a.
        """ # nopep8
        return self._cards[4].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[4].set_value("a", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or table ID for (isotropic) hardening:
        GT.0:	If LCSS is a load curve, then yield stress σ ̅ is a function of plastic strain.If LCSS is a table, then yield stress σ ̅ is a function of plastic strainand plastic strain rate.
        LT.0 : If - LCSS is a load curve, then yield stress σ ̅ is a function of plastic strain.If - LCSS is a table, then yield stress σ ̅ is a function of plastic strainand temperature.
        """ # nopep8
        return self._cards[4].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[4].set_value("lcss", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point  for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point  for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point  for AOPT = 1 and 4.
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
    def macf(self) -> float:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 6 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[5].get_value("macf")

    @macf.setter
    def macf(self, value: float) -> None:
        if value not in [1, 2, 3, 4, -2, -3, -4]:
            raise Exception("""macf must be one of {1,2,3,4,-2,-3,-4}""")
        self._cards[5].set_value("macf", value)

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
        """Get or set the Components of vector v for AOPT = 3.
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
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3.  It may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.and *ELEMENT_TSHELL_BETA..
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

