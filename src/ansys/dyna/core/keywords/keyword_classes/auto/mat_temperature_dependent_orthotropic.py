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

class MatTemperatureDependentOrthotropic(KeywordBase):
    """DYNA MAT_TEMPERATURE_DEPENDENT_ORTHOTROPIC keyword"""

    keyword = "MAT"
    subkeyword = "TEMPERATURE_DEPENDENT_ORTHOTROPIC"
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
                        "aopt",
                        float,
                        20,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "ref",
                        float,
                        30,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                    Field(
                        "macf",
                        int,
                        40,
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
            Card(
                [
                    Field(
                        "eai",
                        float,
                        0,
                        10,
                        kwargs.get("eai")
                    ),
                    Field(
                        "ebi",
                        float,
                        10,
                        10,
                        kwargs.get("ebi")
                    ),
                    Field(
                        "eci",
                        float,
                        20,
                        10,
                        kwargs.get("eci")
                    ),
                    Field(
                        "prabi",
                        float,
                        30,
                        10,
                        kwargs.get("prabi")
                    ),
                    Field(
                        "prcai",
                        float,
                        40,
                        10,
                        kwargs.get("prcai")
                    ),
                    Field(
                        "prcbi",
                        float,
                        50,
                        10,
                        kwargs.get("prcbi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aai",
                        float,
                        0,
                        10,
                        kwargs.get("aai")
                    ),
                    Field(
                        "abi",
                        float,
                        10,
                        10,
                        kwargs.get("abi")
                    ),
                    Field(
                        "aci",
                        float,
                        20,
                        10,
                        kwargs.get("aci")
                    ),
                    Field(
                        "gabi",
                        float,
                        30,
                        10,
                        kwargs.get("gabi")
                    ),
                    Field(
                        "gbci",
                        float,
                        40,
                        10,
                        kwargs.get("gbci")
                    ),
                    Field(
                        "gcai",
                        float,
                        50,
                        10,
                        kwargs.get("gcai")
                    ),
                    Field(
                        "ti",
                        float,
                        60,
                        10,
                        kwargs.get("ti")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatTemperatureDependentOrthotropic.option_specs[0],
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
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[0].set_value("aopt", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor, see *INITIAL_FOAM_REFERENCE_ GEOMETRY. Only for 8-noded solid elements with one point integration.
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[0].set_value("ref", value)

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
        return self._cards[0].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -4, -3, -2]:
            raise Exception("""macf must be one of {1,2,3,4,-4,-3,-2}""")
        self._cards[0].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[2].set_value("beta", value)

    @property
    def eai(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in a-direction at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("eai")

    @eai.setter
    def eai(self, value: float) -> None:
        self._cards[3].set_value("eai", value)

    @property
    def ebi(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in b-direction at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("ebi")

    @ebi.setter
    def ebi(self, value: float) -> None:
        self._cards[3].set_value("ebi", value)

    @property
    def eci(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in c-direction at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("eci")

    @eci.setter
    def eci(self, value: float) -> None:
        self._cards[3].set_value("eci", value)

    @property
    def prabi(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("prabi")

    @prabi.setter
    def prabi(self, value: float) -> None:
        self._cards[3].set_value("prabi", value)

    @property
    def prcai(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("prcai")

    @prcai.setter
    def prcai(self, value: float) -> None:
        self._cards[3].set_value("prcai", value)

    @property
    def prcbi(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio cb at temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("prcbi")

    @prcbi.setter
    def prcbi(self, value: float) -> None:
        self._cards[3].set_value("prcbi", value)

    @property
    def aai(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion in a-direction at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("aai")

    @aai.setter
    def aai(self, value: float) -> None:
        self._cards[4].set_value("aai", value)

    @property
    def abi(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion in b-direction at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("abi")

    @abi.setter
    def abi(self, value: float) -> None:
        self._cards[4].set_value("abi", value)

    @property
    def aci(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion in c-direction at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("aci")

    @aci.setter
    def aci(self, value: float) -> None:
        self._cards[4].set_value("aci", value)

    @property
    def gabi(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ab at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("gabi")

    @gabi.setter
    def gabi(self, value: float) -> None:
        self._cards[4].set_value("gabi", value)

    @property
    def gbci(self) -> typing.Optional[float]:
        """Get or set the Shear modulus bc at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("gbci")

    @gbci.setter
    def gbci(self, value: float) -> None:
        self._cards[4].set_value("gbci", value)

    @property
    def gcai(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ca at temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("gcai")

    @gcai.setter
    def gcai(self, value: float) -> None:
        self._cards[4].set_value("gcai", value)

    @property
    def ti(self) -> typing.Optional[float]:
        """Get or set the i'th temperature.
        """ # nopep8
        return self._cards[4].get_value("ti")

    @ti.setter
    def ti(self, value: float) -> None:
        self._cards[4].set_value("ti", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

