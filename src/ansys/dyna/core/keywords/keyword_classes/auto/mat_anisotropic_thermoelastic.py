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

"""Module providing the MatAnisotropicThermoelastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatAnisotropicThermoelastic(KeywordBase):
    """DYNA MAT_ANISOTROPIC_THERMOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ANISOTROPIC_THERMOELASTIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAnisotropicThermoelastic class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta1",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta2",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta3",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta4",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta5",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ta6",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c11",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c12",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c13",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c14",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c15",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c16",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c22",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c23",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c24",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c25",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c26",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c33",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c34",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c35",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c36",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c44",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c45",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c46",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c55",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c56",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c66",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tge",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tref",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "aopt",
                        float,
                        70,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "macf",
                        float,
                        60,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ref",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAnisotropicThermoelastic.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ta1(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta1")

    @ta1.setter
    def ta1(self, value: int) -> None:
        """Set the ta1 property."""
        self._cards[0].set_value("ta1", value)

    @property
    def ta2(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta2")

    @ta2.setter
    def ta2(self, value: int) -> None:
        """Set the ta2 property."""
        self._cards[0].set_value("ta2", value)

    @property
    def ta3(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta3")

    @ta3.setter
    def ta3(self, value: int) -> None:
        """Set the ta3 property."""
        self._cards[0].set_value("ta3", value)

    @property
    def ta4(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta4")

    @ta4.setter
    def ta4(self, value: int) -> None:
        """Set the ta4 property."""
        self._cards[0].set_value("ta4", value)

    @property
    def ta5(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta5")

    @ta5.setter
    def ta5(self, value: int) -> None:
        """Set the ta5 property."""
        self._cards[0].set_value("ta5", value)

    @property
    def ta6(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the coefficients of thermal expansion for the six components of strain tensor as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("ta6")

    @ta6.setter
    def ta6(self, value: int) -> None:
        """Set the ta6 property."""
        self._cards[0].set_value("ta6", value)

    @property
    def c11(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c11")

    @c11.setter
    def c11(self, value: int) -> None:
        """Set the c11 property."""
        self._cards[1].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c12")

    @c12.setter
    def c12(self, value: int) -> None:
        """Set the c12 property."""
        self._cards[1].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c13")

    @c13.setter
    def c13(self, value: int) -> None:
        """Set the c13 property."""
        self._cards[1].set_value("c13", value)

    @property
    def c14(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c14")

    @c14.setter
    def c14(self, value: int) -> None:
        """Set the c14 property."""
        self._cards[1].set_value("c14", value)

    @property
    def c15(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c15")

    @c15.setter
    def c15(self, value: int) -> None:
        """Set the c15 property."""
        self._cards[1].set_value("c15", value)

    @property
    def c16(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c16")

    @c16.setter
    def c16(self, value: int) -> None:
        """Set the c16 property."""
        self._cards[1].set_value("c16", value)

    @property
    def c22(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c22")

    @c22.setter
    def c22(self, value: int) -> None:
        """Set the c22 property."""
        self._cards[1].set_value("c22", value)

    @property
    def c23(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[1].get_value("c23")

    @c23.setter
    def c23(self, value: int) -> None:
        """Set the c23 property."""
        self._cards[1].set_value("c23", value)

    @property
    def c24(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c24")

    @c24.setter
    def c24(self, value: int) -> None:
        """Set the c24 property."""
        self._cards[2].set_value("c24", value)

    @property
    def c25(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c25")

    @c25.setter
    def c25(self, value: int) -> None:
        """Set the c25 property."""
        self._cards[2].set_value("c25", value)

    @property
    def c26(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c26")

    @c26.setter
    def c26(self, value: int) -> None:
        """Set the c26 property."""
        self._cards[2].set_value("c26", value)

    @property
    def c33(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c33")

    @c33.setter
    def c33(self, value: int) -> None:
        """Set the c33 property."""
        self._cards[2].set_value("c33", value)

    @property
    def c34(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c34")

    @c34.setter
    def c34(self, value: int) -> None:
        """Set the c34 property."""
        self._cards[2].set_value("c34", value)

    @property
    def c35(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c35")

    @c35.setter
    def c35(self, value: int) -> None:
        """Set the c35 property."""
        self._cards[2].set_value("c35", value)

    @property
    def c36(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c36")

    @c36.setter
    def c36(self, value: int) -> None:
        """Set the c36 property."""
        self._cards[2].set_value("c36", value)

    @property
    def c44(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[2].get_value("c44")

    @c44.setter
    def c44(self, value: int) -> None:
        """Set the c44 property."""
        self._cards[2].set_value("c44", value)

    @property
    def c45(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[3].get_value("c45")

    @c45.setter
    def c45(self, value: int) -> None:
        """Set the c45 property."""
        self._cards[3].set_value("c45", value)

    @property
    def c46(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[3].get_value("c46")

    @c46.setter
    def c46(self, value: int) -> None:
        """Set the c46 property."""
        self._cards[3].set_value("c46", value)

    @property
    def c55(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[3].get_value("c55")

    @c55.setter
    def c55(self, value: int) -> None:
        """Set the c55 property."""
        self._cards[3].set_value("c55", value)

    @property
    def c56(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[3].get_value("c56")

    @c56.setter
    def c56(self, value: int) -> None:
        """Set the c56 property."""
        self._cards[3].set_value("c56", value)

    @property
    def c66(self) -> typing.Optional[int]:
        """Get or set the Curve IDs defining the 6x6 symmetric constitutive matrix in material coordinate system as function of temperature. Note that 1 corresponds to the a material direction, 2 to the b material direction, and 3 to the c material direction.
        """ # nopep8
        return self._cards[3].get_value("c66")

    @c66.setter
    def c66(self, value: int) -> None:
        """Set the c66 property."""
        self._cards[3].set_value("c66", value)

    @property
    def tge(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the structural damping coefficient as function of temperature.
        """ # nopep8
        return self._cards[3].get_value("tge")

    @tge.setter
    def tge(self, value: int) -> None:
        """Set the tge property."""
        self._cards[3].set_value("tge", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for the calculation of thermal loads or the definition of thermal expansion coefficients.
        """ # nopep8
        return self._cards[3].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[3].set_value("tref", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[3].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT=1 and 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT=1 and 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT=1 and 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT=2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT=2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT=2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

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
        return self._cards[4].get_value("macf")

    @macf.setter
    def macf(self, value: float) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -2, -3, -4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-2,-3,-4}.""")
        self._cards[4].set_value("macf", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT=2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT=2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT=2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[5].set_value("d3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT=3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT=3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT=3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[5].set_value("v3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[5].set_value("beta", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use initial geometry to initialize the stress tensor (see MAT_002 for a complete description.).
        """ # nopep8
        return self._cards[5].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[5].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

