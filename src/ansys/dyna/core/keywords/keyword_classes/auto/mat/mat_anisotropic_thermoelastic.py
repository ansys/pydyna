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
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATANISOTROPICTHERMOELASTIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ta1", int, 20, 10, None),
    FieldSchema("ta2", int, 30, 10, None),
    FieldSchema("ta3", int, 40, 10, None),
    FieldSchema("ta4", int, 50, 10, None),
    FieldSchema("ta5", int, 60, 10, None),
    FieldSchema("ta6", int, 70, 10, None),
)

_MATANISOTROPICTHERMOELASTIC_CARD1 = (
    FieldSchema("c11", int, 0, 10, None),
    FieldSchema("c12", int, 10, 10, None),
    FieldSchema("c13", int, 20, 10, None),
    FieldSchema("c14", int, 30, 10, None),
    FieldSchema("c15", int, 40, 10, None),
    FieldSchema("c16", int, 50, 10, None),
    FieldSchema("c22", int, 60, 10, None),
    FieldSchema("c23", int, 70, 10, None),
)

_MATANISOTROPICTHERMOELASTIC_CARD2 = (
    FieldSchema("c24", int, 0, 10, None),
    FieldSchema("c25", int, 10, 10, None),
    FieldSchema("c26", int, 20, 10, None),
    FieldSchema("c33", int, 30, 10, None),
    FieldSchema("c34", int, 40, 10, None),
    FieldSchema("c35", int, 50, 10, None),
    FieldSchema("c36", int, 60, 10, None),
    FieldSchema("c44", int, 70, 10, None),
)

_MATANISOTROPICTHERMOELASTIC_CARD3 = (
    FieldSchema("c45", int, 0, 10, None),
    FieldSchema("c46", int, 10, 10, None),
    FieldSchema("c55", int, 20, 10, None),
    FieldSchema("c56", int, 30, 10, None),
    FieldSchema("c66", int, 40, 10, None),
    FieldSchema("tge", int, 50, 10, None),
    FieldSchema("tref", float, 60, 10, None),
    FieldSchema("aopt", float, 70, 10, None),
)

_MATANISOTROPICTHERMOELASTIC_CARD4 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
    FieldSchema("macf", float, 60, 10, 1.0),
)

_MATANISOTROPICTHERMOELASTIC_CARD5 = (
    FieldSchema("d1", float, 0, 10, None),
    FieldSchema("d2", float, 10, 10, None),
    FieldSchema("d3", float, 20, 10, None),
    FieldSchema("v1", float, 30, 10, None),
    FieldSchema("v2", float, 40, 10, None),
    FieldSchema("v3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, None),
)

_MATANISOTROPICTHERMOELASTIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAnisotropicThermoelastic(KeywordBase):
    """DYNA MAT_ANISOTROPIC_THERMOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ANISOTROPIC_THERMOELASTIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "ta1": LinkType.DEFINE_CURVE,
        "ta2": LinkType.DEFINE_CURVE,
        "ta3": LinkType.DEFINE_CURVE,
        "ta4": LinkType.DEFINE_CURVE,
        "ta5": LinkType.DEFINE_CURVE,
        "ta6": LinkType.DEFINE_CURVE,
        "c11": LinkType.DEFINE_CURVE,
        "c12": LinkType.DEFINE_CURVE,
        "c13": LinkType.DEFINE_CURVE,
        "c14": LinkType.DEFINE_CURVE,
        "c15": LinkType.DEFINE_CURVE,
        "c16": LinkType.DEFINE_CURVE,
        "c22": LinkType.DEFINE_CURVE,
        "c23": LinkType.DEFINE_CURVE,
        "c24": LinkType.DEFINE_CURVE,
        "c25": LinkType.DEFINE_CURVE,
        "c26": LinkType.DEFINE_CURVE,
        "c33": LinkType.DEFINE_CURVE,
        "c34": LinkType.DEFINE_CURVE,
        "c35": LinkType.DEFINE_CURVE,
        "c36": LinkType.DEFINE_CURVE,
        "c44": LinkType.DEFINE_CURVE,
        "c45": LinkType.DEFINE_CURVE,
        "c46": LinkType.DEFINE_CURVE,
        "c55": LinkType.DEFINE_CURVE,
        "c56": LinkType.DEFINE_CURVE,
        "c66": LinkType.DEFINE_CURVE,
        "tge": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAnisotropicThermoelastic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICTHERMOELASTIC_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAnisotropicThermoelastic.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATANISOTROPICTHERMOELASTIC_OPTION0_CARD0,
                        **kwargs,
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

    @property
    def ta1_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta1:
                return kwd
        return None

    @ta1_link.setter
    def ta1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta1."""
        self.ta1 = value.lcid

    @property
    def ta2_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta2:
                return kwd
        return None

    @ta2_link.setter
    def ta2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta2."""
        self.ta2 = value.lcid

    @property
    def ta3_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta3:
                return kwd
        return None

    @ta3_link.setter
    def ta3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta3."""
        self.ta3 = value.lcid

    @property
    def ta4_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta4:
                return kwd
        return None

    @ta4_link.setter
    def ta4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta4."""
        self.ta4 = value.lcid

    @property
    def ta5_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta5:
                return kwd
        return None

    @ta5_link.setter
    def ta5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta5."""
        self.ta5 = value.lcid

    @property
    def ta6_link(self) -> DefineCurve:
        """Get the DefineCurve object for ta6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ta6:
                return kwd
        return None

    @ta6_link.setter
    def ta6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ta6."""
        self.ta6 = value.lcid

    @property
    def c11_link(self) -> DefineCurve:
        """Get the DefineCurve object for c11."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c11:
                return kwd
        return None

    @c11_link.setter
    def c11_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c11."""
        self.c11 = value.lcid

    @property
    def c12_link(self) -> DefineCurve:
        """Get the DefineCurve object for c12."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c12:
                return kwd
        return None

    @c12_link.setter
    def c12_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c12."""
        self.c12 = value.lcid

    @property
    def c13_link(self) -> DefineCurve:
        """Get the DefineCurve object for c13."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c13:
                return kwd
        return None

    @c13_link.setter
    def c13_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c13."""
        self.c13 = value.lcid

    @property
    def c14_link(self) -> DefineCurve:
        """Get the DefineCurve object for c14."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c14:
                return kwd
        return None

    @c14_link.setter
    def c14_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c14."""
        self.c14 = value.lcid

    @property
    def c15_link(self) -> DefineCurve:
        """Get the DefineCurve object for c15."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c15:
                return kwd
        return None

    @c15_link.setter
    def c15_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c15."""
        self.c15 = value.lcid

    @property
    def c16_link(self) -> DefineCurve:
        """Get the DefineCurve object for c16."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c16:
                return kwd
        return None

    @c16_link.setter
    def c16_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c16."""
        self.c16 = value.lcid

    @property
    def c22_link(self) -> DefineCurve:
        """Get the DefineCurve object for c22."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c22:
                return kwd
        return None

    @c22_link.setter
    def c22_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c22."""
        self.c22 = value.lcid

    @property
    def c23_link(self) -> DefineCurve:
        """Get the DefineCurve object for c23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c23:
                return kwd
        return None

    @c23_link.setter
    def c23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c23."""
        self.c23 = value.lcid

    @property
    def c24_link(self) -> DefineCurve:
        """Get the DefineCurve object for c24."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c24:
                return kwd
        return None

    @c24_link.setter
    def c24_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c24."""
        self.c24 = value.lcid

    @property
    def c25_link(self) -> DefineCurve:
        """Get the DefineCurve object for c25."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c25:
                return kwd
        return None

    @c25_link.setter
    def c25_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c25."""
        self.c25 = value.lcid

    @property
    def c26_link(self) -> DefineCurve:
        """Get the DefineCurve object for c26."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c26:
                return kwd
        return None

    @c26_link.setter
    def c26_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c26."""
        self.c26 = value.lcid

    @property
    def c33_link(self) -> DefineCurve:
        """Get the DefineCurve object for c33."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c33:
                return kwd
        return None

    @c33_link.setter
    def c33_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c33."""
        self.c33 = value.lcid

    @property
    def c34_link(self) -> DefineCurve:
        """Get the DefineCurve object for c34."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c34:
                return kwd
        return None

    @c34_link.setter
    def c34_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c34."""
        self.c34 = value.lcid

    @property
    def c35_link(self) -> DefineCurve:
        """Get the DefineCurve object for c35."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c35:
                return kwd
        return None

    @c35_link.setter
    def c35_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c35."""
        self.c35 = value.lcid

    @property
    def c36_link(self) -> DefineCurve:
        """Get the DefineCurve object for c36."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c36:
                return kwd
        return None

    @c36_link.setter
    def c36_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c36."""
        self.c36 = value.lcid

    @property
    def c44_link(self) -> DefineCurve:
        """Get the DefineCurve object for c44."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c44:
                return kwd
        return None

    @c44_link.setter
    def c44_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c44."""
        self.c44 = value.lcid

    @property
    def c45_link(self) -> DefineCurve:
        """Get the DefineCurve object for c45."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c45:
                return kwd
        return None

    @c45_link.setter
    def c45_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c45."""
        self.c45 = value.lcid

    @property
    def c46_link(self) -> DefineCurve:
        """Get the DefineCurve object for c46."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c46:
                return kwd
        return None

    @c46_link.setter
    def c46_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c46."""
        self.c46 = value.lcid

    @property
    def c55_link(self) -> DefineCurve:
        """Get the DefineCurve object for c55."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c55:
                return kwd
        return None

    @c55_link.setter
    def c55_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c55."""
        self.c55 = value.lcid

    @property
    def c56_link(self) -> DefineCurve:
        """Get the DefineCurve object for c56."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c56:
                return kwd
        return None

    @c56_link.setter
    def c56_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c56."""
        self.c56 = value.lcid

    @property
    def c66_link(self) -> DefineCurve:
        """Get the DefineCurve object for c66."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.c66:
                return kwd
        return None

    @c66_link.setter
    def c66_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for c66."""
        self.c66 = value.lcid

    @property
    def tge_link(self) -> DefineCurve:
        """Get the DefineCurve object for tge."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tge:
                return kwd
        return None

    @tge_link.setter
    def tge_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tge."""
        self.tge = value.lcid

