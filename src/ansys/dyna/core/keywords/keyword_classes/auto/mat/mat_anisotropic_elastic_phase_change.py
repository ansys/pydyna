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

"""Module providing the MatAnisotropicElasticPhaseChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATANISOTROPICELASTICPHASECHANGE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("c111", float, 20, 10, None),
    FieldSchema("c121", float, 30, 10, None),
    FieldSchema("c221", float, 40, 10, None),
    FieldSchema("c131", float, 50, 10, None),
    FieldSchema("c231", float, 60, 10, None),
    FieldSchema("c331", float, 70, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD1 = (
    FieldSchema("c141", float, 0, 10, None),
    FieldSchema("c241", float, 10, 10, None),
    FieldSchema("c341", float, 20, 10, None),
    FieldSchema("c441", float, 30, 10, None),
    FieldSchema("c151", float, 40, 10, None),
    FieldSchema("c251", float, 50, 10, None),
    FieldSchema("c351", float, 60, 10, None),
    FieldSchema("c451", float, 70, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD2 = (
    FieldSchema("c551", float, 0, 10, None),
    FieldSchema("c161", float, 10, 10, None),
    FieldSchema("c261", float, 20, 10, None),
    FieldSchema("c361", float, 30, 10, None),
    FieldSchema("c461", float, 40, 10, None),
    FieldSchema("c561", float, 50, 10, None),
    FieldSchema("c661", float, 60, 10, None),
    FieldSchema("aopt1", float, 70, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD3 = (
    FieldSchema("xp1", float, 0, 10, None),
    FieldSchema("yp1", float, 10, 10, None),
    FieldSchema("zp1", float, 20, 10, None),
    FieldSchema("a11", float, 30, 10, None),
    FieldSchema("a21", float, 40, 10, None),
    FieldSchema("a31", float, 50, 10, None),
    FieldSchema("macf", int, 60, 10, 1),
    FieldSchema("ihis", int, 70, 10, 0),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD4 = (
    FieldSchema("v11", float, 0, 10, None),
    FieldSchema("v21", float, 10, 10, None),
    FieldSchema("v31", float, 20, 10, None),
    FieldSchema("d11", float, 30, 10, None),
    FieldSchema("d21", float, 40, 10, None),
    FieldSchema("d31", float, 50, 10, None),
    FieldSchema("beta1", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, 0.0),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD5 = (
    FieldSchema("unused", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("c112", float, 20, 10, None),
    FieldSchema("c122", float, 30, 10, None),
    FieldSchema("c222", float, 40, 10, None),
    FieldSchema("c132", float, 50, 10, None),
    FieldSchema("c232", float, 60, 10, None),
    FieldSchema("c332", float, 70, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD6 = (
    FieldSchema("c142", float, 0, 10, None),
    FieldSchema("c242", float, 10, 10, None),
    FieldSchema("c342", float, 20, 10, None),
    FieldSchema("c442", float, 30, 10, None),
    FieldSchema("c152", float, 40, 10, None),
    FieldSchema("c252", float, 50, 10, None),
    FieldSchema("c352", float, 60, 10, None),
    FieldSchema("c452", float, 70, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD7 = (
    FieldSchema("c552", float, 0, 10, None),
    FieldSchema("c162", float, 10, 10, None),
    FieldSchema("c262", float, 20, 10, None),
    FieldSchema("c362", float, 30, 10, None),
    FieldSchema("c462", float, 40, 10, None),
    FieldSchema("c562", float, 50, 10, None),
    FieldSchema("c662", float, 60, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD8 = (
    FieldSchema("xp2", float, 0, 10, None),
    FieldSchema("yp2", float, 10, 10, None),
    FieldSchema("zp2", float, 20, 10, None),
    FieldSchema("a12", float, 30, 10, None),
    FieldSchema("a22", float, 40, 10, None),
    FieldSchema("a32", float, 50, 10, None),
    FieldSchema("xp2", float, 0, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD9 = (
    FieldSchema("v12", float, 0, 10, None),
    FieldSchema("v22", float, 10, 10, None),
    FieldSchema("v32", float, 20, 10, None),
    FieldSchema("d12", float, 30, 10, None),
    FieldSchema("d22", float, 40, 10, None),
    FieldSchema("d32", float, 50, 10, None),
    FieldSchema("beta2", float, 60, 10, None),
)

_MATANISOTROPICELASTICPHASECHANGE_CARD10 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
    FieldSchema("x2", float, 30, 10, None),
    FieldSchema("y2", float, 40, 10, None),
    FieldSchema("z2", float, 50, 10, None),
    FieldSchema("thkfac", float, 60, 10, 1.0),
)

_MATANISOTROPICELASTICPHASECHANGE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAnisotropicElasticPhaseChange(KeywordBase):
    """DYNA MAT_ANISOTROPIC_ELASTIC_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "ANISOTROPIC_ELASTIC_PHASE_CHANGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAnisotropicElasticPhaseChange class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATANISOTROPICELASTICPHASECHANGE_CARD10,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAnisotropicElasticPhaseChange.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATANISOTROPICELASTICPHASECHANGE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def c111(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 1 corresponds to the a material direction
        """ # nopep8
        return self._cards[0].get_value("c111")

    @c111.setter
    def c111(self, value: float) -> None:
        """Set the c111 property."""
        self._cards[0].set_value("c111", value)

    @property
    def c121(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 2 corresponds to the b material direction
        """ # nopep8
        return self._cards[0].get_value("c121")

    @c121.setter
    def c121(self, value: float) -> None:
        """Set the c121 property."""
        self._cards[0].set_value("c121", value)

    @property
    def c221(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c221")

    @c221.setter
    def c221(self, value: float) -> None:
        """Set the c221 property."""
        self._cards[0].set_value("c221", value)

    @property
    def c131(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c131")

    @c131.setter
    def c131(self, value: float) -> None:
        """Set the c131 property."""
        self._cards[0].set_value("c131", value)

    @property
    def c231(self) -> typing.Optional[float]:
        """Get or set the The 2,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c231")

    @c231.setter
    def c231(self, value: float) -> None:
        """Set the c231 property."""
        self._cards[0].set_value("c231", value)

    @property
    def c331(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[0].get_value("c331")

    @c331.setter
    def c331(self, value: float) -> None:
        """Set the c331 property."""
        self._cards[0].set_value("c331", value)

    @property
    def c141(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[1].get_value("c141")

    @c141.setter
    def c141(self, value: float) -> None:
        """Set the c141 property."""
        self._cards[1].set_value("c141", value)

    @property
    def c241(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c241")

    @c241.setter
    def c241(self, value: float) -> None:
        """Set the c241 property."""
        self._cards[1].set_value("c241", value)

    @property
    def c341(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c341")

    @c341.setter
    def c341(self, value: float) -> None:
        """Set the c341 property."""
        self._cards[1].set_value("c341", value)

    @property
    def c441(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c441")

    @c441.setter
    def c441(self, value: float) -> None:
        """Set the c441 property."""
        self._cards[1].set_value("c441", value)

    @property
    def c151(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c151")

    @c151.setter
    def c151(self, value: float) -> None:
        """Set the c151 property."""
        self._cards[1].set_value("c151", value)

    @property
    def c251(self) -> typing.Optional[float]:
        """Get or set the The 2, 5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c251")

    @c251.setter
    def c251(self, value: float) -> None:
        """Set the c251 property."""
        self._cards[1].set_value("c251", value)

    @property
    def c351(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c351")

    @c351.setter
    def c351(self, value: float) -> None:
        """Set the c351 property."""
        self._cards[1].set_value("c351", value)

    @property
    def c451(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[1].get_value("c451")

    @c451.setter
    def c451(self, value: float) -> None:
        """Set the c451 property."""
        self._cards[1].set_value("c451", value)

    @property
    def c551(self) -> typing.Optional[float]:
        """Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[2].get_value("c551")

    @c551.setter
    def c551(self, value: float) -> None:
        """Set the c551 property."""
        self._cards[2].set_value("c551", value)

    @property
    def c161(self) -> typing.Optional[float]:
        """Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c161")

    @c161.setter
    def c161(self, value: float) -> None:
        """Set the c161 property."""
        self._cards[2].set_value("c161", value)

    @property
    def c261(self) -> typing.Optional[float]:
        """Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c261")

    @c261.setter
    def c261(self, value: float) -> None:
        """Set the c261 property."""
        self._cards[2].set_value("c261", value)

    @property
    def c361(self) -> typing.Optional[float]:
        """Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c361")

    @c361.setter
    def c361(self, value: float) -> None:
        """Set the c361 property."""
        self._cards[2].set_value("c361", value)

    @property
    def c461(self) -> typing.Optional[float]:
        """Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c461")

    @c461.setter
    def c461(self, value: float) -> None:
        """Set the c461 property."""
        self._cards[2].set_value("c461", value)

    @property
    def c561(self) -> typing.Optional[float]:
        """Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c561")

    @c561.setter
    def c561(self, value: float) -> None:
        """Set the c561 property."""
        self._cards[2].set_value("c561", value)

    @property
    def c661(self) -> typing.Optional[float]:
        """Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[2].get_value("c661")

    @c661.setter
    def c661(self, value: float) -> None:
        """Set the c661 property."""
        self._cards[2].set_value("c661", value)

    @property
    def aopt1(self) -> typing.Optional[float]:
        """Get or set the Material axes option for phase i, see Figure M2-1.
        EQ.0.0: locally orthotropic with material axes determined by element nodes as shown in part (a) of Figure M2-1. The
        a-direction is from node 1 to node 2 of the element. The b-direction is orthogonal to the a-direction and is in the
        plane formed by nodes 1, 2, and 4. When this option is used in two-dimensional planar and axisymmetric analysis,
        it is critical that the nodes in the element definition be numbered counterclockwise for this option to work correctly.
        EQ.1.0: locally orthotropic with material axes determined by a
        point in space and the global location of the element center; this is the a-direction. This option is for solid elements only.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.
        The plane of a solid element is the midsurface between the inner surface and outer surface defined by the
        first four nodes and the last four nodes of the connectivity of the element, respectively.
        EQ.4.0: locally orthotropic in cylindrical coordinate system with
        the material axes determined by a vector v, and an originating point, P, which define the centerline axis. This option is for solid elements only.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR). Available in R3 version of 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt1")

    @aopt1.setter
    def aopt1(self, value: float) -> None:
        """Set the aopt1 property."""
        self._cards[2].set_value("aopt1", value)

    @property
    def xp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("xp1")

    @xp1.setter
    def xp1(self, value: float) -> None:
        """Set the xp1 property."""
        self._cards[3].set_value("xp1", value)

    @property
    def yp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("yp1")

    @yp1.setter
    def yp1(self, value: float) -> None:
        """Set the yp1 property."""
        self._cards[3].set_value("yp1", value)

    @property
    def zp1(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("zp1")

    @zp1.setter
    def zp1(self, value: float) -> None:
        """Set the zp1 property."""
        self._cards[3].set_value("zp1", value)

    @property
    def a11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a11")

    @a11.setter
    def a11(self, value: float) -> None:
        """Set the a11 property."""
        self._cards[3].set_value("a11", value)

    @property
    def a21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a21")

    @a21.setter
    def a21(self, value: float) -> None:
        """Set the a21 property."""
        self._cards[3].set_value("a21", value)

    @property
    def a31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a31")

    @a31.setter
    def a31(self, value: float) -> None:
        """Set the a31 property."""
        self._cards[3].set_value("a31", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements in phase i:
        EQ.1: No change, default,
        EQ.2: switch material axes a and b,
        EQ.3: switch material axes a and c,
        EQ.4: switch material axes b and c.
        """ # nopep8
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4}.""")
        self._cards[3].set_value("macf", value)

    @property
    def ihis(self) -> int:
        """Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only).
        EQ.0: C11, C12, … from Cards 1, 2, and 3 are used.
        EQ.1: C11, C12, … are initialized by *INITIAL_STRESS_SOLID's	history data.
        """ # nopep8
        return self._cards[3].get_value("ihis")

    @ihis.setter
    def ihis(self, value: int) -> None:
        """Set the ihis property."""
        if value not in [0, 1, None]:
            raise Exception("""ihis must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ihis", value)

    @property
    def v11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v11")

    @v11.setter
    def v11(self, value: float) -> None:
        """Set the v11 property."""
        self._cards[4].set_value("v11", value)

    @property
    def v21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[4].get_value("v21")

    @v21.setter
    def v21(self, value: float) -> None:
        """Set the v21 property."""
        self._cards[4].set_value("v21", value)

    @property
    def v31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[4].get_value("v31")

    @v31.setter
    def v31(self, value: float) -> None:
        """Set the v31 property."""
        self._cards[4].set_value("v31", value)

    @property
    def d11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d11")

    @d11.setter
    def d11(self, value: float) -> None:
        """Set the d11 property."""
        self._cards[4].set_value("d11", value)

    @property
    def d21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d21")

    @d21.setter
    def d21(self, value: float) -> None:
        """Set the d21 property."""
        self._cards[4].set_value("d21", value)

    @property
    def d31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d31")

    @d31.setter
    def d31(self, value: float) -> None:
        """Set the d31 property."""
        self._cards[4].set_value("d31", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[4].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[4].set_value("beta1", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor for the i th phase.
        The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[4].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[4].set_value("ref", value)

    @property
    def c112(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c112")

    @c112.setter
    def c112(self, value: float) -> None:
        """Set the c112 property."""
        self._cards[5].set_value("c112", value)

    @property
    def c122(self) -> typing.Optional[float]:
        """Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c122")

    @c122.setter
    def c122(self, value: float) -> None:
        """Set the c122 property."""
        self._cards[5].set_value("c122", value)

    @property
    def c222(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c222")

    @c222.setter
    def c222(self, value: float) -> None:
        """Set the c222 property."""
        self._cards[5].set_value("c222", value)

    @property
    def c132(self) -> typing.Optional[float]:
        """Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c132")

    @c132.setter
    def c132(self, value: float) -> None:
        """Set the c132 property."""
        self._cards[5].set_value("c132", value)

    @property
    def c232(self) -> typing.Optional[float]:
        """Get or set the The 2, 3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c232")

    @c232.setter
    def c232(self, value: float) -> None:
        """Set the c232 property."""
        self._cards[5].set_value("c232", value)

    @property
    def c332(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[5].get_value("c332")

    @c332.setter
    def c332(self, value: float) -> None:
        """Set the c332 property."""
        self._cards[5].set_value("c332", value)

    @property
    def c142(self) -> typing.Optional[float]:
        """Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[6].get_value("c142")

    @c142.setter
    def c142(self, value: float) -> None:
        """Set the c142 property."""
        self._cards[6].set_value("c142", value)

    @property
    def c242(self) -> typing.Optional[float]:
        """Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c242")

    @c242.setter
    def c242(self, value: float) -> None:
        """Set the c242 property."""
        self._cards[6].set_value("c242", value)

    @property
    def c342(self) -> typing.Optional[float]:
        """Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c342")

    @c342.setter
    def c342(self, value: float) -> None:
        """Set the c342 property."""
        self._cards[6].set_value("c342", value)

    @property
    def c442(self) -> typing.Optional[float]:
        """Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c442")

    @c442.setter
    def c442(self, value: float) -> None:
        """Set the c442 property."""
        self._cards[6].set_value("c442", value)

    @property
    def c152(self) -> typing.Optional[float]:
        """Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c152")

    @c152.setter
    def c152(self, value: float) -> None:
        """Set the c152 property."""
        self._cards[6].set_value("c152", value)

    @property
    def c252(self) -> typing.Optional[float]:
        """Get or set the The 2,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c252")

    @c252.setter
    def c252(self, value: float) -> None:
        """Set the c252 property."""
        self._cards[6].set_value("c252", value)

    @property
    def c352(self) -> typing.Optional[float]:
        """Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c352")

    @c352.setter
    def c352(self, value: float) -> None:
        """Set the c352 property."""
        self._cards[6].set_value("c352", value)

    @property
    def c452(self) -> typing.Optional[float]:
        """Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[6].get_value("c452")

    @c452.setter
    def c452(self, value: float) -> None:
        """Set the c452 property."""
        self._cards[6].set_value("c452", value)

    @property
    def c552(self) -> typing.Optional[float]:
        """Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
        """ # nopep8
        return self._cards[7].get_value("c552")

    @c552.setter
    def c552(self, value: float) -> None:
        """Set the c552 property."""
        self._cards[7].set_value("c552", value)

    @property
    def c162(self) -> typing.Optional[float]:
        """Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c162")

    @c162.setter
    def c162(self, value: float) -> None:
        """Set the c162 property."""
        self._cards[7].set_value("c162", value)

    @property
    def c262(self) -> typing.Optional[float]:
        """Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c262")

    @c262.setter
    def c262(self, value: float) -> None:
        """Set the c262 property."""
        self._cards[7].set_value("c262", value)

    @property
    def c362(self) -> typing.Optional[float]:
        """Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c362")

    @c362.setter
    def c362(self, value: float) -> None:
        """Set the c362 property."""
        self._cards[7].set_value("c362", value)

    @property
    def c462(self) -> typing.Optional[float]:
        """Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c462")

    @c462.setter
    def c462(self, value: float) -> None:
        """Set the c462 property."""
        self._cards[7].set_value("c462", value)

    @property
    def c562(self) -> typing.Optional[float]:
        """Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c562")

    @c562.setter
    def c562(self, value: float) -> None:
        """Set the c562 property."""
        self._cards[7].set_value("c562", value)

    @property
    def c662(self) -> typing.Optional[float]:
        """Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
        """ # nopep8
        return self._cards[7].get_value("c662")

    @c662.setter
    def c662(self, value: float) -> None:
        """Set the c662 property."""
        self._cards[7].set_value("c662", value)

    @property
    def xp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[8].get_value("xp2")

    @xp2.setter
    def xp2(self, value: float) -> None:
        """Set the xp2 property."""
        self._cards[8].set_value("xp2", value)

    @property
    def yp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[8].get_value("yp2")

    @yp2.setter
    def yp2(self, value: float) -> None:
        """Set the yp2 property."""
        self._cards[8].set_value("yp2", value)

    @property
    def zp2(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
        """ # nopep8
        return self._cards[8].get_value("zp2")

    @zp2.setter
    def zp2(self, value: float) -> None:
        """Set the zp2 property."""
        self._cards[8].set_value("zp2", value)

    @property
    def a12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("a12")

    @a12.setter
    def a12(self, value: float) -> None:
        """Set the a12 property."""
        self._cards[8].set_value("a12", value)

    @property
    def a22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[8].get_value("a22")

    @a22.setter
    def a22(self, value: float) -> None:
        """Set the a22 property."""
        self._cards[8].set_value("a22", value)

    @property
    def a32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[8].get_value("a32")

    @a32.setter
    def a32(self, value: float) -> None:
        """Set the a32 property."""
        self._cards[8].set_value("a32", value)

    @property
    def xp2(self) -> typing.Optional[float]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[8].get_value("xp2")

    @xp2.setter
    def xp2(self, value: float) -> None:
        """Set the xp2 property."""
        self._cards[8].set_value("xp2", value)

    @property
    def v12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[9].get_value("v12")

    @v12.setter
    def v12(self, value: float) -> None:
        """Set the v12 property."""
        self._cards[9].set_value("v12", value)

    @property
    def v22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[9].get_value("v22")

    @v22.setter
    def v22(self, value: float) -> None:
        """Set the v22 property."""
        self._cards[9].set_value("v22", value)

    @property
    def v32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[9].get_value("v32")

    @v32.setter
    def v32(self, value: float) -> None:
        """Set the v32 property."""
        self._cards[9].set_value("v32", value)

    @property
    def d12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d12")

    @d12.setter
    def d12(self, value: float) -> None:
        """Set the d12 property."""
        self._cards[9].set_value("d12", value)

    @property
    def d22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d22")

    @d22.setter
    def d22(self, value: float) -> None:
        """Set the d22 property."""
        self._cards[9].set_value("d22", value)

    @property
    def d32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[9].get_value("d32")

    @d32.setter
    def d32(self, value: float) -> None:
        """Set the d32 property."""
        self._cards[9].set_value("d32", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[9].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[9].set_value("beta2", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[10].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[10].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[10].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[10].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[10].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[10].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[10].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[10].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[10].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[10].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[10].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        """Set the thkfac property."""
        self._cards[10].set_value("thkfac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[11].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[11].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

