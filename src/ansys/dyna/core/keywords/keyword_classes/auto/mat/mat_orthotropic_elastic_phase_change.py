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

"""Module providing the MatOrthotropicElasticPhaseChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATORTHOTROPICELASTICPHASECHANGE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ea1", float, 20, 10, None),
    FieldSchema("eb1", float, 30, 10, None),
    FieldSchema("ec1", float, 40, 10, None),
    FieldSchema("prba1", float, 50, 10, None),
    FieldSchema("prca1", float, 60, 10, None),
    FieldSchema("prcb1", float, 70, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD1 = (
    FieldSchema("gab1", float, 0, 10, None),
    FieldSchema("gbc1", float, 10, 10, None),
    FieldSchema("gca1", float, 20, 10, None),
    FieldSchema("aopt1", float, 30, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("a11", float, 30, 10, None),
    FieldSchema("a21", float, 40, 10, None),
    FieldSchema("a31", float, 50, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD3 = (
    FieldSchema("v11", float, 0, 10, None),
    FieldSchema("v21", float, 10, 10, None),
    FieldSchema("v31", float, 20, 10, None),
    FieldSchema("d11", float, 30, 10, None),
    FieldSchema("d21", float, 40, 10, None),
    FieldSchema("d31", float, 50, 10, None),
    FieldSchema("beta1", float, 60, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD4 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("r02", float, 10, 10, None),
    FieldSchema("ea2", float, 20, 10, None),
    FieldSchema("eb2", float, 30, 10, None),
    FieldSchema("ec2", float, 40, 10, None),
    FieldSchema("prba2", float, 50, 10, None),
    FieldSchema("prca2", float, 60, 10, None),
    FieldSchema("prcb2", float, 70, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD5 = (
    FieldSchema("gab2", float, 0, 10, None),
    FieldSchema("gbc2", float, 10, 10, None),
    FieldSchema("gca2", float, 20, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD6 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("a12", float, 30, 10, None),
    FieldSchema("a22", float, 40, 10, None),
    FieldSchema("a32", float, 50, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD7 = (
    FieldSchema("v12", float, 0, 10, None),
    FieldSchema("v22", float, 10, 10, None),
    FieldSchema("v32", float, 20, 10, None),
    FieldSchema("d12", float, 30, 10, None),
    FieldSchema("d22", float, 40, 10, None),
    FieldSchema("d32", float, 50, 10, None),
    FieldSchema("beta2", float, 60, 10, None),
)

_MATORTHOTROPICELASTICPHASECHANGE_CARD8 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
    FieldSchema("x2", float, 30, 10, None),
    FieldSchema("y2", float, 40, 10, None),
    FieldSchema("z2", float, 50, 10, None),
    FieldSchema("thkfac", float, 60, 10, 1.0),
)

_MATORTHOTROPICELASTICPHASECHANGE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatOrthotropicElasticPhaseChange(KeywordBase):
    """DYNA MAT_ORTHOTROPIC_ELASTIC_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "ORTHOTROPIC_ELASTIC_PHASE_CHANGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatOrthotropicElasticPhaseChange class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICELASTICPHASECHANGE_CARD8,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatOrthotropicElasticPhaseChange.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATORTHOTROPICELASTICPHASECHANGE_OPTION0_CARD0,
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
    def ea1(self) -> typing.Optional[float]:
        """Get or set the Ea , Young's modulus in a-direction for phase i.
        """ # nopep8
        return self._cards[0].get_value("ea1")

    @ea1.setter
    def ea1(self, value: float) -> None:
        """Set the ea1 property."""
        self._cards[0].set_value("ea1", value)

    @property
    def eb1(self) -> typing.Optional[float]:
        """Get or set the Eb , Young's modulus in b-direction for phase i
        """ # nopep8
        return self._cards[0].get_value("eb1")

    @eb1.setter
    def eb1(self, value: float) -> None:
        """Set the eb1 property."""
        self._cards[0].set_value("eb1", value)

    @property
    def ec1(self) -> typing.Optional[float]:
        """Get or set the Ec , Young's modulus in c-direction for phase i
        """ # nopep8
        return self._cards[0].get_value("ec1")

    @ec1.setter
    def ec1(self, value: float) -> None:
        """Set the ec1 property."""
        self._cards[0].set_value("ec1", value)

    @property
    def prba1(self) -> typing.Optional[float]:
        """Get or set the Vba , Poisson's ratio in the ba direction for phase i.
        """ # nopep8
        return self._cards[0].get_value("prba1")

    @prba1.setter
    def prba1(self, value: float) -> None:
        """Set the prba1 property."""
        self._cards[0].set_value("prba1", value)

    @property
    def prca1(self) -> typing.Optional[float]:
        """Get or set the Vca , Poisson's ratio in the ca direction for phase i
        """ # nopep8
        return self._cards[0].get_value("prca1")

    @prca1.setter
    def prca1(self, value: float) -> None:
        """Set the prca1 property."""
        self._cards[0].set_value("prca1", value)

    @property
    def prcb1(self) -> typing.Optional[float]:
        """Get or set the Vcb , Poisson's ratio in the cb direction for phase i
        """ # nopep8
        return self._cards[0].get_value("prcb1")

    @prcb1.setter
    def prcb1(self, value: float) -> None:
        """Set the prcb1 property."""
        self._cards[0].set_value("prcb1", value)

    @property
    def gab1(self) -> typing.Optional[float]:
        """Get or set the Gab , shear modulus in the ab direction for phase i..
        """ # nopep8
        return self._cards[1].get_value("gab1")

    @gab1.setter
    def gab1(self, value: float) -> None:
        """Set the gab1 property."""
        self._cards[1].set_value("gab1", value)

    @property
    def gbc1(self) -> typing.Optional[float]:
        """Get or set the Gbc , shear modulus in the bc direction for phase i
        """ # nopep8
        return self._cards[1].get_value("gbc1")

    @gbc1.setter
    def gbc1(self, value: float) -> None:
        """Set the gbc1 property."""
        self._cards[1].set_value("gbc1", value)

    @property
    def gca1(self) -> typing.Optional[float]:
        """Get or set the Gca , shear modulus in the ab direction for phase i
        """ # nopep8
        return self._cards[1].get_value("gca1")

    @gca1.setter
    def gca1(self, value: float) -> None:
        """Set the gca1 property."""
        self._cards[1].set_value("gca1", value)

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
        return self._cards[1].get_value("aopt1")

    @aopt1.setter
    def aopt1(self, value: float) -> None:
        """Set the aopt1 property."""
        self._cards[1].set_value("aopt1", value)

    @property
    def a11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a11")

    @a11.setter
    def a11(self, value: float) -> None:
        """Set the a11 property."""
        self._cards[2].set_value("a11", value)

    @property
    def a21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a21")

    @a21.setter
    def a21(self, value: float) -> None:
        """Set the a21 property."""
        self._cards[2].set_value("a21", value)

    @property
    def a31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a31")

    @a31.setter
    def a31(self, value: float) -> None:
        """Set the a31 property."""
        self._cards[2].set_value("a31", value)

    @property
    def v11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v11")

    @v11.setter
    def v11(self, value: float) -> None:
        """Set the v11 property."""
        self._cards[3].set_value("v11", value)

    @property
    def v21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[3].get_value("v21")

    @v21.setter
    def v21(self, value: float) -> None:
        """Set the v21 property."""
        self._cards[3].set_value("v21", value)

    @property
    def v31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[3].get_value("v31")

    @v31.setter
    def v31(self, value: float) -> None:
        """Set the v31 property."""
        self._cards[3].set_value("v31", value)

    @property
    def d11(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d11")

    @d11.setter
    def d11(self, value: float) -> None:
        """Set the d11 property."""
        self._cards[3].set_value("d11", value)

    @property
    def d21(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d21")

    @d21.setter
    def d21(self, value: float) -> None:
        """Set the d21 property."""
        self._cards[3].set_value("d21", value)

    @property
    def d31(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d31")

    @d31.setter
    def d31(self, value: float) -> None:
        """Set the d31 property."""
        self._cards[3].set_value("d31", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[3].set_value("beta1", value)

    @property
    def r02(self) -> typing.Optional[float]:
        """Get or set the Mass density for phase
        """ # nopep8
        return self._cards[4].get_value("r02")

    @r02.setter
    def r02(self, value: float) -> None:
        """Set the r02 property."""
        self._cards[4].set_value("r02", value)

    @property
    def ea2(self) -> typing.Optional[float]:
        """Get or set the Ea , Young's modulus in a-direction for phase i.
        """ # nopep8
        return self._cards[4].get_value("ea2")

    @ea2.setter
    def ea2(self, value: float) -> None:
        """Set the ea2 property."""
        self._cards[4].set_value("ea2", value)

    @property
    def eb2(self) -> typing.Optional[float]:
        """Get or set the Eb , Young's modulus in b-direction for phase i
        """ # nopep8
        return self._cards[4].get_value("eb2")

    @eb2.setter
    def eb2(self, value: float) -> None:
        """Set the eb2 property."""
        self._cards[4].set_value("eb2", value)

    @property
    def ec2(self) -> typing.Optional[float]:
        """Get or set the Ec , Young's modulus in c-direction for phase i
        """ # nopep8
        return self._cards[4].get_value("ec2")

    @ec2.setter
    def ec2(self, value: float) -> None:
        """Set the ec2 property."""
        self._cards[4].set_value("ec2", value)

    @property
    def prba2(self) -> typing.Optional[float]:
        """Get or set the Vba , Poisson's ratio in the ba direction for phase i.
        """ # nopep8
        return self._cards[4].get_value("prba2")

    @prba2.setter
    def prba2(self, value: float) -> None:
        """Set the prba2 property."""
        self._cards[4].set_value("prba2", value)

    @property
    def prca2(self) -> typing.Optional[float]:
        """Get or set the Vca , Poisson's ratio in the ca direction for phase i
        """ # nopep8
        return self._cards[4].get_value("prca2")

    @prca2.setter
    def prca2(self, value: float) -> None:
        """Set the prca2 property."""
        self._cards[4].set_value("prca2", value)

    @property
    def prcb2(self) -> typing.Optional[float]:
        """Get or set the Vcb , Poisson's ratio in the cb direction for phase i
        """ # nopep8
        return self._cards[4].get_value("prcb2")

    @prcb2.setter
    def prcb2(self, value: float) -> None:
        """Set the prcb2 property."""
        self._cards[4].set_value("prcb2", value)

    @property
    def gab2(self) -> typing.Optional[float]:
        """Get or set the Gab , shear modulus in the ab direction for phase i..
        """ # nopep8
        return self._cards[5].get_value("gab2")

    @gab2.setter
    def gab2(self, value: float) -> None:
        """Set the gab2 property."""
        self._cards[5].set_value("gab2", value)

    @property
    def gbc2(self) -> typing.Optional[float]:
        """Get or set the Gbc , shear modulus in the bc direction for phase i
        """ # nopep8
        return self._cards[5].get_value("gbc2")

    @gbc2.setter
    def gbc2(self, value: float) -> None:
        """Set the gbc2 property."""
        self._cards[5].set_value("gbc2", value)

    @property
    def gca2(self) -> typing.Optional[float]:
        """Get or set the Gca , shear modulus in the ab direction for phase i
        """ # nopep8
        return self._cards[5].get_value("gca2")

    @gca2.setter
    def gca2(self, value: float) -> None:
        """Set the gca2 property."""
        self._cards[5].set_value("gca2", value)

    @property
    def a12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a12")

    @a12.setter
    def a12(self, value: float) -> None:
        """Set the a12 property."""
        self._cards[6].set_value("a12", value)

    @property
    def a22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a22")

    @a22.setter
    def a22(self, value: float) -> None:
        """Set the a22 property."""
        self._cards[6].set_value("a22", value)

    @property
    def a32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a32")

    @a32.setter
    def a32(self, value: float) -> None:
        """Set the a32 property."""
        self._cards[6].set_value("a32", value)

    @property
    def v12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v12")

    @v12.setter
    def v12(self, value: float) -> None:
        """Set the v12 property."""
        self._cards[7].set_value("v12", value)

    @property
    def v22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[7].get_value("v22")

    @v22.setter
    def v22(self, value: float) -> None:
        """Set the v22 property."""
        self._cards[7].set_value("v22", value)

    @property
    def v32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[7].get_value("v32")

    @v32.setter
    def v32(self, value: float) -> None:
        """Set the v32 property."""
        self._cards[7].set_value("v32", value)

    @property
    def d12(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d12")

    @d12.setter
    def d12(self, value: float) -> None:
        """Set the d12 property."""
        self._cards[7].set_value("d12", value)

    @property
    def d22(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d22")

    @d22.setter
    def d22(self, value: float) -> None:
        """Set the d22 property."""
        self._cards[7].set_value("d22", value)

    @property
    def d32(self) -> typing.Optional[float]:
        """Get or set the Define components of the i th phase's vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d32")

    @d32.setter
    def d32(self, value: float) -> None:
        """Set the d32 property."""
        self._cards[7].set_value("d32", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
        overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[7].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[7].set_value("beta2", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[8].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[8].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[8].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[8].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition page.
        """ # nopep8
        return self._cards[8].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[8].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[8].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[8].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[8].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[8].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[8].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[8].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[8].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        """Set the thkfac property."""
        self._cards[8].set_value("thkfac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[9].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

