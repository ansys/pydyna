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

"""Module providing the MatPaper class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATPAPER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e1", float, 20, 10, None),
    FieldSchema("e2", float, 30, 10, None),
    FieldSchema("e3", float, 40, 10, None),
    FieldSchema("pr21", float, 50, 10, None),
    FieldSchema("pr32", float, 60, 10, None),
    FieldSchema("pr31", float, 70, 10, None),
)

_MATPAPER_CARD1 = (
    FieldSchema("g12", float, 0, 10, None),
    FieldSchema("g23", float, 10, 10, None),
    FieldSchema("g13", float, 20, 10, None),
    FieldSchema("e3c", float, 30, 10, None),
    FieldSchema("cc", float, 40, 10, None),
    FieldSchema("twok", float, 50, 10, None),
)

_MATPAPER_CARD2 = (
    FieldSchema("s01", float, 0, 10, None),
    FieldSchema("a01", float, 10, 10, None),
    FieldSchema("b01", float, 20, 10, None),
    FieldSchema("c01", float, 30, 10, None),
    FieldSchema("s02", float, 40, 10, None),
    FieldSchema("a02", float, 50, 10, None),
    FieldSchema("b02", float, 60, 10, None),
    FieldSchema("c02", float, 70, 10, None),
)

_MATPAPER_CARD3 = (
    FieldSchema("s03", float, 0, 10, None),
    FieldSchema("a03", float, 10, 10, None),
    FieldSchema("b03", float, 20, 10, None),
    FieldSchema("c03", float, 30, 10, None),
    FieldSchema("s04", float, 40, 10, None),
    FieldSchema("a04", float, 50, 10, None),
    FieldSchema("b04", float, 60, 10, None),
    FieldSchema("c04", float, 70, 10, None),
)

_MATPAPER_CARD4 = (
    FieldSchema("s05", float, 0, 10, None),
    FieldSchema("a05", float, 10, 10, None),
    FieldSchema("b05", float, 20, 10, None),
    FieldSchema("c05", float, 30, 10, None),
    FieldSchema("prp1", float, 40, 10, 0.5),
    FieldSchema("prp2", float, 50, 10, 0.133),
    FieldSchema("prp4", float, 60, 10, 0.5),
    FieldSchema("prp5", float, 70, 10, 0.133),
)

_MATPAPER_CARD5 = (
    FieldSchema("asig", float, 0, 10, None),
    FieldSchema("bsig", float, 10, 10, None),
    FieldSchema("csig", float, 20, 10, None),
    FieldSchema("tau0", float, 30, 10, None),
    FieldSchema("atau", float, 40, 10, None),
    FieldSchema("btau", float, 50, 10, None),
)

_MATPAPER_CARD6 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("macf", int, 10, 10, 1),
    FieldSchema("xp", float, 20, 10, None),
    FieldSchema("yp", float, 30, 10, None),
    FieldSchema("zp", float, 40, 10, None),
    FieldSchema("a1", float, 50, 10, None),
    FieldSchema("a2", float, 60, 10, None),
    FieldSchema("a3", float, 70, 10, None),
)

_MATPAPER_CARD7 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATPAPER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatPaper(KeywordBase):
    """DYNA MAT_PAPER keyword"""

    keyword = "MAT"
    subkeyword = "PAPER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatPaper class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPAPER_CARD7,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatPaper.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATPAPER_OPTION0_CARD0,
                        **kwargs,
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
    def e1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E1.
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E2.
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[0].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in direction E3
        """ # nopep8
        return self._cards[0].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        """Set the e3 property."""
        self._cards[0].set_value("e3", value)

    @property
    def pr21(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V21
        """ # nopep8
        return self._cards[0].get_value("pr21")

    @pr21.setter
    def pr21(self, value: float) -> None:
        """Set the pr21 property."""
        self._cards[0].set_value("pr21", value)

    @property
    def pr32(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V32
        """ # nopep8
        return self._cards[0].get_value("pr32")

    @pr32.setter
    def pr32(self, value: float) -> None:
        """Set the pr32 property."""
        self._cards[0].set_value("pr32", value)

    @property
    def pr31(self) -> typing.Optional[float]:
        """Get or set the Elastic Poisson's ratio V31
        """ # nopep8
        return self._cards[0].get_value("pr31")

    @pr31.setter
    def pr31(self, value: float) -> None:
        """Set the pr31 property."""
        self._cards[0].set_value("pr31", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G12.
        """ # nopep8
        return self._cards[1].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        """Set the g12 property."""
        self._cards[1].set_value("g12", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G23.
        """ # nopep8
        return self._cards[1].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        """Set the g23 property."""
        self._cards[1].set_value("g23", value)

    @property
    def g13(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in direction G13.
        """ # nopep8
        return self._cards[1].get_value("g13")

    @g13.setter
    def g13(self, value: float) -> None:
        """Set the g13 property."""
        self._cards[1].set_value("g13", value)

    @property
    def e3c(self) -> typing.Optional[float]:
        """Get or set the Elastic compression parameter.
        """ # nopep8
        return self._cards[1].get_value("e3c")

    @e3c.setter
    def e3c(self, value: float) -> None:
        """Set the e3c property."""
        self._cards[1].set_value("e3c", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Elastic compression exponent
        """ # nopep8
        return self._cards[1].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        """Set the cc property."""
        self._cards[1].set_value("cc", value)

    @property
    def twok(self) -> typing.Optional[float]:
        """Get or set the Exponent in in-plane yield surface
        """ # nopep8
        return self._cards[1].get_value("twok")

    @twok.setter
    def twok(self, value: float) -> None:
        """Set the twok property."""
        self._cards[1].set_value("twok", value)

    @property
    def s01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[2].get_value("s01")

    @s01.setter
    def s01(self, value: float) -> None:
        """Set the s01 property."""
        self._cards[2].set_value("s01", value)

    @property
    def a01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("a01")

    @a01.setter
    def a01(self, value: float) -> None:
        """Set the a01 property."""
        self._cards[2].set_value("a01", value)

    @property
    def b01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("b01")

    @b01.setter
    def b01(self, value: float) -> None:
        """Set the b01 property."""
        self._cards[2].set_value("b01", value)

    @property
    def c01(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[2].get_value("c01")

    @c01.setter
    def c01(self, value: float) -> None:
        """Set the c01 property."""
        self._cards[2].set_value("c01", value)

    @property
    def s02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
        """ # nopep8
        return self._cards[2].get_value("s02")

    @s02.setter
    def s02(self, value: float) -> None:
        """Set the s02 property."""
        self._cards[2].set_value("s02", value)

    @property
    def a02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("a02")

    @a02.setter
    def a02(self, value: float) -> None:
        """Set the a02 property."""
        self._cards[2].set_value("a02", value)

    @property
    def b02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("b02")

    @b02.setter
    def b02(self, value: float) -> None:
        """Set the b02 property."""
        self._cards[2].set_value("b02", value)

    @property
    def c02(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[2].get_value("c02")

    @c02.setter
    def c02(self, value: float) -> None:
        """Set the c02 property."""
        self._cards[2].set_value("c02", value)

    @property
    def s03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[3].get_value("s03")

    @s03.setter
    def s03(self, value: float) -> None:
        """Set the s03 property."""
        self._cards[3].set_value("s03", value)

    @property
    def a03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("a03")

    @a03.setter
    def a03(self, value: float) -> None:
        """Set the a03 property."""
        self._cards[3].set_value("a03", value)

    @property
    def b03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("b03")

    @b03.setter
    def b03(self, value: float) -> None:
        """Set the b03 property."""
        self._cards[3].set_value("b03", value)

    @property
    def c03(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[3].get_value("c03")

    @c03.setter
    def c03(self, value: float) -> None:
        """Set the c03 property."""
        self._cards[3].set_value("c03", value)

    @property
    def s04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
        """ # nopep8
        return self._cards[3].get_value("s04")

    @s04.setter
    def s04(self, value: float) -> None:
        """Set the s04 property."""
        self._cards[3].set_value("s04", value)

    @property
    def a04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("a04")

    @a04.setter
    def a04(self, value: float) -> None:
        """Set the a04 property."""
        self._cards[3].set_value("a04", value)

    @property
    def b04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("b04")

    @b04.setter
    def b04(self, value: float) -> None:
        """Set the b04 property."""
        self._cards[3].set_value("b04", value)

    @property
    def c04(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter
        """ # nopep8
        return self._cards[3].get_value("c04")

    @c04.setter
    def c04(self, value: float) -> None:
        """Set the c04 property."""
        self._cards[3].set_value("c04", value)

    @property
    def s05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
        """ # nopep8
        return self._cards[4].get_value("s05")

    @s05.setter
    def s05(self, value: float) -> None:
        """Set the s05 property."""
        self._cards[4].set_value("s05", value)

    @property
    def a05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("a05")

    @a05.setter
    def a05(self, value: float) -> None:
        """Set the a05 property."""
        self._cards[4].set_value("a05", value)

    @property
    def b05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("b05")

    @b05.setter
    def b05(self, value: float) -> None:
        """Set the b05 property."""
        self._cards[4].set_value("b05", value)

    @property
    def c05(self) -> typing.Optional[float]:
        """Get or set the Ith in-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[4].get_value("c05")

    @c05.setter
    def c05(self, value: float) -> None:
        """Set the c05 property."""
        self._cards[4].set_value("c05", value)

    @property
    def prp1(self) -> float:
        """Get or set the Tensile plastic Poisson's ratio in direction 1
        """ # nopep8
        return self._cards[4].get_value("prp1")

    @prp1.setter
    def prp1(self, value: float) -> None:
        """Set the prp1 property."""
        self._cards[4].set_value("prp1", value)

    @property
    def prp2(self) -> float:
        """Get or set the Tensile plastic Poisson's ratio in direction 2
        """ # nopep8
        return self._cards[4].get_value("prp2")

    @prp2.setter
    def prp2(self, value: float) -> None:
        """Set the prp2 property."""
        self._cards[4].set_value("prp2", value)

    @property
    def prp4(self) -> float:
        """Get or set the Compressive plastic Poisson's ratio in direction 1
        """ # nopep8
        return self._cards[4].get_value("prp4")

    @prp4.setter
    def prp4(self, value: float) -> None:
        """Set the prp4 property."""
        self._cards[4].set_value("prp4", value)

    @property
    def prp5(self) -> float:
        """Get or set the Compressive plastic Poisson's ratio in direction 2
        """ # nopep8
        return self._cards[4].get_value("prp5")

    @prp5.setter
    def prp5(self, value: float) -> None:
        """Set the prp5 property."""
        self._cards[4].set_value("prp5", value)

    @property
    def asig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity yield parameter.
        """ # nopep8
        return self._cards[5].get_value("asig")

    @asig.setter
    def asig(self, value: float) -> None:
        """Set the asig property."""
        self._cards[5].set_value("asig", value)

    @property
    def bsig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[5].get_value("bsig")

    @bsig.setter
    def bsig(self, value: float) -> None:
        """Set the bsig property."""
        self._cards[5].set_value("bsig", value)

    @property
    def csig(self) -> typing.Optional[float]:
        """Get or set the Out-of-plane plasticity hardening parameter.
        """ # nopep8
        return self._cards[5].get_value("csig")

    @csig.setter
    def csig(self, value: float) -> None:
        """Set the csig property."""
        self._cards[5].set_value("csig", value)

    @property
    def tau0(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity yield parameter.
        """ # nopep8
        return self._cards[5].get_value("tau0")

    @tau0.setter
    def tau0(self, value: float) -> None:
        """Set the tau0 property."""
        self._cards[5].set_value("tau0", value)

    @property
    def atau(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity hardening parameter
        """ # nopep8
        return self._cards[5].get_value("atau")

    @atau.setter
    def atau(self, value: float) -> None:
        """Set the atau property."""
        self._cards[5].set_value("atau", value)

    @property
    def btau(self) -> typing.Optional[float]:
        """Get or set the Transverse shear plasticity hardening parameter
        """ # nopep8
        return self._cards[5].get_value("btau")

    @btau.setter
    def btau(self, value: float) -> None:
        """Set the btau property."""
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
        """Set the aopt property."""
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
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -4, -3, -2, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-4,-3,-2}.""")
        self._cards[6].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[6].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[6].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[6].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[6].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[6].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[6].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[7].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[7].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[7].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[7].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[7].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[7].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[7].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[7].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO
        """ # nopep8
        return self._cards[7].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[7].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

