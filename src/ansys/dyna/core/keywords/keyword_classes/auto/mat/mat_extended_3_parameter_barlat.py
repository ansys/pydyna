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

"""Module providing the MatExtended3ParameterBarlat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATEXTENDED3PARAMETERBARLAT_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_CARD1 = (
    FieldSchema("lch00", int, 0, 10, None),
    FieldSchema("lch45", int, 10, 10, None),
    FieldSchema("lch90", int, 20, 10, None),
    FieldSchema("lchbi", int, 30, 10, None),
    FieldSchema("lchsh", int, 40, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_CARD2 = (
    FieldSchema("lcr00", int, 0, 10, None),
    FieldSchema("lcr45", int, 10, 10, None),
    FieldSchema("lcr90", int, 20, 10, None),
    FieldSchema("lcrbi", int, 30, 10, None),
    FieldSchema("lcrsh", int, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_CARD3 = (
    FieldSchema("aopt", float, 0, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_CARD4 = (
    FieldSchema("unused", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_CARD5 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATEXTENDED3PARAMETERBARLAT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatExtended3ParameterBarlat(KeywordBase):
    """DYNA MAT_EXTENDED_3-PARAMETER_BARLAT keyword"""

    keyword = "MAT"
    subkeyword = "EXTENDED_3-PARAMETER_BARLAT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatExtended3ParameterBarlat class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATEXTENDED3PARAMETERBARLAT_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatExtended3ParameterBarlat.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATEXTENDED3PARAMETERBARLAT_OPTION0_CARD0,
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def lch00(self) -> typing.Optional[int]:
        """Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
        (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
        below. LCH00 must be defined, the other defaults to LCH00 if not defined.
        """ # nopep8
        return self._cards[1].get_value("lch00")

    @lch00.setter
    def lch00(self, value: int) -> None:
        """Set the lch00 property."""
        self._cards[1].set_value("lch00", value)

    @property
    def lch45(self) -> typing.Optional[int]:
        """Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
        (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
        below. LCH00 must be defined, the other defaults to LCH00 if not defined.
        """ # nopep8
        return self._cards[1].get_value("lch45")

    @lch45.setter
    def lch45(self, value: int) -> None:
        """Set the lch45 property."""
        self._cards[1].set_value("lch45", value)

    @property
    def lch90(self) -> typing.Optional[int]:
        """Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
        (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
        below. LCH00 must be defined, the other defaults to LCH00 if not defined.
        """ # nopep8
        return self._cards[1].get_value("lch90")

    @lch90.setter
    def lch90(self, value: int) -> None:
        """Set the lch90 property."""
        self._cards[1].set_value("lch90", value)

    @property
    def lchbi(self) -> typing.Optional[int]:
        """Get or set the Load curve defining biaxial stress vs. biaxial strain, see discussion in the
        Remarks below for a definition. If not defined this is determined from
        LCH00 and the initial R-values to yield a response close to the standard 3-parameter Barlat model.
        """ # nopep8
        return self._cards[1].get_value("lchbi")

    @lchbi.setter
    def lchbi(self, value: int) -> None:
        """Set the lchbi property."""
        self._cards[1].set_value("lchbi", value)

    @property
    def lchsh(self) -> typing.Optional[int]:
        """Get or set the Load curve defining shear stress vs. shear strain, see discussion in the
        Remarks below for a definition. If not defined this is ignored to yield a
        response close to the standard 3-parameter Barlat model.
        """ # nopep8
        return self._cards[1].get_value("lchsh")

    @lchsh.setter
    def lchsh(self, value: int) -> None:
        """Set the lchsh property."""
        self._cards[1].set_value("lchsh", value)

    @property
    def lcr00(self) -> typing.Optional[int]:
        """Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
        direction (XX is either 00, 45, 90). The exact definition is discussed in the
        Remarks below. Default is a constant R-value of 1.0, a negative input will
        result in a constant R-value of –LCRXX.
        """ # nopep8
        return self._cards[2].get_value("lcr00")

    @lcr00.setter
    def lcr00(self, value: int) -> None:
        """Set the lcr00 property."""
        self._cards[2].set_value("lcr00", value)

    @property
    def lcr45(self) -> typing.Optional[int]:
        """Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
        direction (XX is either 00, 45, 90). The exact definition is discussed in the
        Remarks below. Default is a constant R-value of 1.0, a negative input will
        result in a constant R-value of –LCRXX.
        """ # nopep8
        return self._cards[2].get_value("lcr45")

    @lcr45.setter
    def lcr45(self, value: int) -> None:
        """Set the lcr45 property."""
        self._cards[2].set_value("lcr45", value)

    @property
    def lcr90(self) -> typing.Optional[int]:
        """Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
        direction (XX is either 00, 45, 90). The exact definition is discussed in the
        Remarks below. Default is a constant R-value of 1.0, a negative input will
        result in a constant R-value of –LCRXX.
        """ # nopep8
        return self._cards[2].get_value("lcr90")

    @lcr90.setter
    def lcr90(self, value: int) -> None:
        """Set the lcr90 property."""
        self._cards[2].set_value("lcr90", value)

    @property
    def lcrbi(self) -> typing.Optional[int]:
        """Get or set the Load curve defining biaxial R-value vs. biaxial strain, see discussion in the
        Remarks below for a definition. Default is a constant R-value of 1.0, a
        negative input will result in a constant R-value of –LCRBI.
        """ # nopep8
        return self._cards[2].get_value("lcrbi")

    @lcrbi.setter
    def lcrbi(self, value: int) -> None:
        """Set the lcrbi property."""
        self._cards[2].set_value("lcrbi", value)

    @property
    def lcrsh(self) -> typing.Optional[int]:
        """Get or set the Load curve defining shear R-value vs. shear strain, see discussion in the
        Remarks below for a definition. Default is a constant R-value of 1.0, a
        negative input will result in a constant R-value of –LCRSH.
        """ # nopep8
        return self._cards[2].get_value("lcrsh")

    @lcrsh.setter
    def lcrsh(self, value: int) -> None:
        """Set the lcrsh property."""
        self._cards[2].set_value("lcrsh", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Barlat flow exponent, , must be an integer value.
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[2].set_value("m", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[3].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[5].set_value("beta", value)

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

