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

"""Module providing the MatWtmStmPlc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATWTMSTMPLC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("numfi", float, 40, 10, None),
    FieldSchema("npsc", float, 50, 10, None),
    FieldSchema("wc", float, 60, 10, None),
    FieldSchema("tauc", float, 70, 10, None),
)

_MATWTMSTMPLC_CARD1 = (
    FieldSchema("sigma0", float, 0, 10, None),
    FieldSchema("qr1", float, 10, 10, None),
    FieldSchema("cr1", float, 20, 10, None),
    FieldSchema("qr2", float, 30, 10, None),
    FieldSchema("cr2", float, 40, 10, None),
    FieldSchema("k", float, 50, 10, None),
)

_MATWTMSTMPLC_CARD2 = (
    FieldSchema("a1", float, 0, 10, None),
    FieldSchema("a2", float, 10, 10, None),
    FieldSchema("a3", float, 20, 10, None),
    FieldSchema("a4", float, 30, 10, None),
    FieldSchema("a5", float, 40, 10, None),
    FieldSchema("a6", float, 50, 10, None),
    FieldSchema("a7", float, 60, 10, None),
    FieldSchema("a8", float, 70, 10, None),
)

_MATWTMSTMPLC_CARD3 = (
    FieldSchema("s", float, 0, 10, None),
    FieldSchema("h", float, 10, 10, None),
    FieldSchema("omega", float, 20, 10, None),
    FieldSchema("td", float, 30, 10, None),
    FieldSchema("alpha", float, 40, 10, None),
    FieldSchema("eps0", float, 50, 10, None),
)

_MATWTMSTMPLC_CARD4 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("beta", float, 10, 10, None),
)

_MATWTMSTMPLC_CARD5 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATWTMSTMPLC_CARD6 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
)

_MATWTMSTMPLC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatWtmStmPlc(KeywordBase):
    """DYNA MAT_WTM_STM_PLC keyword"""

    keyword = "MAT"
    subkeyword = "WTM_STM_PLC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatWtmStmPlc class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATWTMSTMPLC_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatWtmStmPlc.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATWTMSTMPLC_OPTION0_CARD0,
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def numfi(self) -> typing.Optional[float]:
        """Get or set the Number of through thickness integration points that must fail before the element is deleted (remember to change this number if switching between full and reduced integration type of elements).
        """ # nopep8
        return self._cards[0].get_value("numfi")

    @numfi.setter
    def numfi(self, value: float) -> None:
        """Set the numfi property."""
        self._cards[0].set_value("numfi", value)

    @property
    def npsc(self) -> typing.Optional[float]:
        """Get or set the Critical value   of the plastic thickness strain (used in the CTS fracture criterion).
        """ # nopep8
        return self._cards[0].get_value("npsc")

    @npsc.setter
    def npsc(self, value: float) -> None:
        """Set the npsc property."""
        self._cards[0].set_value("npsc", value)

    @property
    def wc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Cockcroft-Latham fracture criterion
        """ # nopep8
        return self._cards[0].get_value("wc")

    @wc.setter
    def wc(self, value: float) -> None:
        """Set the wc property."""
        self._cards[0].set_value("wc", value)

    @property
    def tauc(self) -> typing.Optional[float]:
        """Get or set the Critical value   for the Bressan-Williams shear fracture criterion
        """ # nopep8
        return self._cards[0].get_value("tauc")

    @tauc.setter
    def tauc(self, value: float) -> None:
        """Set the tauc property."""
        self._cards[0].set_value("tauc", value)

    @property
    def sigma0(self) -> typing.Optional[float]:
        """Get or set the Initial mean value of yield stress  .
        """ # nopep8
        return self._cards[1].get_value("sigma0")

    @sigma0.setter
    def sigma0(self, value: float) -> None:
        """Set the sigma0 property."""
        self._cards[1].set_value("sigma0", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter .
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        """Set the qr1 property."""
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        """Set the cr1 property."""
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        """Set the qr2 property."""
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        """Set the cr2 property."""
        self._cards[1].set_value("cr2", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the equals half YLD2003 exponent  .  Recommended value for FCC materials is  , i.e.  .
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter  .
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter .
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        """Set the a4 property."""
        self._cards[2].set_value("a4", value)

    @property
    def a5(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        """Set the a5 property."""
        self._cards[2].set_value("a5", value)

    @property
    def a6(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a6")

    @a6.setter
    def a6(self, value: float) -> None:
        """Set the a6 property."""
        self._cards[2].set_value("a6", value)

    @property
    def a7(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a7")

    @a7.setter
    def a7(self, value: float) -> None:
        """Set the a7 property."""
        self._cards[2].set_value("a7", value)

    @property
    def a8(self) -> typing.Optional[float]:
        """Get or set the Yld2003 parameter
        """ # nopep8
        return self._cards[2].get_value("a8")

    @a8.setter
    def a8(self, value: float) -> None:
        """Set the a8 property."""
        self._cards[2].set_value("a8", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter, S.
        """ # nopep8
        return self._cards[3].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        """Set the s property."""
        self._cards[3].set_value("s", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter, H.
        """ # nopep8
        return self._cards[3].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[3].set_value("h", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[3].set_value("omega", value)

    @property
    def td(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("td")

    @td.setter
    def td(self, value: float) -> None:
        """Set the td property."""
        self._cards[3].set_value("td", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[3].set_value("alpha", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Dynamic strain aging parameter,  .
        """ # nopep8
        return self._cards[3].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        """Set the eps0 property."""
        self._cards[3].set_value("eps0", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0:  Locally orthotropic with material axes determined by element nodes as shown in Figure M2-1, and then rotated
        about the shell element normal by the angle BETA.	Nodes 1, 2 and 4 of an element are identical to the nodes
        used for the definition of a coordinate system as by *DEFINE_COORDINATE_NODES..
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[4].set_value("aopt", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO..
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1..
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[5].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[6].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

