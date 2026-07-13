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

"""Module providing the EfvStrengthSteinbergGuinan class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTHSTEINBERGGUINAN_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g0", float, 10, 10, None),
    FieldSchema("sigy0", float, 20, 10, None),
    FieldSchema("sigymax", float, 30, 10, None),
    FieldSchema("beta", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("dgdp", float, 60, 10, None),
    FieldSchema("dgot", float, 70, 10, None),
)

_EFVSTRENGTHSTEINBERGGUINAN_CARD1 = (
    FieldSchema("dydp", float, 0, 10, None),
    FieldSchema("tm", float, 10, 10, None),
)

class EfvStrengthSteinbergGuinan(KeywordBase):
    """DYNA EFV_STRENGTH_STEINBERG_GUINAN keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_STEINBERG_GUINAN"

    def __init__(self, **kwargs):
        """Initialize the EfvStrengthSteinbergGuinan class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHSTEINBERGGUINAN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHSTEINBERGGUINAN_CARD1,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g0(self) -> typing.Optional[float]:
        """Get or set the Initial shear modulus, G_0. See Remarks 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("g0")

    @g0.setter
    def g0(self, value: float) -> None:
        """Set the g0 property."""
        self._cards[0].set_value("g0", value)

    @property
    def sigy0(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress, o_(y,0).
        """ # nopep8
        return self._cards[0].get_value("sigy0")

    @sigy0.setter
    def sigy0(self, value: float) -> None:
        """Set the sigy0 property."""
        self._cards[0].set_value("sigy0", value)

    @property
    def sigymax(self) -> typing.Optional[float]:
        """Get or set the Maximum yield stress, o_(y,max ).
        """ # nopep8
        return self._cards[0].get_value("sigymax")

    @sigymax.setter
    def sigymax(self, value: float) -> None:
        """Set the sigymax property."""
        self._cards[0].set_value("sigymax", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening constant, b.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening exponent, n.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def dgdp(self) -> typing.Optional[float]:
        """Get or set the Derivative of the shear modulus with respect to pressure at the reference state, G_p**'. See Remarks 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("dgdp")

    @dgdp.setter
    def dgdp(self, value: float) -> None:
        """Set the dgdp property."""
        self._cards[0].set_value("dgdp", value)

    @property
    def dgot(self) -> typing.Optional[float]:
        """Get or set the Derivative of the shear modulus with respect to temperature at the reference state, G_T**'. See Remarks 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("dgot")

    @dgot.setter
    def dgot(self, value: float) -> None:
        """Set the dgot property."""
        self._cards[0].set_value("dgot", value)

    @property
    def dydp(self) -> typing.Optional[float]:
        """Get or set the Derivative of the yield stress with respect to pressuree at the reference state, [o_y]_p**'. See Remarks 1 and 2.
        """ # nopep8
        return self._cards[1].get_value("dydp")

    @dydp.setter
    def dydp(self, value: float) -> None:
        """Set the dydp property."""
        self._cards[1].set_value("dydp", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melting temperature, T_melt .
        """ # nopep8
        return self._cards[1].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[1].set_value("tm", value)

