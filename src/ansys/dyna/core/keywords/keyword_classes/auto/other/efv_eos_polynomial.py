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

"""Module providing the EfvEosPolynomial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSPOLYNOMIAL_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a1", float, 10, 10, None),
    FieldSchema("a2", float, 20, 10, None),
    FieldSchema("a3", float, 30, 10, None),
    FieldSchema("b0", float, 40, 10, None),
    FieldSchema("b1", float, 50, 10, None),
    FieldSchema("t1", float, 60, 10, None),
    FieldSchema("t2", float, 70, 10, None),
)

_EFVEOSPOLYNOMIAL_CARD1 = (
    FieldSchema("tref", float, 0, 10, 293.0),
    FieldSchema("cp", float, 10, 10, None),
    FieldSchema("tc", float, 20, 10, None),
)

class EfvEosPolynomial(KeywordBase):
    """DYNA EFV_EOS_POLYNOMIAL keyword"""

    keyword = "EFV"
    subkeyword = "EOS_POLYNOMIAL"

    def __init__(self, **kwargs):
        """Initialize the EfvEosPolynomial class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOLYNOMIAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOLYNOMIAL_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used .(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the A_1, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the A_2, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the A_3, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[0].set_value("a3", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the B_0, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        """Set the b0 property."""
        self._cards[0].set_value("b0", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the B_1, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[0].set_value("b1", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the T_1, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[0].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the T_2, see manual Remarks
        """ # nopep8
        return self._cards[0].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[0].set_value("t2", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[1].set_value("tref", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity
        """ # nopep8
        return self._cards[1].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[1].set_value("cp", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[1].set_value("tc", value)

