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

"""Module providing the EfvEosUser class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSUSER_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("k", float, 10, 10, None),
    FieldSchema("ec2", float, 20, 10, None),
    FieldSchema("ec3", float, 30, 10, None),
    FieldSchema("ec4", float, 40, 10, None),
    FieldSchema("ec5", float, 50, 10, None),
    FieldSchema("ec6", float, 60, 10, None),
    FieldSchema("ec7", float, 70, 10, None),
)

_EFVEOSUSER_CARD1 = (
    FieldSchema("ec8", float, 0, 10, None),
    FieldSchema("ec9", float, 10, 10, None),
    FieldSchema("ec10", float, 20, 10, None),
    FieldSchema("ec11", float, 30, 10, None),
)

class EfvEosUser(KeywordBase):
    """DYNA EFV_EOS_USER keyword"""

    keyword = "EFV"
    subkeyword = "EOS_USER"

    def __init__(self, **kwargs):
        """Initialize the EfvEosUser class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSUSER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSUSER_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def ec2(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec2")

    @ec2.setter
    def ec2(self, value: float) -> None:
        """Set the ec2 property."""
        self._cards[0].set_value("ec2", value)

    @property
    def ec3(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec3")

    @ec3.setter
    def ec3(self, value: float) -> None:
        """Set the ec3 property."""
        self._cards[0].set_value("ec3", value)

    @property
    def ec4(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec4")

    @ec4.setter
    def ec4(self, value: float) -> None:
        """Set the ec4 property."""
        self._cards[0].set_value("ec4", value)

    @property
    def ec5(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec5")

    @ec5.setter
    def ec5(self, value: float) -> None:
        """Set the ec5 property."""
        self._cards[0].set_value("ec5", value)

    @property
    def ec6(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec6")

    @ec6.setter
    def ec6(self, value: float) -> None:
        """Set the ec6 property."""
        self._cards[0].set_value("ec6", value)

    @property
    def ec7(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[0].get_value("ec7")

    @ec7.setter
    def ec7(self, value: float) -> None:
        """Set the ec7 property."""
        self._cards[0].set_value("ec7", value)

    @property
    def ec8(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[1].get_value("ec8")

    @ec8.setter
    def ec8(self, value: float) -> None:
        """Set the ec8 property."""
        self._cards[1].set_value("ec8", value)

    @property
    def ec9(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[1].get_value("ec9")

    @ec9.setter
    def ec9(self, value: float) -> None:
        """Set the ec9 property."""
        self._cards[1].set_value("ec9", value)

    @property
    def ec10(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[1].get_value("ec10")

    @ec10.setter
    def ec10(self, value: float) -> None:
        """Set the ec10 property."""
        self._cards[1].set_value("ec10", value)

    @property
    def ec11(self) -> typing.Optional[float]:
        """Get or set the Equation of state parameter
        """ # nopep8
        return self._cards[1].get_value("ec11")

    @ec11.setter
    def ec11(self, value: float) -> None:
        """Set the ec11 property."""
        self._cards[1].set_value("ec11", value)

