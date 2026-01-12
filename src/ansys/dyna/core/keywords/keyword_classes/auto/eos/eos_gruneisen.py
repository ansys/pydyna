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

"""Module providing the EosGruneisen class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSGRUNEISEN_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("s1", float, 20, 10, None),
    FieldSchema("s2", float, 30, 10, None),
    FieldSchema("s3", float, 40, 10, None),
    FieldSchema("gamao", float, 50, 10, None),
    FieldSchema("a", float, 60, 10, None),
    FieldSchema("e0", float, 70, 10, None),
)

_EOSGRUNEISEN_CARD1 = (
    FieldSchema("v0", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
)

class EosGruneisen(KeywordBase):
    """DYNA EOS_GRUNEISEN keyword"""

    keyword = "EOS"
    subkeyword = "GRUNEISEN"

    def __init__(self, **kwargs):
        """Initialize the EosGruneisen class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSGRUNEISEN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSGRUNEISEN_CARD1,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[0].set_value("s1", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        """Set the s2 property."""
        self._cards[0].set_value("s2", value)

    @property
    def s3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("s3")

    @s3.setter
    def s3(self, value: float) -> None:
        """Set the s3 property."""
        self._cards[0].set_value("s3", value)

    @property
    def gamao(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("gamao")

    @gamao.setter
    def gamao(self, value: float) -> None:
        """Set the gamao property."""
        self._cards[0].set_value("gamao", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the E0 Initial internal energy.
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[0].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[1].set_value("v0", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, which can be the ID of *DEFINE_‌CURVE,
        *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION, defining the energy deposition rate.
        If an energy leak rate is intended, do not specify a negative ordinate in LCID, rather,
        use the constant(s) in the equation of state, e.g., set GAMMA0 or/and A to a negative value.
        If *DEFINE_‌FUNCTION is used, the input of the defined function is time.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

