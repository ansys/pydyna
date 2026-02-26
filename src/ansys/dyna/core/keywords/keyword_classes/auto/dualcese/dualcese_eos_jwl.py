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

"""Module providing the DualceseEosJwl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEEOSJWL_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("eps1", float, 30, 10, None),
    FieldSchema("eps2", float, 40, 10, None),
    FieldSchema("gammao", float, 50, 10, None),
    FieldSchema("rhoo", float, 60, 10, None),
    FieldSchema("eo", float, 70, 10, None),
)

_DUALCESEEOSJWL_CARD1 = (
    FieldSchema("cv", float, 0, 10, None),
)

class DualceseEosJwl(KeywordBase):
    """DYNA DUALCESE_EOS_JWL keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_JWL"

    def __init__(self, **kwargs):
        """Initialize the DualceseEosJwl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSJWL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSJWL_CARD1,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID for the dual CESE solver
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Model parameter (in pressure units), A
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Model parameter (in pressure units), B
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Model constant (dimensionless), 1
        """ # nopep8
        return self._cards[0].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[0].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Model constant (dimensionless), 2
        """ # nopep8
        return self._cards[0].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[0].set_value("eps2", value)

    @property
    def gammao(self) -> typing.Optional[float]:
        """Get or set the Gruneisen coefficient
        """ # nopep8
        return self._cards[0].get_value("gammao")

    @gammao.setter
    def gammao(self, value: float) -> None:
        """Set the gammao property."""
        self._cards[0].set_value("gammao", value)

    @property
    def rhoo(self) -> typing.Optional[float]:
        """Get or set the Initial or reference density
        """ # nopep8
        return self._cards[0].get_value("rhoo")

    @rhoo.setter
    def rhoo(self, value: float) -> None:
        """Set the rhoo property."""
        self._cards[0].set_value("rhoo", value)

    @property
    def eo(self) -> typing.Optional[float]:
        """Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added
        """ # nopep8
        return self._cards[0].get_value("eo")

    @eo.setter
    def eo(self, value: float) -> None:
        """Set the eo property."""
        self._cards[0].set_value("eo", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Heat capacity, C_v
        """ # nopep8
        return self._cards[1].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[1].set_value("cv", value)

