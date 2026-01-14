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

"""Module providing the DualceseEosVanDerWaalsGeneralized class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEEOSVANDERWAALSGENERALIZED_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("ga", float, 30, 10, None),
    FieldSchema("bt", float, 40, 10, None),
)

class DualceseEosVanDerWaalsGeneralized(KeywordBase):
    """DYNA DUALCESE_EOS_VAN_DER_WAALS_GENERALIZED keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_VAN_DER_WAALS_GENERALIZED"

    def __init__(self, **kwargs):
        """Initialize the DualceseEosVanDerWaalsGeneralized class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEEOSVANDERWAALSGENERALIZED_CARD0,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the van der Waals gas constant for molecular cohesive forces
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the van der Waals gas constant for the finite size of molecules
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def ga(self) -> typing.Optional[float]:
        """Get or set the Ratio of specific heats, Must be>1.0.
        """ # nopep8
        return self._cards[0].get_value("ga")

    @ga.setter
    def ga(self, value: float) -> None:
        """Set the ga property."""
        self._cards[0].set_value("ga", value)

    @property
    def bt(self) -> typing.Optional[float]:
        """Get or set the Reference pressure, Must be>+0.0
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[0].set_value("bt", value)

