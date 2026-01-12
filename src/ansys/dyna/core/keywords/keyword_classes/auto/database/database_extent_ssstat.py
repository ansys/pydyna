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

"""Module providing the DatabaseExtentSsstat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEEXTENTSSSTAT_CARD0 = (
    FieldSchema("psid1", int, 0, 10, None),
    FieldSchema("psid2", int, 10, 10, None),
    FieldSchema("psid3", int, 20, 10, None),
    FieldSchema("psid4", int, 30, 10, None),
    FieldSchema("psid5", int, 40, 10, None),
    FieldSchema("psid6", int, 50, 10, None),
    FieldSchema("psid7", int, 60, 10, None),
    FieldSchema("psid8", int, 70, 10, None),
)

class DatabaseExtentSsstat(KeywordBase):
    """DYNA DATABASE_EXTENT_SSSTAT keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_SSSTAT"

    def __init__(self, **kwargs):
        """Initialize the DatabaseExtentSsstat class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEEXTENTSSSTAT_CARD0,
                **kwargs,
            ),        ]
    @property
    def psid1(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 1, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        """Set the psid1 property."""
        self._cards[0].set_value("psid1", value)

    @property
    def psid2(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 2, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid2")

    @psid2.setter
    def psid2(self, value: int) -> None:
        """Set the psid2 property."""
        self._cards[0].set_value("psid2", value)

    @property
    def psid3(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 3, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid3")

    @psid3.setter
    def psid3(self, value: int) -> None:
        """Set the psid3 property."""
        self._cards[0].set_value("psid3", value)

    @property
    def psid4(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 4, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid4")

    @psid4.setter
    def psid4(self, value: int) -> None:
        """Set the psid4 property."""
        self._cards[0].set_value("psid4", value)

    @property
    def psid5(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 5, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid5")

    @psid5.setter
    def psid5(self, value: int) -> None:
        """Set the psid5 property."""
        self._cards[0].set_value("psid5", value)

    @property
    def psid6(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 6, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid6")

    @psid6.setter
    def psid6(self, value: int) -> None:
        """Set the psid6 property."""
        self._cards[0].set_value("psid6", value)

    @property
    def psid7(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 7, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid7")

    @psid7.setter
    def psid7(self, value: int) -> None:
        """Set the psid7 property."""
        self._cards[0].set_value("psid7", value)

    @property
    def psid8(self) -> typing.Optional[int]:
        """Get or set the Part set ID for subsystem 8, see *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid8")

    @psid8.setter
    def psid8(self, value: int) -> None:
        """Set the psid8 property."""
        self._cards[0].set_value("psid8", value)

