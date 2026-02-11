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

"""Module providing the DatabaseMaxSolidSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DATABASEMAXSOLIDSET_CARD0 = (
    FieldSchema("id1", int, 0, 10, None),
    FieldSchema("id2", int, 10, 10, None),
    FieldSchema("id3", int, 20, 10, None),
    FieldSchema("id4", int, 30, 10, None),
    FieldSchema("id5", int, 40, 10, None),
    FieldSchema("id6", int, 50, 10, None),
    FieldSchema("id7", int, 60, 10, None),
    FieldSchema("id8", int, 70, 10, None),
)

class DatabaseMaxSolidSet(KeywordBase):
    """DYNA DATABASE_MAX_SOLID_SET keyword"""

    keyword = "DATABASE"
    subkeyword = "MAX_SOLID_SET"
    _link_fields = {
        "id1": LinkType.SET_SOLID,
        "id2": LinkType.SET_SOLID,
        "id3": LinkType.SET_SOLID,
        "id4": LinkType.SET_SOLID,
        "id5": LinkType.SET_SOLID,
        "id6": LinkType.SET_SOLID,
        "id7": LinkType.SET_SOLID,
        "id8": LinkType.SET_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseMaxSolidSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEMAXSOLIDSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        """Set the id1 property."""
        self._cards[0].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        """Set the id2 property."""
        self._cards[0].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        """Set the id3 property."""
        self._cards[0].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        """Set the id4 property."""
        self._cards[0].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        """Set the id5 property."""
        self._cards[0].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        """Set the id6 property."""
        self._cards[0].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        """Set the id7 property."""
        self._cards[0].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the Solid set ID.
        """ # nopep8
        return self._cards[0].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        """Set the id8 property."""
        self._cards[0].set_value("id8", value)

    @property
    def id1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id1."""
        return self._get_set_link("SOLID", self.id1)

    @id1_link.setter
    def id1_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id1."""
        self.id1 = value.sid

    @property
    def id2_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id2."""
        return self._get_set_link("SOLID", self.id2)

    @id2_link.setter
    def id2_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id2."""
        self.id2 = value.sid

    @property
    def id3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id3."""
        return self._get_set_link("SOLID", self.id3)

    @id3_link.setter
    def id3_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id3."""
        self.id3 = value.sid

    @property
    def id4_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id4."""
        return self._get_set_link("SOLID", self.id4)

    @id4_link.setter
    def id4_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id4."""
        self.id4 = value.sid

    @property
    def id5_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id5."""
        return self._get_set_link("SOLID", self.id5)

    @id5_link.setter
    def id5_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id5."""
        self.id5 = value.sid

    @property
    def id6_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id6."""
        return self._get_set_link("SOLID", self.id6)

    @id6_link.setter
    def id6_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id6."""
        self.id6 = value.sid

    @property
    def id7_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id7."""
        return self._get_set_link("SOLID", self.id7)

    @id7_link.setter
    def id7_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id7."""
        self.id7 = value.sid

    @property
    def id8_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for id8."""
        return self._get_set_link("SOLID", self.id8)

    @id8_link.setter
    def id8_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for id8."""
        self.id8 = value.sid

