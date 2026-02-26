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

"""Module providing the SetModeList class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETMODELIST_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETMODELIST_CARD1 = (
    FieldSchema("mid1", int, 0, 10, None),
    FieldSchema("mid2", int, 10, 10, None),
    FieldSchema("mid3", int, 20, 10, None),
    FieldSchema("mid4", int, 30, 10, None),
    FieldSchema("mid5", int, 40, 10, None),
    FieldSchema("mid6", int, 50, 10, None),
    FieldSchema("mid7", int, 60, 10, None),
    FieldSchema("mid8", int, 70, 10, None),
)

_SETMODELIST_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetModeList(KeywordBase):
    """DYNA SET_MODE_LIST keyword"""

    keyword = "SET"
    subkeyword = "MODE_LIST"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetModeList class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETMODELIST_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETMODELIST_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetModeList.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETMODELIST_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set identification. All mode sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        """Set the mid1 property."""
        self._cards[1].set_value("mid1", value)

    @property
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        """Set the mid2 property."""
        self._cards[1].set_value("mid2", value)

    @property
    def mid3(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid3")

    @mid3.setter
    def mid3(self, value: int) -> None:
        """Set the mid3 property."""
        self._cards[1].set_value("mid3", value)

    @property
    def mid4(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid4")

    @mid4.setter
    def mid4(self, value: int) -> None:
        """Set the mid4 property."""
        self._cards[1].set_value("mid4", value)

    @property
    def mid5(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid5")

    @mid5.setter
    def mid5(self, value: int) -> None:
        """Set the mid5 property."""
        self._cards[1].set_value("mid5", value)

    @property
    def mid6(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid6")

    @mid6.setter
    def mid6(self, value: int) -> None:
        """Set the mid6 property."""
        self._cards[1].set_value("mid6", value)

    @property
    def mid7(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid7")

    @mid7.setter
    def mid7(self, value: int) -> None:
        """Set the mid7 property."""
        self._cards[1].set_value("mid7", value)

    @property
    def mid8(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[1].get_value("mid8")

    @mid8.setter
    def mid8(self, value: int) -> None:
        """Set the mid8 property."""
        self._cards[1].set_value("mid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

