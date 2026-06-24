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

"""Module providing the DefineFunction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.text_card import TextCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFUNCTION_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_DEFINEFUNCTION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

_DEFINEFUNCTION_OPTION1_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFunction(KeywordBase):
    """DYNA DEFINE_FUNCTION keyword"""

    keyword = "DEFINE"
    subkeyword = "FUNCTION"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFunction class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFUNCTION_CARD0,
                **kwargs,
            ),
            TextCard("function", kwargs.get("function")),
            OptionCardSet(
                option_spec = DefineFunction._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFUNCTION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = DefineFunction._option_spec_list[1],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFUNCTION_OPTION1_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Function ID.  Functions, tables (see *DEFINE_TABLE), and load curves may not share common ID's.  A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the An optional descriptive heading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def function(self) -> typing.Optional[str]:
        """Get or set the function text."""
        return self._cards[1].value

    @function.setter
    def function(self, value: str) -> None:
        self._cards[1].value = value

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

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

