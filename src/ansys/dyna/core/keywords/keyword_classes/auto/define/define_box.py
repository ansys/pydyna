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

"""Module providing the DefineBox class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEBOX_CARD0 = (
    FieldSchema("boxid", int, 0, 10, 0),
    FieldSchema("xmn", float, 10, 10, 0.0),
    FieldSchema("xmx", float, 20, 10, 0.0),
    FieldSchema("ymn", float, 30, 10, 0.0),
    FieldSchema("ymx", float, 40, 10, 0.0),
    FieldSchema("zmn", float, 50, 10, 0.0),
    FieldSchema("zmx", float, 60, 10, 0.0),
)

class DefineBox(KeywordBase):
    """DYNA DEFINE_BOX keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineBox class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEBOX_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineBox.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def boxid(self) -> int:
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        """Set the xmn property."""
        self._cards[0].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        """Set the xmx property."""
        self._cards[0].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        """Set the ymn property."""
        self._cards[0].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        """Set the ymx property."""
        self._cards[0].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        """Set the zmn property."""
        self._cards[0].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        """Set the zmx property."""
        self._cards[0].set_value("zmx", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

