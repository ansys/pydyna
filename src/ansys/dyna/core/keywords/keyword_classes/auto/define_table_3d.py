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

"""Module providing the DefineTable3D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineTable3D(KeywordBase):
    """DYNA DEFINE_TABLE_3D keyword"""

    keyword = "DEFINE"
    subkeyword = "TABLE_3D"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTable3D class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "tbid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sfa",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "offa",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "value",
                        float,
                        0,
                        20,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "tbid",
                        int,
                        20,
                        20,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineTable3D.option_specs[0],
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
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably.
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[0].set_value("tbid", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for value.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[0].set_value("sfa", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for values.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        """Set the offa property."""
        self._cards[0].set_value("offa", value)

    @property
    def value(self) -> float:
        """Get or set the Load curve will be defined corresponding to this value, e.g., this value could be a strain rate, see purpose above.
        """ # nopep8
        return self._cards[1].get_value("value")

    @value.setter
    def value(self, value: float) -> None:
        """Set the value property."""
        self._cards[1].set_value("value", value)

    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID.
        """ # nopep8
        return self._cards[1].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[1].set_value("tbid", value)

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

