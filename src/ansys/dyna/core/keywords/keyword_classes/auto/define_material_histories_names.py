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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineMaterialHistoriesNames(KeywordBase):
    """DYNA DEFINE_MATERIAL_HISTORIES_NAMES keyword"""

    keyword = "DEFINE"
    subkeyword = "MATERIAL_HISTORIES_NAMES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "label",
                        str,
                        0,
                        40,
                        kwargs.get("label")
                    ),
                    Field(
                        "a1",
                        float,
                        40,
                        10,
                        kwargs.get("a1", 0.0)
                    ),
                    Field(
                        "a2",
                        float,
                        50,
                        10,
                        kwargs.get("a2", 0.0)
                    ),
                    Field(
                        "a3",
                        float,
                        60,
                        10,
                        kwargs.get("a3", 0.0)
                    ),
                    Field(
                        "a4",
                        float,
                        70,
                        10,
                        kwargs.get("a4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "name",
                        str,
                        0,
                        80,
                        kwargs.get("name")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineMaterialHistoriesNames.option_specs[0],
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
    def label(self) -> typing.Optional[str]:
        """Get or set the String identifying history variable type.
        """ # nopep8
        return self._cards[0].get_value("label")

    @label.setter
    def label(self, value: str) -> None:
        self._cards[0].set_value("label", value)

    @property
    def a1(self) -> float:
        """Get or set the Attributes, for future use. See section LABEL
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> float:
        """Get or set the Attributes, for future use. See section LABEL
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> float:
        """Get or set the Attributes, for future use.See section LABEL
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[0].set_value("a3", value)

    @property
    def a4(self) -> float:
        """Get or set the Attributes, for future use.See section LABEL
        """ # nopep8
        return self._cards[0].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        self._cards[0].set_value("a4", value)

    @property
    def name(self) -> typing.Optional[str]:
        """Get or set the User-defined name for history variable
        """ # nopep8
        return self._cards[1].get_value("name")

    @name.setter
    def name(self, value: str) -> None:
        self._cards[1].set_value("name", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

