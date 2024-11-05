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

class DefineGroundMotion(KeywordBase):
    """DYNA DEFINE_GROUND_MOTION keyword"""

    keyword = "DEFINE"
    subkeyword = "GROUND_MOTION"
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
                        "gmid",
                        int,
                        0,
                        10,
                        kwargs.get("gmid")
                    ),
                    Field(
                        "alcid",
                        int,
                        10,
                        10,
                        kwargs.get("alcid")
                    ),
                    Field(
                        "vlcid",
                        int,
                        20,
                        10,
                        kwargs.get("vlcid")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineGroundMotion.option_specs[0],
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
    def gmid(self) -> typing.Optional[int]:
        """Get or set the Ground motion ID. A unique number has to be defined
        """ # nopep8
        return self._cards[0].get_value("gmid")

    @gmid.setter
    def gmid(self, value: int) -> None:
        self._cards[0].set_value("gmid", value)

    @property
    def alcid(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID of ground acceleration history
        """ # nopep8
        return self._cards[0].get_value("alcid")

    @alcid.setter
    def alcid(self, value: int) -> None:
        self._cards[0].set_value("alcid", value)

    @property
    def vlcid(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID of ground velocity history
        """ # nopep8
        return self._cards[0].get_value("vlcid")

    @vlcid.setter
    def vlcid(self, value: int) -> None:
        self._cards[0].set_value("vlcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

