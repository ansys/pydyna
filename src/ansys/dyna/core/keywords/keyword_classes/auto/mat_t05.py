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

class MatT05(KeywordBase):
    """DYNA MAT_T05 keyword"""

    keyword = "MAT"
    subkeyword = "T05"
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
                        "tmid",
                        int,
                        0,
                        10,
                        kwargs.get("tmid")
                    ),
                    Field(
                        "tro",
                        float,
                        10,
                        10,
                        kwargs.get("tro")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hc",
                        float,
                        0,
                        10,
                        kwargs.get("hc")
                    ),
                    Field(
                        "tc",
                        float,
                        10,
                        10,
                        kwargs.get("tc")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatT05.option_specs[0],
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
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:	EQ.0.0: default to structural density.
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        self._cards[0].set_value("tro", value)

    @property
    def hc(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[1].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        self._cards[1].set_value("hc", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Thermal conductance (SI units are W/K).HC = (heat transfer coefficient)x(beam cross section area)
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        self._cards[1].set_value("tc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

