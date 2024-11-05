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

class Mat147N(KeywordBase):
    """DYNA MAT_147_N keyword"""

    keyword = "MAT"
    subkeyword = "147_N"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "fctim",
                        float,
                        10,
                        10,
                        kwargs.get("fctim")
                    ),
                    Field(
                        "fctmas",
                        float,
                        20,
                        10,
                        kwargs.get("fctmas")
                    ),
                    Field(
                        "fctlen",
                        float,
                        30,
                        10,
                        kwargs.get("fctlen")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat147N.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def fctim(self) -> typing.Optional[float]:
        """Get or set the Factor to multiply milliseconds by to get desired time units.
        """ # nopep8
        return self._cards[0].get_value("fctim")

    @fctim.setter
    def fctim(self, value: float) -> None:
        self._cards[0].set_value("fctim", value)

    @property
    def fctmas(self) -> typing.Optional[float]:
        """Get or set the Factor to multiply kilograms by to get desired mass units.
        """ # nopep8
        return self._cards[0].get_value("fctmas")

    @fctmas.setter
    def fctmas(self, value: float) -> None:
        self._cards[0].set_value("fctmas", value)

    @property
    def fctlen(self) -> typing.Optional[float]:
        """Get or set the Factor to muliply millimeters by to get desired length units.
        """ # nopep8
        return self._cards[0].get_value("fctlen")

    @fctlen.setter
    def fctlen(self, value: float) -> None:
        self._cards[0].set_value("fctlen", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

