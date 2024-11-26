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

class MatSteelEc3(KeywordBase):
    """DYNA MAT_STEEL_EC3 keyword"""

    keyword = "MAT"
    subkeyword = "STEEL_EC3"
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
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "sgy",
                        float,
                        40,
                        10,
                        kwargs.get("sgy")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc_e",
                        int,
                        0,
                        10,
                        kwargs.get("lc_e")
                    ),
                    Field(
                        "lc_pr",
                        int,
                        10,
                        10,
                        kwargs.get("lc_pr")
                    ),
                    Field(
                        "lc_al",
                        int,
                        20,
                        10,
                        kwargs.get("lc_al")
                    ),
                    Field(
                        "tbl_ss",
                        int,
                        30,
                        10,
                        kwargs.get("tbl_ss")
                    ),
                    Field(
                        "lc_fs",
                        int,
                        40,
                        10,
                        kwargs.get("lc_fs")
                    ),
                ],
            ),
            Card(
                [
                ],
            ),
            OptionCardSet(
                option_spec = MatSteelEc3.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus - a reasonable value must be provided even if LC_E is also input.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sgy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sgy")

    @sgy.setter
    def sgy(self, value: float) -> None:
        self._cards[0].set_value("sgy", value)

    @property
    def lc_e(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: Young's Modulus vs Temperature (overrides E	and factors from EC3).
        """ # nopep8
        return self._cards[1].get_value("lc_e")

    @lc_e.setter
    def lc_e(self, value: int) -> None:
        self._cards[1].set_value("lc_e", value)

    @property
    def lc_pr(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: Poisson's Ratio vs Temperature (overrides PR).
        """ # nopep8
        return self._cards[1].get_value("lc_pr")

    @lc_pr.setter
    def lc_pr(self, value: int) -> None:
        self._cards[1].set_value("lc_pr", value)

    @property
    def lc_al(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: alpha vs temperature (over-rides thermal expansion data from EC3).
        """ # nopep8
        return self._cards[1].get_value("lc_al")

    @lc_al.setter
    def lc_al(self, value: int) -> None:
        self._cards[1].set_value("lc_al", value)

    @property
    def tbl_ss(self) -> typing.Optional[int]:
        """Get or set the Optional Table ID containing stress-strain curves at different temperatures (overrides curves from EC3).
        """ # nopep8
        return self._cards[1].get_value("tbl_ss")

    @tbl_ss.setter
    def tbl_ss(self, value: int) -> None:
        self._cards[1].set_value("tbl_ss", value)

    @property
    def lc_fs(self) -> typing.Optional[int]:
        """Get or set the Optional Load curve ID: failure strain vs temperature.
        """ # nopep8
        return self._cards[1].get_value("lc_fs")

    @lc_fs.setter
    def lc_fs(self, value: int) -> None:
        self._cards[1].set_value("lc_fs", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

