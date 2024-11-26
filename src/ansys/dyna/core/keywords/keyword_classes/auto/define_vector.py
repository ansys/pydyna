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

class DefineVector(KeywordBase):
    """DYNA DEFINE_VECTOR keyword"""

    keyword = "DEFINE"
    subkeyword = "VECTOR"
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
                        "vid",
                        int,
                        0,
                        10,
                        kwargs.get("vid", 0)
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        kwargs.get("xt", 0.0)
                    ),
                    Field(
                        "yt",
                        float,
                        20,
                        10,
                        kwargs.get("yt", 0.0)
                    ),
                    Field(
                        "zt",
                        float,
                        30,
                        10,
                        kwargs.get("zt", 0.0)
                    ),
                    Field(
                        "xh",
                        float,
                        40,
                        10,
                        kwargs.get("xh", 0.0)
                    ),
                    Field(
                        "yh",
                        float,
                        50,
                        10,
                        kwargs.get("yh", 0.0)
                    ),
                    Field(
                        "zh",
                        float,
                        60,
                        10,
                        kwargs.get("zh", 0.0)
                    ),
                    Field(
                        "cid",
                        int,
                        70,
                        10,
                        kwargs.get("cid", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineVector.option_specs[0],
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
    def vid(self) -> int:
        """Get or set the Vector ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of vector.
        """ # nopep8
        return self._cards[0].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[0].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of vector.
        """ # nopep8
        return self._cards[0].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[0].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of vector.
        """ # nopep8
        return self._cards[0].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[0].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of vector.
        """ # nopep8
        return self._cards[0].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        self._cards[0].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of vector.
        """ # nopep8
        return self._cards[0].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        self._cards[0].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of vector.
        """ # nopep8
        return self._cards[0].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        self._cards[0].set_value("zh", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID to define vector in loacal coordinate system. All coordinates, XT,YT,ZT,XH,YH, and ZH are in prespect to cid. 0 gobal
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

