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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadThermalVariable(KeywordBase):
    """DYNA LOAD_THERMAL_VARIABLE keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_VARIABLE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        kwargs.get("nsidex", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ts",
                        float,
                        0,
                        10,
                        kwargs.get("ts", 0.0)
                    ),
                    Field(
                        "tb",
                        float,
                        10,
                        10,
                        kwargs.get("tb", 0.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "tse",
                        float,
                        30,
                        10,
                        kwargs.get("tse", 0.0)
                    ),
                    Field(
                        "tbe",
                        float,
                        40,
                        10,
                        kwargs.get("tbe", 0.0)
                    ),
                    Field(
                        "lcide",
                        int,
                        50,
                        10,
                        kwargs.get("lcide")
                    ),
                    Field(
                        "lcidr",
                        int,
                        60,
                        10,
                        kwargs.get("lcidr")
                    ),
                    Field(
                        "lcidedr",
                        int,
                        70,
                        10,
                        kwargs.get("lcidedr")
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing nodes, see *SET_NODE:
        EQ.0: all nodes are included.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that are exempted (optional), see *SET_ NODE.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in box which belong to NSID are initialized. Others are excluded.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def ts(self) -> float:
        """Get or set the Scaled temperature.
        """ # nopep8
        return self._cards[1].get_value("ts")

    @ts.setter
    def ts(self, value: float) -> None:
        self._cards[1].set_value("ts", value)

    @property
    def tb(self) -> float:
        """Get or set the Base temperature.
        """ # nopep8
        return self._cards[1].get_value("tb")

    @tb.setter
    def tb(self, value: float) -> None:
        self._cards[1].set_value("tb", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature, see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def tse(self) -> float:
        """Get or set the Scaled temperature of the exempted nodes (optional).
        """ # nopep8
        return self._cards[1].get_value("tse")

    @tse.setter
    def tse(self, value: float) -> None:
        self._cards[1].set_value("tse", value)

    @property
    def tbe(self) -> float:
        """Get or set the Base temperature of the exempted nodes (optional).
        """ # nopep8
        return self._cards[1].get_value("tbe")

    @tbe.setter
    def tbe(self, value: float) -> None:
        self._cards[1].set_value("tbe", value)

    @property
    def lcide(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional), see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcide")

    @lcide.setter
    def lcide(self, value: int) -> None:
        self._cards[1].set_value("lcide", value)

    @property
    def lcidr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature for dynamic relaxation phase
        """ # nopep8
        return self._cards[1].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        self._cards[1].set_value("lcidr", value)

    @property
    def lcidedr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional) for dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("lcidedr")

    @lcidedr.setter
    def lcidedr(self, value: int) -> None:
        self._cards[1].set_value("lcidedr", value)

