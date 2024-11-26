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

class ControlMppRebalance(KeywordBase):
    """DYNA CONTROL_MPP_REBALANCE keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_REBALANCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ncycle",
                        int,
                        0,
                        10,
                        kwargs.get("ncycle")
                    ),
                    Field(
                        "icoor",
                        int,
                        10,
                        10,
                        kwargs.get("icoor", 0)
                    ),
                    Field(
                        "icost",
                        int,
                        20,
                        10,
                        kwargs.get("icost", 0)
                    ),
                    Field(
                        "thres",
                        float,
                        30,
                        10,
                        kwargs.get("thres", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def ncycle(self) -> typing.Optional[int]:
        """Get or set the Number of cycles between rebalance steps
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: int) -> None:
        self._cards[0].set_value("ncycle", value)

    @property
    def icoor(self) -> int:
        """Get or set the Coordinates used in rebalance:
        EQ.0:	Current coordinates
        NE.0 : Coordinates at t = 0
        """ # nopep8
        return self._cards[0].get_value("icoor")

    @icoor.setter
    def icoor(self, value: int) -> None:
        self._cards[0].set_value("icoor", value)

    @property
    def icost(self) -> int:
        """Get or set the Element costs used in rebalance:
        Q.0:	Time costs
        EQ.1 : Original
        """ # nopep8
        return self._cards[0].get_value("icost")

    @icost.setter
    def icost(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icost must be one of {0,1}""")
        self._cards[0].set_value("icost", value)

    @property
    def thres(self) -> float:
        """Get or set the Percent threshold for rebalancing when performing in-core adaptivity (see Remark 1). For in-core adaptivity, only include this field
        """ # nopep8
        return self._cards[0].get_value("thres")

    @thres.setter
    def thres(self, value: float) -> None:
        self._cards[0].set_value("thres", value)

