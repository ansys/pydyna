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

class IgaEdgeUvw(KeywordBase):
    """DYNA IGA_EDGE_UVW keyword"""

    keyword = "IGA"
    subkeyword = "EDGE_UVW"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "exyzid",
                        int,
                        10,
                        10,
                        kwargs.get("exyzid")
                    ),
                    Field(
                        "nid",
                        int,
                        20,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "sense",
                        int,
                        30,
                        10,
                        kwargs.get("sense", 0)
                    ),
                    Field(
                        "rstart",
                        float,
                        40,
                        20,
                        kwargs.get("rstart")
                    ),
                    Field(
                        "rend",
                        float,
                        60,
                        20,
                        kwargs.get("rend")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Parametric edge ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def exyzid(self) -> typing.Optional[int]:
        """Get or set the Physical edge IDs, see *IGA_EDGE_XYZ and Remark 1.
        """ # nopep8
        return self._cards[0].get_value("exyzid")

    @exyzid.setter
    def exyzid(self, value: int) -> None:
        self._cards[0].set_value("exyzid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Parametric univariate NURBS ID, see *IGA_1D_NURBS_UVW, see Remark 2.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def sense(self) -> int:
        """Get or set the Sense of Orientation with respect to the physical edge.
        EQ. 0:Same(default)
        EQ.1 : Reversed.
        """ # nopep8
        return self._cards[0].get_value("sense")

    @sense.setter
    def sense(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sense must be one of {0,1}""")
        self._cards[0].set_value("sense", value)

    @property
    def rstart(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate defining the start of the trimmed parametric NURBS, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("rstart")

    @rstart.setter
    def rstart(self, value: float) -> None:
        self._cards[0].set_value("rstart", value)

    @property
    def rend(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate defining the end of the trimmed parametric NURBS, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("rend")

    @rend.setter
    def rend(self, value: float) -> None:
        self._cards[0].set_value("rend", value)

