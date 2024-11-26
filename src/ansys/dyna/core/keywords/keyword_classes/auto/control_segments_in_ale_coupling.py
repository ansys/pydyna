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

class ControlSegmentsInAleCoupling(KeywordBase):
    """DYNA CONTROL_SEGMENTS_IN_ALE_COUPLING keyword"""

    keyword = "CONTROL"
    subkeyword = "SEGMENTS_IN_ALE_COUPLING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rankey",
                        int,
                        0,
                        10,
                        kwargs.get("rankey", 0)
                    ),
                    Field(
                        "segset",
                        int,
                        10,
                        10,
                        kwargs.get("segset", 0)
                    ),
                    Field(
                        "ncychk",
                        int,
                        20,
                        10,
                        kwargs.get("ncychk", 10)
                    ),
                    Field(
                        "sym",
                        int,
                        30,
                        10,
                        kwargs.get("sym", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ninthk",
                        int,
                        0,
                        10,
                        kwargs.get("ninthk", 0)
                    ),
                    Field(
                        "conthk",
                        float,
                        10,
                        10,
                        kwargs.get("conthk", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def rankey(self) -> int:
        """Get or set the Rank of *CONSTRAINED_LAGRANGE_IN_SOLID in the input deck. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("rankey")

    @rankey.setter
    def rankey(self, value: int) -> None:
        self._cards[0].set_value("rankey", value)

    @property
    def segset(self) -> int:
        """Get or set the Set ID of *SET_SEGMENT (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("segset")

    @segset.setter
    def segset(self, value: int) -> None:
        self._cards[0].set_value("segset", value)

    @property
    def ncychk(self) -> int:
        """Get or set the Number of cycles between checks to activate/deactivate coupling segments (see Remark 3).
        """ # nopep8
        return self._cards[0].get_value("ncychk")

    @ncychk.setter
    def ncychk(self, value: int) -> None:
        self._cards[0].set_value("ncychk", value)

    @property
    def sym(self) -> int:
        """Get or set the Flag to deactivate coupling segments with normal boundary constraints.
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("sym")

    @sym.setter
    def sym(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sym must be one of {0,1}""")
        self._cards[0].set_value("sym", value)

    @property
    def ninthk(self) -> int:
        """Get or set the Minimum number of coupling points in contact to deactivate the segment (see Remark 4).
        """ # nopep8
        return self._cards[1].get_value("ninthk")

    @ninthk.setter
    def ninthk(self, value: int) -> None:
        self._cards[1].set_value("ninthk", value)

    @property
    def conthk(self) -> float:
        """Get or set the Contact thickness (see Remark 5).
        """ # nopep8
        return self._cards[1].get_value("conthk")

    @conthk.setter
    def conthk(self, value: float) -> None:
        self._cards[1].set_value("conthk", value)

