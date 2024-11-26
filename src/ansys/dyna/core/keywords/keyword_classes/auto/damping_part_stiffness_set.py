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

class DampingPartStiffnessSet(KeywordBase):
    """DYNA DAMPING_PART_STIFFNESS_SET keyword"""

    keyword = "DAMPING"
    subkeyword = "PART_STIFFNESS_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "coef",
                        float,
                        10,
                        10,
                        kwargs.get("coef")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID, see *PART SET.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def coef(self) -> typing.Optional[float]:
        """Get or set the Rayleigh damping coefficient.  Two methods are now available:
        LT.0.0:	Rayleigh damping coefficient in units of time, set based on a given frequencyand applied uniformly to each element in the specified part or part set.This method is typically used for implicit dynamic analysis.See remarks below.
        EQ.0.0 : Inactive.
        GT.0.0 : Unitless damping coefficient for stiffness weighted damping.This non - classical method is typically used for explicit analyses as it does not require assembly of a stiffness matrix.Values between 0.01 and 0.25 are recommended.Higher values are strongly discouraged,and values less than 0.01 may have little effect.The damping coefficient is uniquely calculated internally for each element of the part ID.
        """ # nopep8
        return self._cards[0].get_value("coef")

    @coef.setter
    def coef(self, value: float) -> None:
        self._cards[0].set_value("coef", value)

