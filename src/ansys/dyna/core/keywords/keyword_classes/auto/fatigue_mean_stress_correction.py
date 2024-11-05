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

class FatigueMeanStressCorrection(KeywordBase):
    """DYNA FATIGUE_MEAN_STRESS_CORRECTION keyword"""

    keyword = "FATIGUE"
    subkeyword = "MEAN_STRESS_CORRECTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method", 0)
                    ),
                ],
            ),
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
                        "sigma",
                        float,
                        10,
                        10,
                        kwargs.get("sigma")
                    ),
                ],
            ),
        ]

    @property
    def method(self) -> int:
        """Get or set the Mean stress correction method:
        EQ.0: Goodman equation
        EQ.1: Soderberg equation
        EQ.2: Gerber equation
        EQ.3: Goodman tension only
        EQ.4: Gerber tension only
        EQ.11: Morrow equation
        EQ.12: Smith-Watson-Topper equation
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 11, 12]:
            raise Exception("""method must be one of {0,1,2,3,4,11,12}""")
        self._cards[0].set_value("method", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID for which the current mean stress correction method is applied.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[1].set_value("mid", value)

    @property
    def sigma(self) -> typing.Optional[float]:
        """Get or set the Ultimate tensile strength to be used in the Goodman equation
        (METHOD = 0, 3) or the Gerber equation (METHOD = 2, 4), or
        yield strength to be used in the Soderberg equation (METHOD = 1)
        """ # nopep8
        return self._cards[1].get_value("sigma")

    @sigma.setter
    def sigma(self, value: float) -> None:
        self._cards[1].set_value("sigma", value)

