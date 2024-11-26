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

class FatigueFailure(KeywordBase):
    """DYNA FATIGUE_FAILURE keyword"""

    keyword = "FATIGUE"
    subkeyword = "FAILURE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ifailure",
                        int,
                        0,
                        10,
                        kwargs.get("ifailure", 0)
                    ),
                    Field(
                        "dratio",
                        float,
                        10,
                        10,
                        kwargs.get("dratio", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def ifailure(self) -> int:
        """Get or set the Treatment of elements failed due to fatigue:
        EQ.0:	keep the elements in the model.
        EQ.1:	delete the elements from the model
        """ # nopep8
        return self._cards[0].get_value("ifailure")

    @ifailure.setter
    def ifailure(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifailure must be one of {0,1}""")
        self._cards[0].set_value("ifailure", value)

    @property
    def dratio(self) -> float:
        """Get or set the Threshold value of cumulative damage ratio for an element to be considered failed
        """ # nopep8
        return self._cards[0].get_value("dratio")

    @dratio.setter
    def dratio(self, value: float) -> None:
        self._cards[0].set_value("dratio", value)

