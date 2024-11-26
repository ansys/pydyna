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

class FatigueMultiaxial(KeywordBase):
    """DYNA FATIGUE_MULTIAXIAL keyword"""

    keyword = "FATIGUE"
    subkeyword = "MULTIAXIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "maxial",
                        int,
                        0,
                        10,
                        kwargs.get("maxial", 0)
                    ),
                    Field(
                        "nplane",
                        int,
                        10,
                        10,
                        kwargs.get("nplane", 18)
                    ),
                ],
            ),
        ]

    @property
    def maxial(self) -> int:
        """Get or set the Multiaxial fatigue analysis criterion:
        EQ.0: fatigue analysis using equivalent stress or strain index (defined by INDEX in *FATIGUE)
        EQ.1: fatigue analysis on multiple planes
        EQ.2: fatigue analysis on critical plane which is determined	by the highest 1st principal stress or strain
        """ # nopep8
        return self._cards[0].get_value("maxial")

    @maxial.setter
    def maxial(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""maxial must be one of {0,1,2}""")
        self._cards[0].set_value("maxial", value)

    @property
    def nplane(self) -> int:
        """Get or set the Number of planes for fatigue analysis (for MAXIAL = 1 only)
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        self._cards[0].set_value("nplane", value)

