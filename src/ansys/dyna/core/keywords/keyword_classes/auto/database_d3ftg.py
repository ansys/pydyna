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

class DatabaseD3Ftg(KeywordBase):
    """DYNA DATABASE_D3FTG keyword"""

    keyword = "DATABASE"
    subkeyword = "D3FTG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "binary",
                        int,
                        0,
                        10,
                        kwargs.get("binary", 0)
                    ),
                    Field(
                        "dt",
                        float,
                        10,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def binary(self) -> int:
        """Get or set the Flag for writing the binary plot file
        EQ.0: off
        EQ.1:write the binary plot file d3ftg
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""binary must be one of {0,1}""")
        self._cards[0].set_value("binary", value)

    @property
    def dt(self) -> float:
        """Get or set the Time interval between output states in time domain fatigue analysis (see *FATIGUE_OPTION)
        EQ.0.0:	only fatigue results at the end of the analysis are output
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

