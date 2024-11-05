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

class IcfdDatabaseHtc(KeywordBase):
    """DYNA ICFD_DATABASE_HTC keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_HTC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "out",
                        int,
                        0,
                        10,
                        kwargs.get("out", 0)
                    ),
                    Field(
                        "htc",
                        int,
                        10,
                        10,
                        kwargs.get("htc", 0)
                    ),
                    Field(
                        "tb",
                        float,
                        20,
                        10,
                        kwargs.get("tb", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "outdt",
                        float,
                        70,
                        10,
                        kwargs.get("outdt", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def out(self) -> int:
        """Get or set the Determines if the solver should calculate the heat transfer coefficient and how to output it:
        EQ.0:No HTC calculation
        EQ.1:HTC calculated and output in LSPP as a surface variable.
        EQ.2:The solver will also look for FSI boundaries and output the HTC value at the solid nodes in an ASCII file called icfdhtci.dat.
        EQ.3:The solver will also look for FSI boundaries that are part of SEGMENT_SETS and output the HTC for those segments in an ASCII file called icfd_convseg.****.key in a format that can directly read by LS-DYNA for a subsequent pure structural thermal analysis.
        """ # nopep8
        return self._cards[0].get_value("out")

    @out.setter
    def out(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""out must be one of {0,1,2,3}""")
        self._cards[0].set_value("out", value)

    @property
    def htc(self) -> int:
        """Get or set the Determines how the bulk temperature is defined.
        EQ.0: Automatically calculated by the solver based on the average temperature flowing through the pipe section
        EQ.1: User imposed value
        """ # nopep8
        return self._cards[0].get_value("htc")

    @htc.setter
    def htc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""htc must be one of {0,1}""")
        self._cards[0].set_value("htc", value)

    @property
    def tb(self) -> float:
        """Get or set the Value of the bulk temperature if HTC = 1.
        """ # nopep8
        return self._cards[0].get_value("tb")

    @tb.setter
    def tb(self, value: float) -> None:
        self._cards[0].set_value("tb", value)

    @property
    def outdt(self) -> float:
        """Get or set the Output frequency of the HTC in the various ASCII files. If left to 0.,the solver will output the HTC at every timestep.
        """ # nopep8
        return self._cards[0].get_value("outdt")

    @outdt.setter
    def outdt(self, value: float) -> None:
        self._cards[0].set_value("outdt", value)

