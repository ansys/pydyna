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

class ControlImplicitBuckle(KeywordBase):
    """DYNA CONTROL_IMPLICIT_BUCKLE keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_BUCKLE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nmode",
                        int,
                        0,
                        10,
                        kwargs.get("nmode", 0)
                    ),
                    Field(
                        "bckmth",
                        int,
                        10,
                        10,
                        kwargs.get("bckmth", 1)
                    ),
                ],
            ),
        ]

    @property
    def nmode(self) -> int:
        """Get or set the Number of buckling modes to compute:
        EQ.0: none (default)
        EQ.n: compute n lowest buckling modes
        LT.0: curve ID = (-NEIG) used for intermittent buckling analysis
        """ # nopep8
        return self._cards[0].get_value("nmode")

    @nmode.setter
    def nmode(self, value: int) -> None:
        self._cards[0].set_value("nmode", value)

    @property
    def bckmth(self) -> int:
        """Get or set the Method used to extract buckling modes
        EQ.1: Use Block Shift and Invert Lanczos. Default of all problems
        not using *CONTROL_IMPLICIT_INERTIA_RELIEF.
        EQ.2: Use Power Method. Only valid option for problems using
        *CONTROL_IMPLICIT_INERTIA_RELIEF. Optional for other problems. See Remarks
        """ # nopep8
        return self._cards[0].get_value("bckmth")

    @bckmth.setter
    def bckmth(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""bckmth must be one of {1,2}""")
        self._cards[0].set_value("bckmth", value)

