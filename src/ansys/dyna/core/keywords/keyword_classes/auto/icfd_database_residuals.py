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

class IcfdDatabaseResiduals(KeywordBase):
    """DYNA ICFD_DATABASE_RESIDUALS keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_RESIDUALS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rlvl",
                        int,
                        0,
                        10,
                        kwargs.get("rlvl", 0)
                    ),
                ],
            ),
        ]

    @property
    def rlvl(self) -> int:
        """Get or set the Residual output level :
        EQ.0: No output.
        EQ.1: Only outputs the number of iterations needed for solving the pressure Poisson equation.
        EQ.2: Outputs the number of iterations for the momentum, pressure, mesh movement and temperature equations.
        EQ.3: Also gives the residual for each iteration during the solve of the momentum, pressure, mesh movement and temperature equations.
        """ # nopep8
        return self._cards[0].get_value("rlvl")

    @rlvl.setter
    def rlvl(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""rlvl must be one of {0,1,2,3}""")
        self._cards[0].set_value("rlvl", value)

