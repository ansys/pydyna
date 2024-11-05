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

class ControlThermalEigenvalue(KeywordBase):
    """DYNA CONTROL_THERMAL_EIGENVALUE keyword"""

    keyword = "CONTROL"
    subkeyword = "THERMAL_EIGENVALUE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "neig",
                        int,
                        0,
                        10,
                        kwargs.get("neig", 0)
                    ),
                ],
            ),
        ]

    @property
    def neig(self) -> int:
        """Get or set the Number of eigenvalues to compute:
        EQ.0:	No eigenvalues are computed.
        GT.0 : Compute NEIG eigenvalues of each thermal conductance matrix.
        """ # nopep8
        return self._cards[0].get_value("neig")

    @neig.setter
    def neig(self, value: int) -> None:
        self._cards[0].set_value("neig", value)

