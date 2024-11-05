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

class CeseMat002(KeywordBase):
    """DYNA CESE_MAT_002 keyword"""

    keyword = "CESE"
    subkeyword = "MAT_002"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "mu",
                        float,
                        10,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "prnd",
                        float,
                        20,
                        10,
                        kwargs.get("prnd", 0.72)
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identifier
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Fluid dynamic viscosity. For Air at 15  C, MU = 1.81E-5 kg/m.s.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def prnd(self) -> float:
        """Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72.
        """ # nopep8
        return self._cards[0].get_value("prnd")

    @prnd.setter
    def prnd(self, value: float) -> None:
        self._cards[0].set_value("prnd", value)

