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

class ControlFormingStrainRatioSmooth(KeywordBase):
    """DYNA CONTROL_FORMING_STRAIN_RATIO_SMOOTH keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_STRAIN_RATIO_SMOOTH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt/cycle",
                        float,
                        0,
                        10,
                        kwargs.get("dt/cycle")
                    ),
                    Field(
                        "weight",
                        float,
                        10,
                        10,
                        kwargs.get("weight")
                    ),
                ],
            ),
        ]

    @property
    def dt_cycle(self) -> typing.Optional[float]:
        """Get or set the Flag for output option (time interval or cycle number).
        LT.0:	the absolute value is the time interval between outputs.
        GT.0:	number of cycles between outputs
        """ # nopep8
        return self._cards[0].get_value("dt/cycle")

    @dt_cycle.setter
    def dt_cycle(self, value: float) -> None:
        self._cards[0].set_value("dt/cycle", value)

    @property
    def weight(self) -> typing.Optional[float]:
        """Get or set the Coefficient a in equation below
        """ # nopep8
        return self._cards[0].get_value("weight")

    @weight.setter
    def weight(self, value: float) -> None:
        self._cards[0].set_value("weight", value)

