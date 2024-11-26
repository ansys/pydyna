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

class ControlImplicitStabilizationSpr(KeywordBase):
    """DYNA CONTROL_IMPLICIT_STABILIZATION_SPR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_STABILIZATION_SPR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ias",
                        int,
                        0,
                        10,
                        kwargs.get("ias", 2)
                    ),
                    Field(
                        "scale",
                        float,
                        10,
                        10,
                        kwargs.get("scale", 1.0)
                    ),
                    Field(
                        "tstart",
                        float,
                        20,
                        10,
                        kwargs.get("tstart", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        30,
                        10,
                        kwargs.get("tend", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ias(self) -> int:
        """Get or set the Artificial Stabilization flag
        EQ.1: active
        EQ.2: inactive (default)
        """ # nopep8
        return self._cards[0].get_value("ias")

    @ias.setter
    def ias(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""ias must be one of {2,1}""")
        self._cards[0].set_value("ias", value)

    @property
    def scale(self) -> float:
        """Get or set the Scale factor for artificial stabilization. Values greater than 1.0 cause less springback in the first few steps, while values less than 1.0 allow the part to springback more freely over the first few steps.
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        self._cards[0].set_value("scale", value)

    @property
    def tstart(self) -> float:
        """Get or set the Start time. (default: immediately upon entering implicit mode)
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[0].set_value("tstart", value)

    @property
    def tend(self) -> float:
        """Get or set the End time. (default: termination time)
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[0].set_value("tend", value)

