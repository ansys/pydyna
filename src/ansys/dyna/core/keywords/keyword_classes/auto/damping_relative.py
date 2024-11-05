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

class DampingRelative(KeywordBase):
    """DYNA DAMPING_RELATIVE keyword"""

    keyword = "DAMPING"
    subkeyword = "RELATIVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cdamp",
                        float,
                        0,
                        10,
                        kwargs.get("cdamp", 0.0)
                    ),
                    Field(
                        "freq",
                        float,
                        10,
                        10,
                        kwargs.get("freq", 0.0)
                    ),
                    Field(
                        "pidrb",
                        int,
                        20,
                        10,
                        kwargs.get("pidrb", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "dv2",
                        float,
                        40,
                        10,
                        kwargs.get("dv2", 0.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                ],
            ),
        ]

    @property
    def cdamp(self) -> float:
        """Get or set the Fraction of critical damping.
        """ # nopep8
        return self._cards[0].get_value("cdamp")

    @cdamp.setter
    def cdamp(self, value: float) -> None:
        self._cards[0].set_value("cdamp", value)

    @property
    def freq(self) -> float:
        """Get or set the Frequency at which CDAMP is to apply (cycles per unit time, e.g., Hz if time unit is seconds).
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: float) -> None:
        self._cards[0].set_value("freq", value)

    @property
    def pidrb(self) -> int:
        """Get or set the Part ID of rigid body, see *PART. Motion relative to this rigid body will be damped.
        """ # nopep8
        return self._cards[0].get_value("pidrb")

    @pidrb.setter
    def pidrb(self, value: int) -> None:
        self._cards[0].set_value("pidrb", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID. The requested damping is applied only to the parts in the set.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def dv2(self) -> float:
        """Get or set the Optional constant for velocity-squared term
        """ # nopep8
        return self._cards[0].get_value("dv2")

    @dv2.setter
    def dv2(self, value: float) -> None:
        self._cards[0].set_value("dv2", value)

    @property
    def lcid(self) -> int:
        """Get or set the ID of curve that defines fraction of critical damping vs. time. CDAMP will be ignored if LCID is non-zero.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

