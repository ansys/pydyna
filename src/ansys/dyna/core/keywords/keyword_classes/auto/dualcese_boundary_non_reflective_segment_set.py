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

class DualceseBoundaryNonReflectiveSegmentSet(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_NON_REFLECTIVE_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_NON_REFLECTIVE_SEGMENT_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "dirx",
                        float,
                        10,
                        10,
                        kwargs.get("dirx")
                    ),
                    Field(
                        "diry",
                        float,
                        20,
                        10,
                        kwargs.get("diry")
                    ),
                    Field(
                        "dirz",
                        float,
                        30,
                        10,
                        kwargs.get("dirz")
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        self._cards[0].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        self._cards[0].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        self._cards[0].set_value("dirz", value)

