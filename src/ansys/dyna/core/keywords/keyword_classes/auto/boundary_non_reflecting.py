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

class BoundaryNonReflecting(KeywordBase):
    """DYNA BOUNDARY_NON_REFLECTING keyword"""

    keyword = "BOUNDARY"
    subkeyword = "NON_REFLECTING"

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
                        "ad",
                        float,
                        10,
                        10,
                        kwargs.get("ad", 0.0)
                    ),
                    Field(
                        "as",
                        float,
                        20,
                        10,
                        kwargs.get("as", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def ad(self) -> float:
        """Get or set the Default activation flag for dilatational waves.
        EQ.0.0: on (default),
        NE.0.0: off.
        """ # nopep8
        return self._cards[0].get_value("ad")

    @ad.setter
    def ad(self, value: float) -> None:
        self._cards[0].set_value("ad", value)

    @property
    def as_(self) -> float:
        """Get or set the Default activation flag for shear waves.
        EQ.0.0: on (default),
        NE.0.0: off.
        """ # nopep8
        return self._cards[0].get_value("as")

    @as_.setter
    def as_(self, value: float) -> None:
        self._cards[0].set_value("as", value)

