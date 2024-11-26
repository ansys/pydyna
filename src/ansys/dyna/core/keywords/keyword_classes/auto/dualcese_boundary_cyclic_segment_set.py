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

class DualceseBoundaryCyclicSegmentSet(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_CYCLIC_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_CYCLIC_SEGMENT_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid1",
                        int,
                        0,
                        10,
                        kwargs.get("ssid1")
                    ),
                    Field(
                        "ssid2",
                        int,
                        0,
                        10,
                        kwargs.get("ssid2")
                    ),
                    Field(
                        "cyctyp",
                        int,
                        20,
                        10,
                        kwargs.get("cyctyp", 0)
                    ),
                ],
            ),
        ]

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
        """ # nopep8
        return self._cards[0].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        self._cards[0].set_value("ssid1", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
        """ # nopep8
        return self._cards[0].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        self._cards[0].set_value("ssid2", value)

    @property
    def cyctyp(self) -> int:
        """Get or set the Relationship between the two cyclic boundary condition surfaces:
        EQ.0:	none assumed(default)
        EQ.1 : The first surface is rotated about an axis to match the second surface.
        EQ.2 : The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface
        """ # nopep8
        return self._cards[0].get_value("cyctyp")

    @cyctyp.setter
    def cyctyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""cyctyp must be one of {0,1,2}""")
        self._cards[0].set_value("cyctyp", value)

