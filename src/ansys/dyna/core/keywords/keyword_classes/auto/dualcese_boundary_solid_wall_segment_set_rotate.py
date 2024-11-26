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

class DualceseBoundarySolidWallSegmentSetRotate(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_SOLID_WALL_SEGMENT_SET_ROTATE keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_SOLID_WALL_SEGMENT_SET_ROTATE"

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
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp", 0.0)
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp", 0.0)
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp", 0.0)
                    ),
                    Field(
                        "nx",
                        float,
                        50,
                        10,
                        kwargs.get("nx", 0.0)
                    ),
                    Field(
                        "ny",
                        float,
                        60,
                        10,
                        kwargs.get("ny", 0.0)
                    ),
                    Field(
                        "nz",
                        float,
                        70,
                        10,
                        kwargs.get("nz", 0.0)
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to define this solid wall boundary movement
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def xp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[0].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[0].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[0].set_value("zp", value)

    @property
    def nx(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> float:
        """Get or set the Coordinates for a point on the axis of rotation
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        self._cards[0].set_value("nz", value)

