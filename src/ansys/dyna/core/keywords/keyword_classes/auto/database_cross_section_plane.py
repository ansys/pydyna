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

class DatabaseCrossSectionPlane(KeywordBase):
    """DYNA DATABASE_CROSS_SECTION_PLANE keyword"""

    keyword = "DATABASE"
    subkeyword = "CROSS_SECTION_PLANE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "csid",
                        int,
                        0,
                        10,
                        kwargs.get("csid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "xct",
                        float,
                        10,
                        10,
                        kwargs.get("xct", 0.0)
                    ),
                    Field(
                        "yct",
                        float,
                        20,
                        10,
                        kwargs.get("yct", 0.0)
                    ),
                    Field(
                        "zct",
                        float,
                        30,
                        10,
                        kwargs.get("zct", 0.0)
                    ),
                    Field(
                        "xch",
                        float,
                        40,
                        10,
                        kwargs.get("xch", 0.0)
                    ),
                    Field(
                        "ych",
                        float,
                        50,
                        10,
                        kwargs.get("ych", 0.0)
                    ),
                    Field(
                        "zch",
                        float,
                        60,
                        10,
                        kwargs.get("zch", 0.0)
                    ),
                    Field(
                        "radius",
                        float,
                        70,
                        10,
                        kwargs.get("radius", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xhev",
                        float,
                        0,
                        10,
                        kwargs.get("xhev", 0.0)
                    ),
                    Field(
                        "yhev",
                        float,
                        10,
                        10,
                        kwargs.get("yhev", 0.0)
                    ),
                    Field(
                        "zhev",
                        float,
                        20,
                        10,
                        kwargs.get("zhev", 0.0)
                    ),
                    Field(
                        "lenl",
                        float,
                        30,
                        10,
                        kwargs.get("lenl")
                    ),
                    Field(
                        "lenm",
                        float,
                        40,
                        10,
                        kwargs.get("lenm")
                    ),
                    Field(
                        "id",
                        int,
                        50,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "itype",
                        int,
                        60,
                        10,
                        kwargs.get("itype", 0)
                    ),
                ],
            ),
        ]

    @property
    def csid(self) -> typing.Optional[int]:
        """Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
        """ # nopep8
        return self._cards[0].get_value("csid")

    @csid.setter
    def csid(self, value: int) -> None:
        self._cards[0].set_value("csid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID. If zero all parts are included.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[1].set_value("psid", value)

    @property
    def xct(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, N, originating on wall (tail) and terminating in space (head), (see Figure 9.1 in user's manual).
        """ # nopep8
        return self._cards[1].get_value("xct")

    @xct.setter
    def xct(self, value: float) -> None:
        self._cards[1].set_value("xct", value)

    @property
    def yct(self) -> float:
        """Get or set the y-coordinate of tail of normal vector, N.
        """ # nopep8
        return self._cards[1].get_value("yct")

    @yct.setter
    def yct(self, value: float) -> None:
        self._cards[1].set_value("yct", value)

    @property
    def zct(self) -> float:
        """Get or set the z-coordinate of tail of normal vector, N.
        """ # nopep8
        return self._cards[1].get_value("zct")

    @zct.setter
    def zct(self, value: float) -> None:
        self._cards[1].set_value("zct", value)

    @property
    def xch(self) -> float:
        """Get or set the x-coordinate of head of normal vector, N.
        """ # nopep8
        return self._cards[1].get_value("xch")

    @xch.setter
    def xch(self, value: float) -> None:
        self._cards[1].set_value("xch", value)

    @property
    def ych(self) -> float:
        """Get or set the y-coordinate of head of normal vector, N.
        """ # nopep8
        return self._cards[1].get_value("ych")

    @ych.setter
    def ych(self, value: float) -> None:
        self._cards[1].set_value("ych", value)

    @property
    def zch(self) -> float:
        """Get or set the z-coordinate of head of normal vector, N.
        """ # nopep8
        return self._cards[1].get_value("zch")

    @zch.setter
    def zch(self, value: float) -> None:
        self._cards[1].set_value("zch", value)

    @property
    def radius(self) -> float:
        """Get or set the Optional radius.
        EQ.0.0:	Not used.
        GT.0.0 : A circular cut plane will be created that is centered at(XCT ,YCT ,ZCT) with radius = RADIUS and has a normal vector originating at(XCT ,YCT ,ZCT) and pointing towards(XCH ,YCH ,ZCH).
        LT.0.0 : The radius will be the absolute value of RADIUS and XCT and XCH will be nodes IDs.The node with ID XCT is the center of the circular cut plane.The normal vector of the plane is the vector pointing from the node with ID XCT to the node with ID XCH.YCT, ZCT, YCH,and ZCH are ignored.
        If RADIUS != 0.0, the variables XHEV, YHEV, ZHEV, LENL,and LENM, which are specified on Card 1a.2, will be ignored.
        """ # nopep8
        return self._cards[1].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[1].set_value("radius", value)

    @property
    def xhev(self) -> float:
        """Get or set the x-coordinate of head of edge vector, L.
        """ # nopep8
        return self._cards[2].get_value("xhev")

    @xhev.setter
    def xhev(self, value: float) -> None:
        self._cards[2].set_value("xhev", value)

    @property
    def yhev(self) -> float:
        """Get or set the y-coordinate of head of edge vector, L.
        """ # nopep8
        return self._cards[2].get_value("yhev")

    @yhev.setter
    def yhev(self, value: float) -> None:
        self._cards[2].set_value("yhev", value)

    @property
    def zhev(self) -> float:
        """Get or set the z-coordinate of head of edge vector, L.
        """ # nopep8
        return self._cards[2].get_value("zhev")

    @zhev.setter
    def zhev(self, value: float) -> None:
        self._cards[2].set_value("zhev", value)

    @property
    def lenl(self) -> typing.Optional[float]:
        """Get or set the Length of edge a, in L direction (default is set to infinity).
        """ # nopep8
        return self._cards[2].get_value("lenl")

    @lenl.setter
    def lenl(self, value: float) -> None:
        self._cards[2].set_value("lenl", value)

    @property
    def lenm(self) -> typing.Optional[float]:
        """Get or set the Length of edge b, in M direction (default is set to infinity).
        """ # nopep8
        return self._cards[2].get_value("lenm")

    @lenm.setter
    def lenm(self, value: float) -> None:
        self._cards[2].set_value("lenm", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Rigid body or accelerometer ID. The force resultants are output in the updated local system of the rigid body or accelerometer.
        """ # nopep8
        return self._cards[2].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[2].set_value("id", value)

    @property
    def itype(self) -> int:
        """Get or set the Flag that specifies whether ID above pertains to a rigid body, an accelerometer, or a coordinate system:
        EQ. 0: rigid body (default),
        EQ. 1: accelerometer,
        EQ. 2: coordinate ID.
        """ # nopep8
        return self._cards[2].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""itype must be one of {0,1,2}""")
        self._cards[2].set_value("itype", value)

