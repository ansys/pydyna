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

class RigidwallPlanarFiniteMoving(KeywordBase):
    """DYNA RIGIDWALL_PLANAR_FINITE_MOVING keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "PLANAR_FINITE_MOVING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        kwargs.get("nsidex", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "offset",
                        float,
                        30,
                        10,
                        kwargs.get("offset", 0.0)
                    ),
                    Field(
                        "birth",
                        float,
                        40,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        50,
                        10,
                        kwargs.get("death", 1.0E+20)
                    ),
                    Field(
                        "rwksf",
                        float,
                        60,
                        10,
                        kwargs.get("rwksf", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xt",
                        float,
                        0,
                        10,
                        kwargs.get("xt", 0.0)
                    ),
                    Field(
                        "yt",
                        float,
                        10,
                        10,
                        kwargs.get("yt", 0.0)
                    ),
                    Field(
                        "zt",
                        float,
                        20,
                        10,
                        kwargs.get("zt", 0.0)
                    ),
                    Field(
                        "xh",
                        float,
                        30,
                        10,
                        kwargs.get("xh", 0.0)
                    ),
                    Field(
                        "yh",
                        float,
                        40,
                        10,
                        kwargs.get("yh", 0.0)
                    ),
                    Field(
                        "zh",
                        float,
                        50,
                        10,
                        kwargs.get("zh", 0.0)
                    ),
                    Field(
                        "fric",
                        float,
                        60,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                    Field(
                        "wvel",
                        float,
                        70,
                        10,
                        kwargs.get("wvel", 0.0)
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
                        kwargs.get("lenl", 0.0)
                    ),
                    Field(
                        "lenm",
                        float,
                        40,
                        10,
                        kwargs.get("lenm", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mass",
                        float,
                        0,
                        10,
                        kwargs.get("mass")
                    ),
                    Field(
                        "v0",
                        float,
                        10,
                        10,
                        kwargs.get("v0", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
        EQ.0: All nodes are tracked for interacting with the rigid wall.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in box are included as tracked nodes for the interacting with ther rigid wall, see *DEFINE_BOX. If options NSID or NSIDEX are active then only the subset of nodes activated by these options are checked to see if they are within the box.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def offset(self) -> float:
        """Get or set the All nodes within a normal offset distance, OFFSET, to the rigid wall are included as tracked nodes for the rigid wall. If options NSID, NSIDEX, or BOXID are active then only the subset of nodes activated by these options are checked to see if they are within the offset distance.
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[0].set_value("offset", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def rwksf(self) -> float:
        """Get or set the Stiffness scaling factor. If RWKSF is also specified in *CONTROL_ CONTACT, the stiffness is scaled by the product of the two values.
        """ # nopep8
        return self._cards[0].get_value("rwksf")

    @rwksf.setter
    def rwksf(self, value: float) -> None:
        self._cards[0].set_value("rwksf", value)

    @property
    def xt(self) -> float:
        """Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
        """ # nopep8
        return self._cards[1].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[1].set_value("xt", value)

    @property
    def yt(self) -> float:
        """Get or set the y-coordinate of tail of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[1].set_value("yt", value)

    @property
    def zt(self) -> float:
        """Get or set the z-coordinate of tail of normal vector n
        """ # nopep8
        return self._cards[1].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[1].set_value("zt", value)

    @property
    def xh(self) -> float:
        """Get or set the x-coordinate of head of normal vector n
        """ # nopep8
        return self._cards[1].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        self._cards[1].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the y-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        self._cards[1].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the z-coordinate of head of normal vector n.
        """ # nopep8
        return self._cards[1].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        self._cards[1].set_value("zh", value)

    @property
    def fric(self) -> float:
        """Get or set the Interface friction:
        EQ.0.0: frictionless sliding after contact,
        EQ.1.0: no sliding after contact, 0.0 < FRIC < 1.0: Coulomb friction coefficient,
        EQ.2.0: node is welded after contact with frictionless sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL,
        EQ.3.0: node is welded after contact with no sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL.
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[1].set_value("fric", value)

    @property
    def wvel(self) -> float:
        """Get or set the Critical normal velocity at which nodes weld to wall (FRIC = 2.0 or 3.0).
        """ # nopep8
        return self._cards[1].get_value("wvel")

    @wvel.setter
    def wvel(self, value: float) -> None:
        self._cards[1].set_value("wvel", value)

    @property
    def xhev(self) -> float:
        """Get or set the x-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("xhev")

    @xhev.setter
    def xhev(self, value: float) -> None:
        self._cards[2].set_value("xhev", value)

    @property
    def yhev(self) -> float:
        """Get or set the y-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("yhev")

    @yhev.setter
    def yhev(self, value: float) -> None:
        self._cards[2].set_value("yhev", value)

    @property
    def zhev(self) -> float:
        """Get or set the z-coordinate of head of edge vector l.
        """ # nopep8
        return self._cards[2].get_value("zhev")

    @zhev.setter
    def zhev(self, value: float) -> None:
        self._cards[2].set_value("zhev", value)

    @property
    def lenl(self) -> float:
        """Get or set the Length of l edge.
        EQ.0.0: defines an infinite size plane.
        """ # nopep8
        return self._cards[2].get_value("lenl")

    @lenl.setter
    def lenl(self, value: float) -> None:
        self._cards[2].set_value("lenl", value)

    @property
    def lenm(self) -> float:
        """Get or set the Length of m edge.
        EQ.0.0: defines an infinite size plane.
        """ # nopep8
        return self._cards[2].get_value("lenm")

    @lenm.setter
    def lenm(self, value: float) -> None:
        self._cards[2].set_value("lenm", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Total mass of stonewall.
        """ # nopep8
        return self._cards[3].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[3].set_value("mass", value)

    @property
    def v0(self) -> float:
        """Get or set the Initial velocity of stonewall in direction of defining vector, n.
        """ # nopep8
        return self._cards[3].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[3].set_value("v0", value)

