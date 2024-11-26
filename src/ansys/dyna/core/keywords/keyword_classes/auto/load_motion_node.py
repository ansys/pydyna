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

class LoadMotionNode(KeywordBase):
    """DYNA LOAD_MOTION_NODE keyword"""

    keyword = "LOAD"
    subkeyword = "MOTION_NODE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "node1",
                        int,
                        0,
                        10,
                        kwargs.get("node1")
                    ),
                    Field(
                        "dof1",
                        int,
                        10,
                        10,
                        kwargs.get("dof1", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "cid1",
                        int,
                        40,
                        10,
                        kwargs.get("cid1", 0)
                    ),
                    Field(
                        "node2",
                        int,
                        50,
                        10,
                        kwargs.get("node2", 0)
                    ),
                    Field(
                        "dof2",
                        int,
                        60,
                        10,
                        kwargs.get("dof2", 0)
                    ),
                    Field(
                        "cid2",
                        int,
                        70,
                        10,
                        kwargs.get("cid2", 0)
                    ),
                ],
            ),
        ]

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the Node ID for the concentrated force
        """ # nopep8
        return self._cards[0].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[0].set_value("node1", value)

    @property
    def dof1(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
        EQ.1:  x-direction of load action,
        EQ.2:  y-direction of load action,
        EQ.3:  z-direction of load action,
        EQ.4:  moment about the x-axis,
        EQ.5:  moment about the y-axis,
        EQ.6:  moment about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("dof1")

    @dof1.setter
    def dof1(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6]:
            raise Exception("""dof1 must be one of {0,1,2,3,4,5,6}""")
        self._cards[0].set_value("dof1", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE. The  applied force is a function of the applicable degree-of-freedom of NODE2
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def cid1(self) -> int:
        """Get or set the Coordinate system ID (optional), see remark 1 on next page.
        """ # nopep8
        return self._cards[0].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        self._cards[0].set_value("cid1", value)

    @property
    def node2(self) -> int:
        """Get or set the Node ID for calculating the force.
        """ # nopep8
        return self._cards[0].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[0].set_value("node2", value)

    @property
    def dof2(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ. 1:  x-coordinate
        EQ. 2:  y-coordinate,
        EQ. 3:  z-coordinate,
        EQ. 4:  x-translational displacement,
        EQ. 5:  y-translational displacement,
        EQ. 6:  z-translational displacement,
        EQ. 7:  rotational displacement about the x-axis,
        EQ. 8:  rotational displacement about the y-axis,
        EQ. 9:  rotational displacement about the z-axis.
        EQ.10:  x-translational velocity,
        EQ.11:  y-translational velocity,
        EQ.12:  z-translational velocity,
        EQ.13:  rotational velocity about the x-axis,
        EQ.14:  rotational velocity about the y-axis,
        EQ.15:  rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("dof2")

    @dof2.setter
    def dof2(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]:
            raise Exception("""dof2 must be one of {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}""")
        self._cards[0].set_value("dof2", value)

    @property
    def cid2(self) -> int:
        """Get or set the Coordinate system ID (optional), see remark 1.
        """ # nopep8
        return self._cards[0].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        self._cards[0].set_value("cid2", value)

