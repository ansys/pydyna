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

"""Module providing the LoadRigidBody class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadRigidBody(KeywordBase):
    """DYNA LOAD_RIGID_BODY keyword"""

    keyword = "LOAD"
    subkeyword = "RIGID_BODY"

    def __init__(self, **kwargs):
        """Initialize the LoadRigidBody class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "m1",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "m2",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "m3",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid body.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.1: x-direction of load action,
        EQ.2: y-direction of load action,
        EQ.3: z-direction of load action,
        EQ.4: follower force,
        EQ.5: moment about the x-axis,
        EQ.6: moment about the y-axis,
        EQ.7: moment about the z-axis,
        EQ.8: follower moment.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""dof must be `None` or one of {1,2,3,4,5,6,7,8}.""")
        self._cards[0].set_value("dof", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        GT.0: force as a function of time,
        LT.0: force as a function of the absolute value of the rigid body displacement.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID (optional).
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def m1(self) -> int:
        """Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m1")

    @m1.setter
    def m1(self, value: int) -> None:
        """Set the m1 property."""
        self._cards[0].set_value("m1", value)

    @property
    def m2(self) -> int:
        """Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m2")

    @m2.setter
    def m2(self, value: int) -> None:
        """Set the m2 property."""
        self._cards[0].set_value("m2", value)

    @property
    def m3(self) -> int:
        """Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m3")

    @m3.setter
    def m3(self, value: int) -> None:
        """Set the m3 property."""
        self._cards[0].set_value("m3", value)

