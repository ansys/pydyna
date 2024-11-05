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

class LoadNodeSetOnce(KeywordBase):
    """DYNA LOAD_NODE_SET_ONCE keyword"""

    keyword = "LOAD"
    subkeyword = "NODE_SET_ONCE"

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
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 0)
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
                        "cid",
                        int,
                        40,
                        10,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "m1",
                        int,
                        50,
                        10,
                        kwargs.get("m1", 0)
                    ),
                    Field(
                        "m2",
                        int,
                        60,
                        10,
                        kwargs.get("m2", 0)
                    ),
                    Field(
                        "m3",
                        int,
                        70,
                        10,
                        kwargs.get("m3", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "once",
                        int,
                        0,
                        10,
                        kwargs.get("once", 0)
                    ),
                    Field(
                        "lcid1",
                        int,
                        10,
                        10,
                        kwargs.get("lcid1")
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see also *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
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
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""dof must be one of {0,1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("dof", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID(see *DEFINE_CURVE) or function ID(see *DEFINE_FUNCTION).
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
    def cid(self) -> int:
        """Get or set the Coordinate system ID (optional).
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def m1(self) -> int:
        """Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m1")

    @m1.setter
    def m1(self, value: int) -> None:
        self._cards[0].set_value("m1", value)

    @property
    def m2(self) -> int:
        """Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m2")

    @m2.setter
    def m2(self, value: int) -> None:
        self._cards[0].set_value("m2", value)

    @property
    def m3(self) -> int:
        """Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.
        """ # nopep8
        return self._cards[0].get_value("m3")

    @m3.setter
    def m3(self, value: int) -> None:
        self._cards[0].set_value("m3", value)

    @property
    def once(self) -> int:
        """Get or set the EQ.0: Do nothing (Default)
        EQ.1: When LCID refers to a * DEFINE_FUNCTION, the function is only evaluated onceand the value will be storedand applied to the rest of run..
        """ # nopep8
        return self._cards[1].get_value("once")

    @once.setter
    def once(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""once must be one of {0,1}""")
        self._cards[1].set_value("once", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the If ONCE equals to 1, LCID1 is scale factor vs time  curve which uses to scale the stored value.
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        self._cards[1].set_value("lcid1", value)

