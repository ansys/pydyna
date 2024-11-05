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

class BoundarySphFlow(KeywordBase):
    """DYNA BOUNDARY_SPH_FLOW keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_FLOW"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "styp",
                        int,
                        10,
                        10,
                        kwargs.get("styp", 1)
                    ),
                    Field(
                        "dof",
                        int,
                        20,
                        10,
                        kwargs.get("dof", 0)
                    ),
                    Field(
                        "vad",
                        int,
                        30,
                        10,
                        kwargs.get("vad", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        50,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "death",
                        float,
                        60,
                        10,
                        kwargs.get("death", 1.0E+20)
                    ),
                    Field(
                        "birth",
                        float,
                        70,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "vid",
                        int,
                        10,
                        10,
                        kwargs.get("vid")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID (NSID), SEE *SET_NODE, or part ID (PID), see *PART.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def styp(self) -> int:
        """Get or set the Set type:
        EQ.1: part set ID, see *SET_PART (default),
        EQ.2: part ID, see *PART,
        EQ.3: node set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""styp must be one of {1,2,3}""")
        self._cards[0].set_value("styp", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: Not valid, please use any of the other available options,
        EQ. 1: x-translational degree-of-freedom,
        EQ. 2: y-translational degree-of-freedom,
        EQ. 3: z-translational degree-of-freedom,
        EQ. 4: translational motion in direction given by the VID. Movement on plane normal to the vector is permitted.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""dof must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("dof", value)

    @property
    def vad(self) -> int:
        """Get or set the Velocity/Acceleration/Displacement flag applied to SPH elements before activation:
        EQ. 0: velocity,
        EQ. 1: acceleration,
        EQ. 2: displacement.
        """ # nopep8
        return self._cards[0].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""vad must be one of {0,1,2}""")
        self._cards[0].set_value("vad", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe motion value versus time, see *DEFINECURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor. (default=1.0)
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def death(self) -> float:
        """Get or set the Time imposed motion/constraint is removed.
        EQ. 0.0: default set to 1020
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def birth(self) -> float:
        """Get or set the Time imposed motion/constraint is activated.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node fixed in space which determines the boundary between activated particles and deactivated particles
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for defining the orientation of the SPH flow. see *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[1].set_value("vid", value)

