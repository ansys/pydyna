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

class ConstrainedRigidBodyStoppers(KeywordBase):
    """DYNA CONSTRAINED_RIGID_BODY_STOPPERS keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "RIGID_BODY_STOPPERS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "lcmax",
                        int,
                        10,
                        10,
                        kwargs.get("lcmax", 0)
                    ),
                    Field(
                        "lcmin",
                        int,
                        20,
                        10,
                        kwargs.get("lcmin", 0)
                    ),
                    Field(
                        "psidmx",
                        int,
                        30,
                        10,
                        kwargs.get("psidmx", 0)
                    ),
                    Field(
                        "psidmn",
                        int,
                        40,
                        10,
                        kwargs.get("psidmn", 0)
                    ),
                    Field(
                        "lcvmnx",
                        int,
                        50,
                        10,
                        kwargs.get("lcvmnx", 0)
                    ),
                    Field(
                        "dir",
                        int,
                        60,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "vid",
                        int,
                        70,
                        10,
                        kwargs.get("vid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tb",
                        float,
                        0,
                        10,
                        kwargs.get("tb", 0.0)
                    ),
                    Field(
                        "td",
                        float,
                        10,
                        10,
                        kwargs.get("td", 1.0E+21)
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "stiff",
                        float,
                        30,
                        10,
                        kwargs.get("stiff", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of lead rigid body, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcmax(self) -> int:
        """Get or set the Load curve ID defining the maximum coordinate or displacement as a function of time. See *DEFINE_CURVE:
        LT.0: Load Curve ID |LCMAX| provides an upper bound for the displacement of the rigid body,
        EQ.0: no limitation of the maximum displacement (default),
        GT.0: Load Curve ID LCMAX provides an upper bound for the position of the rigid body center of mass.
        """ # nopep8
        return self._cards[0].get_value("lcmax")

    @lcmax.setter
    def lcmax(self, value: int) -> None:
        self._cards[0].set_value("lcmax", value)

    @property
    def lcmin(self) -> int:
        """Get or set the Load curve ID defining the minimum coordinate or displacement as a function of time. See *DEFINE_CURVE:
        LT.0: Load Curve ID |LCMIN| defines a lower bound for the displacement of the rigid body,
        EQ.0: no limitation of the minimum displacement (default),
        GT.0: Load Curve ID LCMIN defines a lower bound for the position of the rigid body center of mass.
        """ # nopep8
        return self._cards[0].get_value("lcmin")

    @lcmin.setter
    def lcmin(self, value: int) -> None:
        self._cards[0].set_value("lcmin", value)

    @property
    def psidmx(self) -> int:
        """Get or set the Optional part set ID of rigid bodies that are constrained in the maximum coordinate direction to the lead rigid body.  The part set definition (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is greater than or equal to the closure distance, the constrained rigid body motion away from the lead rigid body also stops.  However, the constrained rigid body is free to move towards the lead rigid body.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
        """ # nopep8
        return self._cards[0].get_value("psidmx")

    @psidmx.setter
    def psidmx(self, value: int) -> None:
        self._cards[0].set_value("psidmx", value)

    @property
    def psidmn(self) -> int:
        """Get or set the Optional part set ID of rigid bodies that are constrained in the minimum coordinate direction to the lead rigid body.  The part set definition, (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2 in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is less than or equal to the closure distance, the constrained rigid body motion towards the lead rigid body also stops.  However, the constrained rigid body is free to move away from the lead rigid part.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
        """ # nopep8
        return self._cards[0].get_value("psidmn")

    @psidmn.setter
    def psidmn(self, value: int) -> None:
        self._cards[0].set_value("psidmn", value)

    @property
    def lcvmnx(self) -> int:
        """Get or set the Load curve ID which defines the maximum absolute value of the velocity as a function of time that is allowed for the lead rigid body.  See *DEFINE_‌CURVE:
        EQ.0:	no limitation on the velocity.
        """ # nopep8
        return self._cards[0].get_value("lcvmnx")

    @lcvmnx.setter
    def lcvmnx(self, value: int) -> None:
        self._cards[0].set_value("lcvmnx", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction stopper acts in (reqiured):
        EQ.1: x-translation,
        EQ.2: y-translation,
        EQ.3: z-translation,
        EQ.4: arbitrary, defined by vector VID (see VID),
        EQ.5: x-axis rotation ,
        EQ.6: y-axis rotation,
        EQ.7: z-axis rotation,
        EQ.8: arbitrary, defined by vector VID (see VID).
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""dir must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("dir", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector for arbitrary orientation of stopper, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def tb(self) -> float:
        """Get or set the Time at which stopper is activated (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("tb")

    @tb.setter
    def tb(self, value: float) -> None:
        self._cards[1].set_value("tb", value)

    @property
    def td(self) -> float:
        """Get or set the Time at which stopper is deactivated (default = 10^21).
        """ # nopep8
        return self._cards[1].get_value("td")

    @td.setter
    def td(self, value: float) -> None:
        self._cards[1].set_value("td", value)

    @property
    def stiff(self) -> float:
        """Get or set the Augmentation stiffness for implicit
        """ # nopep8
        return self._cards[1].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        self._cards[1].set_value("stiff", value)

