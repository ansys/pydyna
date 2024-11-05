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

class ChangeRigidBodyStopper(KeywordBase):
    """DYNA CHANGE_RIGID_BODY_STOPPER keyword"""

    keyword = "CHANGE"
    subkeyword = "RIGID_BODY_STOPPER"

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
                        "birth",
                        float,
                        0,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        10,
                        10,
                        kwargs.get("death", 1.0E+28)
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
        """Get or set the Load curve ID defining the maximum coordinate as a function of time:
        EQ.0: no limitation of the maximum displacement. New curves can be defined by the *DEFINE_CURVE within the present restart deck (default).
        """ # nopep8
        return self._cards[0].get_value("lcmax")

    @lcmax.setter
    def lcmax(self, value: int) -> None:
        self._cards[0].set_value("lcmax", value)

    @property
    def lcmin(self) -> int:
        """Get or set the Load curve ID defining the minimum coordinate as a function of time:
        EQ.0: no limitation of the minimum displacement. New curves can be defined by the *DEFINE_CURVE within the present restart deck (default).
        """ # nopep8
        return self._cards[0].get_value("lcmin")

    @lcmin.setter
    def lcmin(self, value: int) -> None:
        self._cards[0].set_value("lcmin", value)

    @property
    def psidmx(self) -> int:
        """Get or set the Optional part set ID of rigid bodies that are constraned in the maximum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
        """ # nopep8
        return self._cards[0].get_value("psidmx")

    @psidmx.setter
    def psidmx(self, value: int) -> None:
        self._cards[0].set_value("psidmx", value)

    @property
    def psidmn(self) -> int:
        """Get or set the Optional part set ID of rigid bodies that are constraned in the minimum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
        """ # nopep8
        return self._cards[0].get_value("psidmn")

    @psidmn.setter
    def psidmn(self, value: int) -> None:
        self._cards[0].set_value("psidmn", value)

    @property
    def lcvmnx(self) -> int:
        """Get or set the Load curve ID which defines the maximum absolute value of the velocity that is allowed within the stopper:
        EQ.0: no limitation of the maximum velocity(default).
        """ # nopep8
        return self._cards[0].get_value("lcvmnx")

    @lcvmnx.setter
    def lcvmnx(self, value: int) -> None:
        self._cards[0].set_value("lcvmnx", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction stopper acts in:
        EQ.1: x-translation,
        EQ.2: y-translation,
        EQ.3: z-translation,
        EQ.4: arbitrary, defined by vector VID,
        EQ.5: x-axis rotation,
        EQ.6: y-axis rotation,
        EQ.7: z-axis rotation,
        EQ.8: arbitrary, defined by vector VID.
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""dir must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("dir", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector for arbitrary orientation of stopper. The vector must be defined by a *DEFINE_VECTOR within the present restart deck.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which stopper is activated (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which stopper is deactivated (default = 1.0E+28).
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

