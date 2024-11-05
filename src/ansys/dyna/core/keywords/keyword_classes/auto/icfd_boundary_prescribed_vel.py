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

class IcfdBoundaryPrescribedVel(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_VEL keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_VEL"

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
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "vad",
                        int,
                        20,
                        10,
                        kwargs.get("vad", 1)
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        40,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "vid",
                        int,
                        50,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "death",
                        float,
                        60,
                        10,
                        kwargs.get("death", 1.E+28)
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
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.1: x- degree-of-freedom,
        EQ.2: y- degree-of-freedom,
        EQ.3: z- degree-of-freedom
        EQ.4: Normal direction degree-of-freedom.
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""dof must be one of {1,2,3,4}""")
        self._cards[0].set_value("dof", value)

    @property
    def vad(self) -> int:
        """Get or set the Velocity flag:
        EQ.1 linear velocity
        EQ.2 angular velocity
        . EQ.3: Parabolic velocity profile
        EQ.4: Activates synthetic turbulent field on part (See *ICFD_BOUNDARY_TURB_SYNTHESIS)
        """ # nopep8
        return self._cards[0].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""vad must be one of {1,2,3,4}""")
        self._cards[0].set_value("vad", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe motion value versus time, see *DEFINE_ CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  See BIRTH below.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.  (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Point ID for angular velocity application point, see *ICFD_DEFINE_POINT.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def death(self) -> float:
        """Get or set the Time imposed motion/constraint is removed.
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def birth(self) -> float:
        """Get or set the Time imposed motion/constraint is activated starting from the initial abscissa value of the curve.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

