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

class BoundaryPrescribedOrientationRigidEulerp(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_EULERP keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ORIENTATION_RIGID_EULERP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pidb",
                        int,
                        0,
                        10,
                        kwargs.get("pidb")
                    ),
                    Field(
                        "pida",
                        int,
                        10,
                        10,
                        kwargs.get("pida")
                    ),
                    Field(
                        "intrp",
                        int,
                        20,
                        10,
                        kwargs.get("intrp", 1)
                    ),
                    Field(
                        "birth",
                        float,
                        30,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        40,
                        10,
                        kwargs.get("death", 1.e20)
                    ),
                    Field(
                        "toffset",
                        int,
                        50,
                        10,
                        kwargs.get("toffset", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcide1",
                        int,
                        0,
                        10,
                        kwargs.get("lcide1")
                    ),
                    Field(
                        "lcide2",
                        int,
                        10,
                        10,
                        kwargs.get("lcide2")
                    ),
                    Field(
                        "lcide3",
                        int,
                        20,
                        10,
                        kwargs.get("lcide3")
                    ),
                    Field(
                        "lcide4",
                        int,
                        30,
                        10,
                        kwargs.get("lcide4")
                    ),
                ],
            ),
        ]

    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body B whose orientation is prescribed
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        self._cards[0].set_value("pidb", value)

    @property
    def pida(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
        """ # nopep8
        return self._cards[0].get_value("pida")

    @pida.setter
    def pida(self, value: int) -> None:
        self._cards[0].set_value("pida", value)

    @property
    def intrp(self) -> int:
        """Get or set the Interpolation method used on time history curves:
        EQ.1: Linear interpolation (default)
        """ # nopep8
        return self._cards[0].get_value("intrp")

    @intrp.setter
    def intrp(self, value: int) -> None:
        self._cards[0].set_value("intrp", value)

    @property
    def birth(self) -> float:
        """Get or set the Prior to this time the body moves freely under the action of other agents.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def toffset(self) -> int:
        """Get or set the Time offset flag:
        EQ.0:   No time offset is applied.
        EQ.1:	The time value of all load curves will be offset by the birth time,
        EQ.0:	no time offset is applied
        """ # nopep8
        return self._cards[0].get_value("toffset")

    @toffset.setter
    def toffset(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""toffset must be one of {0,1}""")
        self._cards[0].set_value("toffset", value)

    @property
    def lcide1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcide1")

    @lcide1.setter
    def lcide1(self, value: int) -> None:
        self._cards[1].set_value("lcide1", value)

    @property
    def lcide2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcide2")

    @lcide2.setter
    def lcide2(self, value: int) -> None:
        self._cards[1].set_value("lcide2", value)

    @property
    def lcide3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcide3")

    @lcide3.setter
    def lcide3(self, value: int) -> None:
        self._cards[1].set_value("lcide3", value)

    @property
    def lcide4(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.
        """ # nopep8
        return self._cards[1].get_value("lcide4")

    @lcide4.setter
    def lcide4(self, value: int) -> None:
        self._cards[1].set_value("lcide4", value)

