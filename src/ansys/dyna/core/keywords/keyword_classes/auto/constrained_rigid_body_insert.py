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

class ConstrainedRigidBodyInsert(KeywordBase):
    """DYNA CONSTRAINED_RIGID_BODY_INSERT keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "RIGID_BODY_INSERT"

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
                        "pidl",
                        int,
                        10,
                        10,
                        kwargs.get("pidl")
                    ),
                    Field(
                        "pidc",
                        int,
                        20,
                        10,
                        kwargs.get("pidc")
                    ),
                    Field(
                        "coordid",
                        int,
                        30,
                        10,
                        kwargs.get("coordid")
                    ),
                    Field(
                        "idir",
                        int,
                        40,
                        10,
                        kwargs.get("idir", 3)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mflag",
                        int,
                        0,
                        10,
                        kwargs.get("mflag", 0)
                    ),
                    Field(
                        "mcid",
                        int,
                        10,
                        10,
                        kwargs.get("mcid")
                    ),
                    Field(
                        "deathm",
                        float,
                        20,
                        10,
                        kwargs.get("deathm", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "partb",
                        int,
                        0,
                        10,
                        kwargs.get("partb")
                    ),
                    Field(
                        "deathb",
                        float,
                        10,
                        10,
                        kwargs.get("deathb", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Insert ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def pidl(self) -> typing.Optional[int]:
        """Get or set the Lead (die) rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidl")

    @pidl.setter
    def pidl(self, value: int) -> None:
        self._cards[0].set_value("pidl", value)

    @property
    def pidc(self) -> typing.Optional[int]:
        """Get or set the Constraned (die insert) rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidc")

    @pidc.setter
    def pidc(self, value: int) -> None:
        self._cards[0].set_value("pidc", value)

    @property
    def coordid(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID. The x direction is the direction the insert moves independently of the die.
        """ # nopep8
        return self._cards[0].get_value("coordid")

    @coordid.setter
    def coordid(self, value: int) -> None:
        self._cards[0].set_value("coordid", value)

    @property
    def idir(self) -> int:
        """Get or set the Direction in which the insert moves independently of the die:
        EQ.1:	Local x - direction
        EQ.2 : Local y - direction
        EQ.3 : Local z - direction(default)
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        if value not in [3, 0, 1, 2]:
            raise Exception("""idir must be one of {3,0,1,2}""")
        self._cards[0].set_value("idir", value)

    @property
    def mflag(self) -> int:
        """Get or set the Motion flag.
        EQ.0:	Relative motion is unconstrained.
        EQ.1:	The displacement of the insert relative to the die is imposed.
        EQ.2:	The velocity of the insert relative to the die is imposed.
        EQ.3:	The acceleration of the insert relative to the die is imposed..
        """ # nopep8
        return self._cards[1].get_value("mflag")

    @mflag.setter
    def mflag(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""mflag must be one of {0,1,2,3}""")
        self._cards[1].set_value("mflag", value)

    @property
    def mcid(self) -> typing.Optional[int]:
        """Get or set the Curve defining the motion of the die insert relative to the die.
        """ # nopep8
        return self._cards[1].get_value("mcid")

    @mcid.setter
    def mcid(self, value: int) -> None:
        self._cards[1].set_value("mcid", value)

    @property
    def deathm(self) -> float:
        """Get or set the Death time of the imposed motion. If it is equal to 0.0, the motion is imposed for the entire analysis.
        """ # nopep8
        return self._cards[1].get_value("deathm")

    @deathm.setter
    def deathm(self, value: float) -> None:
        self._cards[1].set_value("deathm", value)

    @property
    def partb(self) -> typing.Optional[int]:
        """Get or set the Part ID for a discrete beam connected between the insert and die.
        """ # nopep8
        return self._cards[2].get_value("partb")

    @partb.setter
    def partb(self, value: int) -> None:
        self._cards[2].set_value("partb", value)

    @property
    def deathb(self) -> float:
        """Get or set the Death time for the discrete beam specified by BPART.
        """ # nopep8
        return self._cards[2].get_value("deathb")

    @deathb.setter
    def deathb(self, value: float) -> None:
        self._cards[2].set_value("deathb", value)

