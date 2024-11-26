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

class ControlImplicitRotationalDynamics(KeywordBase):
    """DYNA CONTROL_IMPLICIT_ROTATIONAL_DYNAMICS keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_ROTATIONAL_DYNAMICS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "omega",
                        float,
                        20,
                        10,
                        kwargs.get("omega")
                    ),
                    Field(
                        "vid",
                        int,
                        30,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "nomeg",
                        int,
                        40,
                        10,
                        kwargs.get("nomeg", 0)
                    ),
                    Field(
                        "iref",
                        int,
                        50,
                        10,
                        kwargs.get("iref", 0)
                    ),
                    Field(
                        "omegadr",
                        float,
                        60,
                        10,
                        kwargs.get("omegadr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "omeg1",
                        float,
                        0,
                        10,
                        kwargs.get("omeg1")
                    ),
                    Field(
                        "omeg2",
                        float,
                        10,
                        10,
                        kwargs.get("omeg2")
                    ),
                    Field(
                        "omeg3",
                        float,
                        20,
                        10,
                        kwargs.get("omeg3")
                    ),
                    Field(
                        "omeg4",
                        float,
                        30,
                        10,
                        kwargs.get("omeg4")
                    ),
                    Field(
                        "omeg5",
                        float,
                        40,
                        10,
                        kwargs.get("omeg5")
                    ),
                    Field(
                        "omeg6",
                        float,
                        50,
                        10,
                        kwargs.get("omeg6")
                    ),
                    Field(
                        "omeg7",
                        float,
                        60,
                        10,
                        kwargs.get("omeg7")
                    ),
                    Field(
                        "omeg8",
                        float,
                        70,
                        10,
                        kwargs.get("omeg8")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID of the rotational components.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.0:	Part;
        EQ.1:	Part set.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""stype must be one of {0,1}""")
        self._cards[0].set_value("stype", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Rotating speed.
        LT.0:	curve ID = (-OMEGA) gives rotating speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[0].set_value("omega", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID to define the rotating axis. It is defined in *DEFINE_VECTOR, and the tail of the vector should be set as the rotating center.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def nomeg(self) -> int:
        """Get or set the Number of rotating speeds. This feature is intended to automatically preform parameter studies with respect to the rotation speed. The keyword *CONTROL_IMPLICIT_EIGENVALUE must be included if NOMEG>0.
        """ # nopep8
        return self._cards[0].get_value("nomeg")

    @nomeg.setter
    def nomeg(self, value: int) -> None:
        self._cards[0].set_value("nomeg", value)

    @property
    def iref(self) -> int:
        """Get or set the Reference frame:
        EQ.0:	Rotating coordinate system;
        EQ.1:	Fixed coordinate system.
        EQ.2:	Rotating coordinate system, but rotate rotating parts for visualization purpose.
        """ # nopep8
        return self._cards[0].get_value("iref")

    @iref.setter
    def iref(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""iref must be one of {0,1,2}""")
        self._cards[0].set_value("iref", value)

    @property
    def omegadr(self) -> typing.Optional[float]:
        """Get or set the Rotating speed defined in dynamic relaxation.
        GT.0: rotating speed defined in dynamic relaxation.
        LT.0:curve ID = (-OMEGA) gives rotating speed as a function of time.
        """ # nopep8
        return self._cards[0].get_value("omegadr")

    @omegadr.setter
    def omegadr(self, value: float) -> None:
        self._cards[0].set_value("omegadr", value)

    @property
    def omeg1(self) -> typing.Optional[float]:
        """Get or set the The 1th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg1")

    @omeg1.setter
    def omeg1(self, value: float) -> None:
        self._cards[1].set_value("omeg1", value)

    @property
    def omeg2(self) -> typing.Optional[float]:
        """Get or set the The 2th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg2")

    @omeg2.setter
    def omeg2(self, value: float) -> None:
        self._cards[1].set_value("omeg2", value)

    @property
    def omeg3(self) -> typing.Optional[float]:
        """Get or set the The 3th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg3")

    @omeg3.setter
    def omeg3(self, value: float) -> None:
        self._cards[1].set_value("omeg3", value)

    @property
    def omeg4(self) -> typing.Optional[float]:
        """Get or set the The 4th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg4")

    @omeg4.setter
    def omeg4(self, value: float) -> None:
        self._cards[1].set_value("omeg4", value)

    @property
    def omeg5(self) -> typing.Optional[float]:
        """Get or set the The 5th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg5")

    @omeg5.setter
    def omeg5(self, value: float) -> None:
        self._cards[1].set_value("omeg5", value)

    @property
    def omeg6(self) -> typing.Optional[float]:
        """Get or set the The 6th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg6")

    @omeg6.setter
    def omeg6(self, value: float) -> None:
        self._cards[1].set_value("omeg6", value)

    @property
    def omeg7(self) -> typing.Optional[float]:
        """Get or set the The 7th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg7")

    @omeg7.setter
    def omeg7(self, value: float) -> None:
        self._cards[1].set_value("omeg7", value)

    @property
    def omeg8(self) -> typing.Optional[float]:
        """Get or set the The 8th rotating speed.
        """ # nopep8
        return self._cards[1].get_value("omeg8")

    @omeg8.setter
    def omeg8(self, value: float) -> None:
        self._cards[1].set_value("omeg8", value)

