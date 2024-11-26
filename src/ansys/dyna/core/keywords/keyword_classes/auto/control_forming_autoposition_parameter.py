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

class ControlFormingAutopositionParameter(KeywordBase):
    """DYNA CONTROL_FORMING_AUTOPOSITION_PARAMETER keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_AUTOPOSITION_PARAMETER"

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
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "dir",
                        int,
                        20,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "mpid",
                        int,
                        30,
                        10,
                        kwargs.get("mpid")
                    ),
                    Field(
                        "position",
                        int,
                        40,
                        10,
                        kwargs.get("position", 1)
                    ),
                    Field(
                        "premove",
                        float,
                        50,
                        10,
                        kwargs.get("premove")
                    ),
                    Field(
                        "thick",
                        float,
                        60,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "porder",
                        str,
                        70,
                        10,
                        kwargs.get("porder")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. This part will be moved based on the following controlling parameters.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID set with *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR. The default is the global coordinate system.
        LT.0: | CID | is vector ID giving the direction the part will be moved
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction that the part will be moved
        .EQ.1:  x direction
        .EQ.2:  y direction
        .EQ.3:  z direction
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""dir must be one of {1,2,3}""")
        self._cards[0].set_value("dir", value)

    @property
    def mpid(self) -> typing.Optional[int]:
        """Get or set the Part ID. The part (with PID) will be moved based on part defined by MPID.
        """ # nopep8
        return self._cards[0].get_value("mpid")

    @mpid.setter
    def mpid(self, value: int) -> None:
        self._cards[0].set_value("mpid", value)

    @property
    def position(self) -> int:
        """Get or set the .EQ1: means that PID is above MPID	.EQ-1: means that PID is below MPID
        """ # nopep8
        return self._cards[0].get_value("position")

    @position.setter
    def position(self, value: int) -> None:
        if value not in [1, -1]:
            raise Exception("""position must be one of {1,-1}""")
        self._cards[0].set_value("position", value)

    @property
    def premove(self) -> typing.Optional[float]:
        """Get or set the PID is moved with a value of PREMOVE. If this parameter is defined, it is unnecessary to define MPID
        """ # nopep8
        return self._cards[0].get_value("premove")

    @premove.setter
    def premove(self, value: float) -> None:
        self._cards[0].set_value("premove", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Part thickness
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def porder(self) -> typing.Optional[str]:
        """Get or set the The name of the parameters in the parameter list.
        """ # nopep8
        return self._cards[0].get_value("porder")

    @porder.setter
    def porder(self, value: str) -> None:
        self._cards[0].set_value("porder", value)

