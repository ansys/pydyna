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

class IcfdDatabasePointout(KeywordBase):
    """DYNA ICFD_DATABASE_POINTOUT keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_POINTOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "dtout",
                        float,
                        10,
                        10,
                        kwargs.get("dtout", 0.0)
                    ),
                    Field(
                        "pstype",
                        int,
                        20,
                        10,
                        kwargs.get("pstype", 0)
                    ),
                    Field(
                        "vx",
                        float,
                        30,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        40,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        50,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                ],
            ),
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
                        "x",
                        float,
                        10,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        30,
                        10,
                        kwargs.get("z")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> int:
        """Get or set the Point Set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def pstype(self) -> int:
        """Get or set the Point Set type:
        EQ.0: Fixed points.
        EQ.1: Tracer points using prescribed velocity.
        EQ.2: Tracer points using fluid velocity.
        EQ.3: Tracer points using mesh velocity.
        """ # nopep8
        return self._cards[0].get_value("pstype")

    @pstype.setter
    def pstype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""pstype must be one of {0,1,2,3}""")
        self._cards[0].set_value("pstype", value)

    @property
    def vx(self) -> float:
        """Get or set the Constant velocities to be used when PSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Constant velocities to be used when PSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Constant velocities to be used when PSTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Point ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Point initial coordinates.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)
