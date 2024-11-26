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

class BoundaryPrecrack(KeywordBase):
    """DYNA BOUNDARY_PRECRACK keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRECRACK"

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
                        "ctype",
                        int,
                        10,
                        10,
                        kwargs.get("ctype", 1)
                    ),
                    Field(
                        "np",
                        int,
                        20,
                        10,
                        kwargs.get("np")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x",
                        float,
                        0,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        10,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        20,
                        10,
                        kwargs.get("z")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID where the pre-crack is located
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ctype(self) -> int:
        """Get or set the Type of pre-crack:
        EQ.1: straight line
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        self._cards[0].set_value("ctype", value)

    @property
    def np(self) -> typing.Optional[int]:
        """Get or set the Number of points defining the pre-crack
        """ # nopep8
        return self._cards[0].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        self._cards[0].set_value("np", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the points defining the pre-crack.  It is recommended that these points be defined such that the pre-crack does not coincide with mesh lines.   A pre-crack coinciding with mesh lines will be automatically moved with sometimes unexpected results, e.g., the moved pre-crack location does not lie on part PID and the pre-crack cannot be created
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

