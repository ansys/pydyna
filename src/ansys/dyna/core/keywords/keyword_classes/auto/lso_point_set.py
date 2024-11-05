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

class LsoPointSet(KeywordBase):
    """DYNA LSO_POINT_SET keyword"""

    keyword = "LSO"
    subkeyword = "POINT_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "use",
                        int,
                        10,
                        10,
                        kwargs.get("use", 1)
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
    def setid(self) -> typing.Optional[int]:
        """Get or set the Identifier for this point set. Called by *LSO_DOMAIN.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def use(self) -> int:
        """Get or set the Points in this set are used as:
        EQ.1: Fixed time history points (default)
        EQ.2: Positions of tracer particles
        """ # nopep8
        return self._cards[0].get_value("use")

    @use.setter
    def use(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""use must be one of {1,2}""")
        self._cards[0].set_value("use", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the point.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

