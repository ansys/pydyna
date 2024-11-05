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

class ConstrainedLocal(KeywordBase):
    """DYNA CONSTRAINED_LOCAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "LOCAL"

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
                        "heading",
                        int,
                        10,
                        70,
                        kwargs.get("heading")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tc",
                        int,
                        0,
                        10,
                        kwargs.get("tc", 1)
                    ),
                    Field(
                        "rc",
                        int,
                        10,
                        10,
                        kwargs.get("rc", 1)
                    ),
                    Field(
                        "dir",
                        int,
                        20,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "x",
                        float,
                        30,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        40,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        50,
                        10,
                        kwargs.get("z")
                    ),
                    Field(
                        "cid",
                        int,
                        60,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "tol",
                        float,
                        70,
                        10,
                        kwargs.get("tol", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional ID which can be referred to by *SENSOR_CONTROL.
        This ID must be unique and cannot be shared with * BOUNDARY_SPC.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[int]:
        """Get or set the An optional descriptor that will be written into the d3hsp file and the spcforc file.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: int) -> None:
        self._cards[0].set_value("heading", value)

    @property
    def tc(self) -> int:
        """Get or set the Translational Constraint:
        EQ.1: constrained x translation,
        EQ.2: constrained y translation
        EQ.3: constrained z translation,
        EQ.4: constrained x and y translation,
        EQ.5: constrained y and z translation,
        EQ.6: constrained z and x translation,
        EQ.7: constrained x,y and z translation.
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""tc must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("tc", value)

    @property
    def rc(self) -> int:
        """Get or set the Rotaional Constraint:
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotaion
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x,y and z rotations.
        """ # nopep8
        return self._cards[1].get_value("rc")

    @rc.setter
    def rc(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rc must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("rc", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction of normal
        EQ.1:local x,
        EQ.2: local y,
        EQ.3:local z
        """ # nopep8
        return self._cards[1].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""dir must be one of {1,2,3}""")
        self._cards[1].set_value("dir", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Local x-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Local y-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Local z-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for orientation of the local coordinate system
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def tol(self) -> float:
        """Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[1].set_value("tol", value)

