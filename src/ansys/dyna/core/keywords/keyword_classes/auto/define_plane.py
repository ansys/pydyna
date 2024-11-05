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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefinePlane(KeywordBase):
    """DYNA DEFINE_PLANE keyword"""

    keyword = "DEFINE"
    subkeyword = "PLANE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "x1",
                        float,
                        10,
                        10,
                        kwargs.get("x1", 0.0)
                    ),
                    Field(
                        "y1",
                        float,
                        20,
                        10,
                        kwargs.get("y1", 0.0)
                    ),
                    Field(
                        "z1",
                        float,
                        30,
                        10,
                        kwargs.get("z1", 0.0)
                    ),
                    Field(
                        "x2",
                        float,
                        40,
                        10,
                        kwargs.get("x2", 0.0)
                    ),
                    Field(
                        "y2",
                        float,
                        50,
                        10,
                        kwargs.get("y2", 0.0)
                    ),
                    Field(
                        "z2",
                        float,
                        60,
                        10,
                        kwargs.get("z2", 0.0)
                    ),
                    Field(
                        "cid",
                        int,
                        70,
                        10,
                        kwargs.get("cid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x3",
                        float,
                        0,
                        10,
                        kwargs.get("x3", 0.0)
                    ),
                    Field(
                        "y3",
                        float,
                        10,
                        10,
                        kwargs.get("y3", 0.0)
                    ),
                    Field(
                        "z3",
                        float,
                        20,
                        10,
                        kwargs.get("z3", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefinePlane.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def pid(self) -> int:
        """Get or set the Plane ID. A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def x1(self) -> float:
        """Get or set the X-coordinate of point 1.
        """ # nopep8
        return self._cards[0].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[0].set_value("x1", value)

    @property
    def y1(self) -> float:
        """Get or set the Y-coordinate of point 1.
        """ # nopep8
        return self._cards[0].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[0].set_value("y1", value)

    @property
    def z1(self) -> float:
        """Get or set the Z-coordinate of point 1.
        """ # nopep8
        return self._cards[0].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[0].set_value("z1", value)

    @property
    def x2(self) -> float:
        """Get or set the X-coordinate of point 2.
        """ # nopep8
        return self._cards[0].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[0].set_value("x2", value)

    @property
    def y2(self) -> float:
        """Get or set the Y-coordinate of point 2.
        """ # nopep8
        return self._cards[0].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[0].set_value("y2", value)

    @property
    def z2(self) -> float:
        """Get or set the Z-coordinate of point 2.
        """ # nopep8
        return self._cards[0].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[0].set_value("z2", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID applied to the coordinates used to define the current plane. The coordinates X1, Y1, Z1, X2, Y2, Z2, X3, Y3 and Z3 are defined with respect to the coordinate system CID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def x3(self) -> float:
        """Get or set the X-coordinate of point 3.
        """ # nopep8
        return self._cards[1].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        self._cards[1].set_value("x3", value)

    @property
    def y3(self) -> float:
        """Get or set the Y-coordinate of point 3.
        """ # nopep8
        return self._cards[1].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        self._cards[1].set_value("y3", value)

    @property
    def z3(self) -> float:
        """Get or set the Z-coordinate of point 3.
        """ # nopep8
        return self._cards[1].get_value("z3")

    @z3.setter
    def z3(self, value: float) -> None:
        self._cards[1].set_value("z3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

