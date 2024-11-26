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

class Iga1DNurbsXyz(KeywordBase):
    """DYNA IGA_1D_NURBS_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "1D_NURBS_XYZ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "nr",
                        int,
                        10,
                        10,
                        kwargs.get("nr")
                    ),
                    Field(
                        "pr",
                        int,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unir",
                        int,
                        0,
                        10,
                        kwargs.get("unir", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r1",
                        float,
                        0,
                        20,
                        kwargs.get("r1")
                    ),
                    Field(
                        "r2",
                        float,
                        20,
                        20,
                        kwargs.get("r2")
                    ),
                    Field(
                        "r3",
                        float,
                        40,
                        20,
                        kwargs.get("r3")
                    ),
                    Field(
                        "r4",
                        float,
                        60,
                        20,
                        kwargs.get("r4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rfirst",
                        float,
                        0,
                        20,
                        kwargs.get("rfirst")
                    ),
                    Field(
                        "rlast",
                        float,
                        20,
                        20,
                        kwargs.get("rlast")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x",
                        float,
                        0,
                        20,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        20,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        20,
                        kwargs.get("z")
                    ),
                    Field(
                        "wgt",
                        float,
                        60,
                        20,
                        kwargs.get("wgt", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical univariate NURBS ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of control points in the local r-direction.
        """ # nopep8
        return self._cards[0].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        self._cards[0].set_value("nr", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local r-direction.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def unir(self) -> int:
        """Get or set the Knot vector type in the local r-direction.
        EQ.0: Specify the entire knot vector in the local r - direction.
        EQ.1 : Uniform open knot vector in the local r - direction.
        EQ.2 : Uniform periodic knot vector in the local r - direction.
        """ # nopep8
        return self._cards[1].get_value("unir")

    @unir.setter
    def unir(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""unir must be one of {0,1,2}""")
        self._cards[1].set_value("unir", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
        """ # nopep8
        return self._cards[2].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[2].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
        """ # nopep8
        return self._cards[2].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[2].set_value("r2", value)

    @property
    def r3(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
        """ # nopep8
        return self._cards[2].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        self._cards[2].set_value("r3", value)

    @property
    def r4(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
        """ # nopep8
        return self._cards[2].get_value("r4")

    @r4.setter
    def r4(self, value: float) -> None:
        self._cards[2].set_value("r4", value)

    @property
    def rfirst(self) -> typing.Optional[float]:
        """Get or set the First knot value in the local r-direction.
        """ # nopep8
        return self._cards[3].get_value("rfirst")

    @rfirst.setter
    def rfirst(self, value: float) -> None:
        self._cards[3].set_value("rfirst", value)

    @property
    def rlast(self) -> typing.Optional[float]:
        """Get or set the Last knot value in the local r-direction.
        """ # nopep8
        return self._cards[3].get_value("rlast")

    @rlast.setter
    def rlast(self, value: float) -> None:
        self._cards[3].set_value("rlast", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global x-direction with j = 1, NR.
        """ # nopep8
        return self._cards[4].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[4].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global y-direction with j = 1, NR.
        """ # nopep8
        return self._cards[4].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[4].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global z-direction with j = 1, NR.
        """ # nopep8
        return self._cards[4].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[4].set_value("z", value)

    @property
    def wgt(self) -> float:
        """Get or set the Control weights with j = 1, NR, see Remark 2.
        """ # nopep8
        return self._cards[4].get_value("wgt")

    @wgt.setter
    def wgt(self, value: float) -> None:
        self._cards[4].set_value("wgt", value)

