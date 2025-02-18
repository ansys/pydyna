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

class Iga3DNurbsXyz(KeywordBase):
    """DYNA IGA_3D_NURBS_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "3D_NURBS_XYZ"

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
                        **kwargs,
                    ),
                    Field(
                        "nr",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ns",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nt",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ps",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pt",
                        int,
                        60,
                        10,
                        **kwargs,
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unis",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unit",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "r2",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "r3",
                        float,
                        40,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "r4",
                        float,
                        60,
                        20,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "rlast",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s1",
                        float,
                        0,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "s2",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "s3",
                        float,
                        40,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "s4",
                        float,
                        60,
                        20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfirst",
                        float,
                        0,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "slast",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t1",
                        float,
                        0,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "t2",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "t3",
                        float,
                        40,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "t4",
                        float,
                        60,
                        20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tfirst",
                        float,
                        0,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "tlast",
                        float,
                        20,
                        20,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        20,
                        **kwargs,
                    ),
                    Field(
                        "wgt",
                        float,
                        60,
                        20,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical trivariate NURBS ID. A unique number must be chosen.
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
    def ns(self) -> typing.Optional[int]:
        """Get or set the Number of control points in the local s-direction.
        """ # nopep8
        return self._cards[0].get_value("ns")

    @ns.setter
    def ns(self, value: int) -> None:
        self._cards[0].set_value("ns", value)

    @property
    def nt(self) -> typing.Optional[int]:
        """Get or set the Number of control points in the local t-direction.
        """ # nopep8
        return self._cards[0].get_value("nt")

    @nt.setter
    def nt(self, value: int) -> None:
        self._cards[0].set_value("nt", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local r-direction.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local s-direction.
        """ # nopep8
        return self._cards[0].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        self._cards[0].set_value("ps", value)

    @property
    def pt(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local t-direction.
        """ # nopep8
        return self._cards[0].get_value("pt")

    @pt.setter
    def pt(self, value: int) -> None:
        self._cards[0].set_value("pt", value)

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
        if value not in [0, 1, 2, None]:
            raise Exception("""unir must be `None` or one of {0,1,2}""")
        self._cards[1].set_value("unir", value)

    @property
    def unis(self) -> int:
        """Get or set the Knot vector type in the local s-direction.
        EQ.0: Specify the entire knot vector in the local s - direction.
        EQ.1 : Uniform open knot vector in the local s - direction.
        EQ.2 : Uniform periodic knot vector in the local s - direction.
        """ # nopep8
        return self._cards[1].get_value("unis")

    @unis.setter
    def unis(self, value: int) -> None:
        if value not in [0, 1, 2, None]:
            raise Exception("""unis must be `None` or one of {0,1,2}""")
        self._cards[1].set_value("unis", value)

    @property
    def unit(self) -> int:
        """Get or set the Knot vector type in the local t-direction.
        EQ.0: Specify the entire knot vector in the local t - direction.
        EQ.1 : Uniform open knot vector in the local t - direction.
        EQ.2 : Uniform periodic knot vector in the local t - direction.
        """ # nopep8
        return self._cards[1].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [0, 1, 2, None]:
            raise Exception("""unit must be `None` or one of {0,1,2}""")
        self._cards[1].set_value("unit", value)

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
    def s1(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local s-direction with j = 1, NS+PS+1..
        """ # nopep8
        return self._cards[4].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[4].set_value("s1", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
        """ # nopep8
        return self._cards[4].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        self._cards[4].set_value("s2", value)

    @property
    def s3(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
        """ # nopep8
        return self._cards[4].get_value("s3")

    @s3.setter
    def s3(self, value: float) -> None:
        self._cards[4].set_value("s3", value)

    @property
    def s4(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
        """ # nopep8
        return self._cards[4].get_value("s4")

    @s4.setter
    def s4(self, value: float) -> None:
        self._cards[4].set_value("s4", value)

    @property
    def sfirst(self) -> typing.Optional[float]:
        """Get or set the First knot value in the local s-direction.
        """ # nopep8
        return self._cards[5].get_value("sfirst")

    @sfirst.setter
    def sfirst(self, value: float) -> None:
        self._cards[5].set_value("sfirst", value)

    @property
    def slast(self) -> typing.Optional[float]:
        """Get or set the Last knot value in the local s-direction.
        """ # nopep8
        return self._cards[5].get_value("slast")

    @slast.setter
    def slast(self, value: float) -> None:
        self._cards[5].set_value("slast", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local t-direction with j = 1, NT+PT+1..
        """ # nopep8
        return self._cards[6].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[6].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
        """ # nopep8
        return self._cards[6].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[6].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
        """ # nopep8
        return self._cards[6].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[6].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
        """ # nopep8
        return self._cards[6].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[6].set_value("t4", value)

    @property
    def tfirst(self) -> typing.Optional[float]:
        """Get or set the First knot value in the local t-direction.
        """ # nopep8
        return self._cards[7].get_value("tfirst")

    @tfirst.setter
    def tfirst(self, value: float) -> None:
        self._cards[7].set_value("tfirst", value)

    @property
    def tlast(self) -> typing.Optional[float]:
        """Get or set the Last knot value in the local t-direction.
        """ # nopep8
        return self._cards[7].get_value("tlast")

    @tlast.setter
    def tlast(self, value: float) -> None:
        self._cards[7].set_value("tlast", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global x-direction with k = 1, NR*NS*NT.
        """ # nopep8
        return self._cards[8].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[8].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global y-direction with j = 1, NR*NS*NT.
        """ # nopep8
        return self._cards[8].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[8].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Non-homogeneous control point coordinates in the global z-direction with j = 1, NR*NS*NT.
        """ # nopep8
        return self._cards[8].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[8].set_value("z", value)

    @property
    def wgt(self) -> float:
        """Get or set the Control weights with j = 1, NR*NS, see Remark 3.
        """ # nopep8
        return self._cards[8].get_value("wgt")

    @wgt.setter
    def wgt(self, value: float) -> None:
        self._cards[8].set_value("wgt", value)

