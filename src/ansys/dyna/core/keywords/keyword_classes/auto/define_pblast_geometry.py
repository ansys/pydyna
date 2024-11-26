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

class DefinePblastGeometry(KeywordBase):
    """DYNA DEFINE_PBLAST_GEOMETRY keyword"""

    keyword = "DEFINE"
    subkeyword = "PBLAST_GEOMETRY"
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
                        "gid",
                        int,
                        0,
                        10,
                        kwargs.get("gid", 0)
                    ),
                    Field(
                        "gtype1",
                        int,
                        10,
                        10,
                        kwargs.get("gtype1", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xa",
                        float,
                        0,
                        10,
                        kwargs.get("xa", 0.0)
                    ),
                    Field(
                        "ya",
                        float,
                        10,
                        10,
                        kwargs.get("ya", 0.0)
                    ),
                    Field(
                        "za",
                        float,
                        20,
                        10,
                        kwargs.get("za", 0.0)
                    ),
                    Field(
                        "xb",
                        float,
                        30,
                        10,
                        kwargs.get("xb", 0.0)
                    ),
                    Field(
                        "yb",
                        float,
                        40,
                        10,
                        kwargs.get("yb", 0.0)
                    ),
                    Field(
                        "zb",
                        float,
                        50,
                        10,
                        kwargs.get("zb", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        10,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        20,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g1",
                        float,
                        0,
                        10,
                        kwargs.get("g1", 0.0)
                    ),
                    Field(
                        "g2",
                        float,
                        10,
                        10,
                        kwargs.get("g2", 0.0)
                    ),
                    Field(
                        "g3",
                        float,
                        20,
                        10,
                        kwargs.get("g3", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefinePblastGeometry.option_specs[0],
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
    def gid(self) -> int:
        """Get or set the ID of a GEOMETRY defining high explosive particle domain.
        """ # nopep8
        return self._cards[0].get_value("gid")

    @gid.setter
    def gid(self, value: int) -> None:
        self._cards[0].set_value("gid", value)

    @property
    def gtype1(self) -> int:
        """Get or set the Geometry type
        EQ.1: box
        EQ.2: sphere
        EQ.3: cylinder
        EQ.4: ellipsoid
        EQ.5: hemisphere (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("gtype1")

    @gtype1.setter
    def gtype1(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""gtype1 must be one of {1,2,3,4,5}""")
        self._cards[0].set_value("gtype1", value)

    @property
    def xa(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("xa")

    @xa.setter
    def xa(self, value: float) -> None:
        self._cards[1].set_value("xa", value)

    @property
    def ya(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("ya")

    @ya.setter
    def ya(self, value: float) -> None:
        self._cards[1].set_value("ya", value)

    @property
    def za(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("za")

    @za.setter
    def za(self, value: float) -> None:
        self._cards[1].set_value("za", value)

    @property
    def xb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("xb")

    @xb.setter
    def xb(self, value: float) -> None:
        self._cards[1].set_value("xb", value)

    @property
    def yb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("yb")

    @yb.setter
    def yb(self, value: float) -> None:
        self._cards[1].set_value("yb", value)

    @property
    def zb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("zb")

    @zb.setter
    def zb(self, value: float) -> None:
        self._cards[1].set_value("zb", value)

    @property
    def xc(self) -> float:
        """Get or set the X-coordinate of charge center.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y-coordinate of charge center.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z-coordinate of charge center.
        """ # nopep8
        return self._cards[2].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[2].set_value("zc", value)

    @property
    def g1(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of x edge
        GTYPE.EQ.2: Radius of sphere
        GTYPE.EQ.3: Radius of cross section
        GTYPE.EQ.4: length of x-axes
        GTYPE.EQ.5: Radius of hemisphere.
        """ # nopep8
        return self._cards[3].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        self._cards[3].set_value("g1", value)

    @property
    def g2(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of y edge
        GTYPE.EQ.3: length of cylinder
        GTYPE.EQ.4: length of y-axes.
        """ # nopep8
        return self._cards[3].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        self._cards[3].set_value("g2", value)

    @property
    def g3(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of z edge
        GTYPE.EQ.4: length of z-axes.
        """ # nopep8
        return self._cards[3].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        self._cards[3].set_value("g3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

