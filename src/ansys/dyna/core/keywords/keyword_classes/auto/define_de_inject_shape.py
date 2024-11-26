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

class DefineDeInjectShape(KeywordBase):
    """DYNA DEFINE_DE_INJECT_SHAPE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECT_SHAPE"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "nde",
                        int,
                        10,
                        10,
                        kwargs.get("nde")
                    ),
                    Field(
                        "iauto",
                        int,
                        20,
                        10,
                        kwargs.get("iauto", 0)
                    ),
                    Field(
                        "itype",
                        int,
                        30,
                        10,
                        kwargs.get("itype", 1)
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
                    Field(
                        "r",
                        float,
                        30,
                        10,
                        kwargs.get("r")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeInjectShape.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the The ID of the shape pattern
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def nde(self) -> typing.Optional[int]:
        """Get or set the Number of DEs in this pattern
        """ # nopep8
        return self._cards[0].get_value("nde")

    @nde.setter
    def nde(self, value: int) -> None:
        self._cards[0].set_value("nde", value)

    @property
    def iauto(self) -> int:
        """Get or set the Flag for how to specify the bonded shape patterns:
        EQ.0:	Give each particle’s relative position and radius
        EQ.1 : Use predefined pattern types
        """ # nopep8
        return self._cards[0].get_value("iauto")

    @iauto.setter
    def iauto(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iauto must be one of {0,1}""")
        self._cards[0].set_value("iauto", value)

    @property
    def itype(self) -> int:
        """Get or set the Bond particles patterns when IAUTO = 1:
        EQ.1:	Line
        EQ.2 : Cuboid
        EQ.3 : Prism with equilateral triangle faces
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""itype must be one of {1,2,3}""")
        self._cards[0].set_value("itype", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the DEs in this pattern.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the DEs in this pattern.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the DEs in this pattern.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radii of the DEs in this pattern
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

