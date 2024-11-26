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

class DefineCurveCompensationConstraintBegin(KeywordBase):
    """DYNA DEFINE_CURVE_COMPENSATION_CONSTRAINT_BEGIN keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_COMPENSATION_CONSTRAINT_BEGIN"
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
                        "crvid",
                        int,
                        0,
                        10,
                        kwargs.get("crvid")
                    ),
                    Field(
                        "in/out",
                        int,
                        10,
                        10,
                        kwargs.get("in/out")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x",
                        float,
                        0,
                        16,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        16,
                        16,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        32,
                        16,
                        kwargs.get("z", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveCompensationConstraintBegin.option_specs[0],
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
    def crvid(self) -> typing.Optional[int]:
        """Get or set the Curve ID; must be unique, with the same begin and end point coordinates.
        """ # nopep8
        return self._cards[0].get_value("crvid")

    @crvid.setter
    def crvid(self, value: int) -> None:
        self._cards[0].set_value("crvid", value)

    @property
    def in_out(self) -> typing.Optional[int]:
        """Get or set the Flag to indicate local area to be compensated:
        EQ.1: Compensate area includes enclosed curve under keyword 'BEGIN' and transition area between the two curves; no changes will be made to the area outside the curve under keyword , 'END'
        """ # nopep8
        return self._cards[0].get_value("in/out")

    @in_out.setter
    def in_out(self, value: int) -> None:
        self._cards[0].set_value("in/out", value)

    @property
    def x(self) -> float:
        """Get or set the X-coordinate of a point on the curve.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Y-coordinate of a point on the curve
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Z-coordinate of a point on the curve.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

