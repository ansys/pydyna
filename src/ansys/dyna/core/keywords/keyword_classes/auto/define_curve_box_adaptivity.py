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

class DefineCurveBoxAdaptivity(KeywordBase):
    """DYNA DEFINE_CURVE_BOX_ADAPTIVITY keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_BOX_ADAPTIVITY"
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
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "level",
                        int,
                        20,
                        10,
                        kwargs.get("level")
                    ),
                    Field(
                        "dist1",
                        float,
                        30,
                        10,
                        kwargs.get("dist1")
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
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveBoxAdaptivity.option_specs[0],
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
        """Get or set the Curve ID; must be unique. The curve must be closed: its first and
        last point must coincide. See Examples
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Sheet blank Part ID, as in *PART
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def level(self) -> typing.Optional[int]:
        """Get or set the Adaptive refinement levels, similar to the field MAXLVL in
        *CONTROL_ADAPTIVE. See Remark
        """ # nopep8
        return self._cards[0].get_value("level")

    @level.setter
    def level(self, value: int) -> None:
        self._cards[0].set_value("level", value)

    @property
    def dist1(self) -> typing.Optional[float]:
        """Get or set the Depth in the ..-direction that the curve defined with Card 2 will
        be extruded. Currently this variable must be input as a negative value.
        """ # nopep8
        return self._cards[0].get_value("dist1")

    @dist1.setter
    def dist1(self, value: float) -> None:
        self._cards[0].set_value("dist1", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the X coordinate of a point on the curve.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of a point on the curve.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of a point on the curve.
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

