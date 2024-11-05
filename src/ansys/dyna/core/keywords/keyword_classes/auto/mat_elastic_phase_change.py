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

class MatElasticPhaseChange(KeywordBase):
    """DYNA MAT_ELASTIC_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_PHASE_CHANGE"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro1",
                        float,
                        10,
                        10,
                        kwargs.get("ro1")
                    ),
                    Field(
                        "e1",
                        float,
                        20,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "pr1",
                        float,
                        30,
                        10,
                        kwargs.get("pr1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ro2",
                        float,
                        10,
                        10,
                        kwargs.get("ro2")
                    ),
                    Field(
                        "e2",
                        float,
                        20,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "pr2",
                        float,
                        30,
                        10,
                        kwargs.get("pr2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x1",
                        float,
                        0,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "y1",
                        float,
                        10,
                        10,
                        kwargs.get("y1")
                    ),
                    Field(
                        "z1",
                        float,
                        20,
                        10,
                        kwargs.get("z1")
                    ),
                    Field(
                        "x2",
                        float,
                        30,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "y2",
                        float,
                        40,
                        10,
                        kwargs.get("y2")
                    ),
                    Field(
                        "z2",
                        float,
                        50,
                        10,
                        kwargs.get("z2")
                    ),
                    Field(
                        "thkfac",
                        float,
                        60,
                        10,
                        kwargs.get("thkfac", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatElasticPhaseChange.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label not exceeding 8	characters must be specified..
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro1(self) -> typing.Optional[float]:
        """Get or set the Mass density for phase i.
        """ # nopep8
        return self._cards[0].get_value("ro1")

    @ro1.setter
    def ro1(self, value: float) -> None:
        self._cards[0].set_value("ro1", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for phase i
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        self._cards[0].set_value("e1", value)

    @property
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for phase i.
        """ # nopep8
        return self._cards[0].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        self._cards[0].set_value("pr1", value)

    @property
    def ro2(self) -> typing.Optional[float]:
        """Get or set the Mass density for phase i.
        """ # nopep8
        return self._cards[1].get_value("ro2")

    @ro2.setter
    def ro2(self, value: float) -> None:
        self._cards[1].set_value("ro2", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for phase i
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        self._cards[1].set_value("e2", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for phase i.
        """ # nopep8
        return self._cards[1].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        self._cards[1].set_value("pr2", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[2].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane
        """ # nopep8
        return self._cards[2].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[2].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[2].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[2].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        self._cards[2].set_value("thkfac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

