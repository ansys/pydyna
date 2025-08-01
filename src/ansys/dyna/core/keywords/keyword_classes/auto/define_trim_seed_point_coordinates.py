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

"""Module providing the DefineTrimSeedPointCoordinates class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineTrimSeedPointCoordinates(KeywordBase):
    """DYNA DEFINE_TRIM_SEED_POINT_COORDINATES keyword"""

    keyword = "DEFINE"
    subkeyword = "TRIM_SEED_POINT_COORDINATES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTrimSeedPointCoordinates class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "nseed",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "x1",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "y1",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "z1",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "x2",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "y2",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "z2",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineTrimSeedPointCoordinates.option_specs[0],
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
    def nseed(self) -> int:
        """Get or set the Number of seed points. Maximum value of two is allowed.
        """ # nopep8
        return self._cards[0].get_value("nseed")

    @nseed.setter
    def nseed(self, value: int) -> None:
        """Set the nseed property."""
        self._cards[0].set_value("nseed", value)

    @property
    def x1(self) -> float:
        """Get or set the x-coordinate of first node.
        """ # nopep8
        return self._cards[0].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[0].set_value("x1", value)

    @property
    def y1(self) -> float:
        """Get or set the y-coordinate of first node.
        """ # nopep8
        return self._cards[0].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[0].set_value("y1", value)

    @property
    def z1(self) -> float:
        """Get or set the z-coordinate of first node.
        """ # nopep8
        return self._cards[0].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[0].set_value("z1", value)

    @property
    def x2(self) -> float:
        """Get or set the x-coordinate of second node.
        """ # nopep8
        return self._cards[0].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[0].set_value("x2", value)

    @property
    def y2(self) -> float:
        """Get or set the y-coordinate of second node.
        """ # nopep8
        return self._cards[0].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[0].set_value("y2", value)

    @property
    def z2(self) -> float:
        """Get or set the z-coordinate of second node.
        """ # nopep8
        return self._cards[0].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[0].set_value("z2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

