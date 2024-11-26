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

class InitialAirbagParticlePosition(KeywordBase):
    """DYNA INITIAL_AIRBAG_PARTICLE_POSITION keyword"""

    keyword = "INITIAL"
    subkeyword = "AIRBAG_PARTICLE_POSITION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bag_id",
                        int,
                        0,
                        10,
                        kwargs.get("bag_id")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "x",
                        float,
                        10,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        30,
                        10,
                        kwargs.get("z")
                    ),
                ],
            ),
        ]

    @property
    def bag_id(self) -> typing.Optional[int]:
        """Get or set the Airbag ID defined in *AIRBAG_‌PARTICLE_‌ID card
        """ # nopep8
        return self._cards[0].get_value("bag_id")

    @bag_id.setter
    def bag_id(self, value: int) -> None:
        self._cards[0].set_value("bag_id", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x coordinate
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y coordinate
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z coordinate
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

