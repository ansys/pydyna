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

class MatNull(KeywordBase):
    """DYNA MAT_NULL keyword"""

    keyword = "MAT"
    subkeyword = "NULL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "pc",
                        float,
                        20,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "mu",
                        float,
                        30,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "terod",
                        float,
                        40,
                        10,
                        kwargs.get("terod")
                    ),
                    Field(
                        "cerod",
                        float,
                        50,
                        10,
                        kwargs.get("cerod")
                    ),
                    Field(
                        "ym",
                        float,
                        60,
                        10,
                        kwargs.get("ym")
                    ),
                    Field(
                        "pr",
                        float,
                        70,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff (<= 0.0).
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Viscosity coefficient (optional).
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def terod(self) -> typing.Optional[float]:
        """Get or set the Relative volume. V/V0, for erosion in tension. Typically, use values greater than unity. If zero, erosion in tension is inactive.
        """ # nopep8
        return self._cards[0].get_value("terod")

    @terod.setter
    def terod(self, value: float) -> None:
        self._cards[0].set_value("terod", value)

    @property
    def cerod(self) -> typing.Optional[float]:
        """Get or set the Relative volume, V/V0, for erosion in compression. Typically, use values less than unity. If zero, erosion in compression is inactive.
        """ # nopep8
        return self._cards[0].get_value("cerod")

    @cerod.setter
    def cerod(self, value: float) -> None:
        self._cards[0].set_value("cerod", value)

    @property
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's modulus (used for null beams and shells only).
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        self._cards[0].set_value("ym", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (used for null beams and shells only).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

