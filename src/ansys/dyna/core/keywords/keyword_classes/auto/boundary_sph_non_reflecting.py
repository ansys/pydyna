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

class BoundarySphNonReflecting(KeywordBase):
    """DYNA BOUNDARY_SPH_NON_REFLECTING keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_NON_REFLECTING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vtx",
                        float,
                        0,
                        10,
                        kwargs.get("vtx")
                    ),
                    Field(
                        "vty",
                        float,
                        10,
                        10,
                        kwargs.get("vty")
                    ),
                    Field(
                        "vtz",
                        float,
                        20,
                        10,
                        kwargs.get("vtz")
                    ),
                    Field(
                        "vhx",
                        float,
                        30,
                        10,
                        kwargs.get("vhx")
                    ),
                    Field(
                        "vhy",
                        float,
                        40,
                        10,
                        kwargs.get("vhy")
                    ),
                    Field(
                        "vhz",
                        float,
                        50,
                        10,
                        kwargs.get("vhz")
                    ),
                ],
            ),
        ]

    @property
    def vtx(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of tail of a normal vector originating on the wall
        (tail) and terminating in the body (head); that is, the vector points
        from the non-reflecting boundary plane to the body.
        """ # nopep8
        return self._cards[0].get_value("vtx")

    @vtx.setter
    def vtx(self, value: float) -> None:
        self._cards[0].set_value("vtx", value)

    @property
    def vty(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of tail.
        """ # nopep8
        return self._cards[0].get_value("vty")

    @vty.setter
    def vty(self, value: float) -> None:
        self._cards[0].set_value("vty", value)

    @property
    def vtz(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of tail.
        """ # nopep8
        return self._cards[0].get_value("vtz")

    @vtz.setter
    def vtz(self, value: float) -> None:
        self._cards[0].set_value("vtz", value)

    @property
    def vhx(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhx")

    @vhx.setter
    def vhx(self, value: float) -> None:
        self._cards[0].set_value("vhx", value)

    @property
    def vhy(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhy")

    @vhy.setter
    def vhy(self, value: float) -> None:
        self._cards[0].set_value("vhy", value)

    @property
    def vhz(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhz")

    @vhz.setter
    def vhz(self, value: float) -> None:
        self._cards[0].set_value("vhz", value)

