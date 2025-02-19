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

class ContactAutoMove(KeywordBase):
    """DYNA CONTACT_AUTO_MOVE keyword"""

    keyword = "CONTACT"
    subkeyword = "AUTO_MOVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "atime",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "offset",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for this auto positioning input
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact ID
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for a vector oriented in the direction of the movement of the master surface. See *DEFINE_VECTOR. The origin of this vector is unimportant since the direction consines of the vector are computed and used
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Optional loas curve ID defining velocity versus time. The load curve should be defined by four points, and its shape should resemble a trapzoid with the longest parallel side along the abcissa. The abcissa is adjusted(shortened)in the flat part of the curve where the velocity is constant to account for the movement
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def atime(self) -> typing.Optional[float]:
        """Get or set the Activeation time. A this time the master surface is moved
        """ # nopep8
        return self._cards[0].get_value("atime")

    @atime.setter
    def atime(self, value: float) -> None:
        self._cards[0].set_value("atime", value)

    @property
    def offset(self) -> float:
        """Get or set the Time at which a master surface will move to close a gap distance,
        which may happen following the move of another master surface.
        This is useful in sequential multiple flanging or press hemming
        simulation. Simulation time (CPU) is much faster based on the
        shortened tool travel (no change to the termination time).
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[0].set_value("offset", value)

