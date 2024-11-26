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

class ChangeVelocity(KeywordBase):
    """DYNA CHANGE_VELOCITY keyword"""

    keyword = "CHANGE"
    subkeyword = "VELOCITY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vx",
                        float,
                        0,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        10,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        20,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "vxr",
                        float,
                        30,
                        10,
                        kwargs.get("vxr", 0.0)
                    ),
                    Field(
                        "vyr",
                        float,
                        40,
                        10,
                        kwargs.get("vyr", 0.0)
                    ),
                    Field(
                        "vzr",
                        float,
                        50,
                        10,
                        kwargs.get("vzr", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID containing nodes for initial velocity.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def vx(self) -> float:
        """Get or set the Velocity in x-direction.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Velocity in y-direction.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Velocity in z-direction.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[1].set_value("vz", value)

    @property
    def vxr(self) -> float:
        """Get or set the Rotational velocity about the x-axis.
        """ # nopep8
        return self._cards[1].get_value("vxr")

    @vxr.setter
    def vxr(self, value: float) -> None:
        self._cards[1].set_value("vxr", value)

    @property
    def vyr(self) -> float:
        """Get or set the Rotational velocity about the y-axis.
        """ # nopep8
        return self._cards[1].get_value("vyr")

    @vyr.setter
    def vyr(self, value: float) -> None:
        self._cards[1].set_value("vyr", value)

    @property
    def vzr(self) -> float:
        """Get or set the Rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[1].get_value("vzr")

    @vzr.setter
    def vzr(self, value: float) -> None:
        self._cards[1].set_value("vzr", value)

