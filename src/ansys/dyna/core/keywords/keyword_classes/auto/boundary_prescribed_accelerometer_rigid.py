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

class BoundaryPrescribedAccelerometerRigid(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_ACCELEROMETER_RIGID keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_ACCELEROMETER_RIGID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "lcidx",
                        int,
                        20,
                        10,
                        kwargs.get("lcidx")
                    ),
                    Field(
                        "lcidy",
                        int,
                        30,
                        10,
                        kwargs.get("lcidy")
                    ),
                    Field(
                        "lcidz",
                        int,
                        40,
                        10,
                        kwargs.get("lcidz")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body whose motion is prescribed.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID corresponding to the location of the accelerometer
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID describing the orientation of the accelerometer's local axes
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local x-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local y-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID containing the local z-acceleration time history from the accelerometer.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[1].set_value("lcidz", value)

