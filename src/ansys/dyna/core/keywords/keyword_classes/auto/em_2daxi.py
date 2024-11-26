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

class Em2Daxi(KeywordBase):
    """DYNA EM_2DAXI keyword"""

    keyword = "EM"
    subkeyword = "2DAXI"

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
                    Field(
                        "ssid",
                        int,
                        10,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "starssid",
                        int,
                        40,
                        10,
                        kwargs.get("starssid")
                    ),
                    Field(
                        "endssid",
                        int,
                        50,
                        10,
                        kwargs.get("endssid")
                    ),
                    Field(
                        "numsec",
                        int,
                        60,
                        10,
                        kwargs.get("numsec")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the part to be solved using 2D axisymmetry.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID : Segment that will define the 2D cross section of the part where the EM field is solved.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def starssid(self) -> typing.Optional[int]:
        """Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
        """ # nopep8
        return self._cards[0].get_value("starssid")

    @starssid.setter
    def starssid(self, value: int) -> None:
        self._cards[0].set_value("starssid", value)

    @property
    def endssid(self) -> typing.Optional[int]:
        """Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
        """ # nopep8
        return self._cards[0].get_value("endssid")

    @endssid.setter
    def endssid(self, value: int) -> None:
        self._cards[0].set_value("endssid", value)

    @property
    def numsec(self) -> typing.Optional[int]:
        """Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh. This has to be a power of two.
        """ # nopep8
        return self._cards[0].get_value("numsec")

    @numsec.setter
    def numsec(self, value: int) -> None:
        self._cards[0].set_value("numsec", value)

