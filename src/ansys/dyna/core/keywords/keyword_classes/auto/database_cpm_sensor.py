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

class DatabaseCpmSensor(KeywordBase):
    """DYNA DATABASE_CPM_SENSOR keyword"""

    keyword = "DATABASE"
    subkeyword = "CPM_SENSOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "binary",
                        int,
                        10,
                        10,
                        kwargs.get("binary", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "segsid",
                        int,
                        0,
                        10,
                        kwargs.get("segsid")
                    ),
                    Field(
                        "offset",
                        float,
                        10,
                        10,
                        kwargs.get("offset")
                    ),
                    Field(
                        "r/lx",
                        float,
                        20,
                        10,
                        kwargs.get("r/lx")
                    ),
                    Field(
                        "len/ly",
                        float,
                        30,
                        10,
                        kwargs.get("len/ly")
                    ),
                    Field(
                        "lz",
                        float,
                        40,
                        10,
                        kwargs.get("lz")
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Output interval
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for the binary file
        EQ.1:  ASCII file is written,
        EQ.2:  Data written to the binary file binout,
        EQ.3:  ASCII file  is written and the data written to the binary file binout
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""binary must be one of {1,2,3}""")
        self._cards[0].set_value("binary", value)

    @property
    def segsid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[1].get_value("segsid")

    @segsid.setter
    def segsid(self, value: int) -> None:
        self._cards[1].set_value("segsid", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset distance between the center of the sensor and  the segment center. If it is positive, it is on the side pointed to by the segment normal vector.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[1].set_value("offset", value)

    @property
    def r_lx(self) -> typing.Optional[float]:
        """Get or set the Radius(sphere)/length in local X direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("r/lx")

    @r_lx.setter
    def r_lx(self, value: float) -> None:
        self._cards[1].set_value("r/lx", value)

    @property
    def len_ly(self) -> typing.Optional[float]:
        """Get or set the Length(cylinder)/length in local Y direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("len/ly")

    @len_ly.setter
    def len_ly(self, value: float) -> None:
        self._cards[1].set_value("len/ly", value)

    @property
    def lz(self) -> typing.Optional[float]:
        """Get or set the Length in local Z direction(rectangular) of the sensor.
        """ # nopep8
        return self._cards[1].get_value("lz")

    @lz.setter
    def lz(self, value: float) -> None:
        self._cards[1].set_value("lz", value)

