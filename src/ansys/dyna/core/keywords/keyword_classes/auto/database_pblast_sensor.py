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

class DatabasePblastSensor(KeywordBase):
    """DYNA DATABASE_PBLAST_SENSOR keyword"""

    keyword = "DATABASE"
    subkeyword = "PBLAST_SENSOR"

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
                        kwargs.get("dt", 0)
                    ),
                    Field(
                        "binary",
                        int,
                        10,
                        10,
                        kwargs.get("binary", 3)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        kwargs.get("itype", 0)
                    ),
                    Field(
                        "offset",
                        float,
                        20,
                        10,
                        kwargs.get("offset", 0)
                    ),
                    Field(
                        "radius",
                        float,
                        30,
                        10,
                        kwargs.get("radius", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> float:
        """Get or set the Output interval.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for the binary file:
        EQ.3:	Data is written to the binary file binout.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        self._cards[0].set_value("binary", value)

    @property
    def id(self) -> int:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[1].set_value("id", value)

    @property
    def itype(self) -> int:
        """Get or set the EQ.0: *SET_SHELL ID
        EQ.1: Shell ID.
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itype must be one of {0,1}""")
        self._cards[1].set_value("itype", value)

    @property
    def offset(self) -> float:
        """Get or set the Offset distance, d, between sensor and the segment center. Where d>0 is along shell normal and d<0 is against shell normal.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[1].set_value("offset", value)

    @property
    def radius(self) -> float:
        """Get or set the Radius of sphere of the sensor.  Please see *DATABASE_CPM_SENSOR for sphere sensor.
        """ # nopep8
        return self._cards[1].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[1].set_value("radius", value)

