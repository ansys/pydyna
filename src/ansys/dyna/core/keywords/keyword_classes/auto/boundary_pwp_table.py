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

class BoundaryPwpTable(KeywordBase):
    """DYNA BOUNDARY_PWP_TABLE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PWP_TABLE"

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
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tbirth",
                        float,
                        40,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "tdeath",
                        float,
                        50,
                        10,
                        kwargs.get("tdeath", 1.0E20)
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
                        "itotex",
                        int,
                        10,
                        10,
                        kwargs.get("itotex", 0)
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "table",
                        int,
                        30,
                        10,
                        kwargs.get("table", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Time at which boundary condition becomes active
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Time at which boundary condition becomes inactive
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def itotex(self) -> int:
        """Get or set the Flag for type of pressure boundary condition: (see notes)
        =0: 	Total head
        =1: 	Excess head
        =2:	Hydraulic head
        """ # nopep8
        return self._cards[1].get_value("itotex")

    @itotex.setter
    def itotex(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""itotex must be one of {0,1,2}""")
        self._cards[1].set_value("itotex", value)

    @property
    def table(self) -> int:
        """Get or set the Table ID for TABLE option only. See notes below.
        """ # nopep8
        return self._cards[1].get_value("table")

    @table.setter
    def table(self, value: int) -> None:
        self._cards[1].set_value("table", value)

