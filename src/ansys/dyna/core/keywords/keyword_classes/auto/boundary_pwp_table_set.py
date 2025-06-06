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

"""Module providing the BoundaryPwpTableSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundaryPwpTableSet(KeywordBase):
    """DYNA BOUNDARY_PWP_TABLE_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PWP_TABLE_SET"

    def __init__(self, **kwargs):
        """Initialize the BoundaryPwpTableSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tbirth",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "tdeath",
                        float,
                        50,
                        10,
                        1.0E20,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "itotex",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "table",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lc(self) -> typing.Optional[float]:
        """Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: float) -> None:
        """Set the lc property."""
        self._cards[0].set_value("lc", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Time at which boundary condition becomes active
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Time at which boundary condition becomes inactive
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
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
        """Set the itotex property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""itotex must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("itotex", value)

    @property
    def table(self) -> int:
        """Get or set the Table ID for TABLE option only. See notes below.
        """ # nopep8
        return self._cards[1].get_value("table")

    @table.setter
    def table(self, value: int) -> None:
        """Set the table property."""
        self._cards[1].set_value("table", value)

