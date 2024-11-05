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

class BatteryEchemThermal(KeywordBase):
    """DYNA BATTERY_ECHEM_THERMAL keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_THERMAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "thame",
                        str,
                        0,
                        10,
                        kwargs.get("thame")
                    ),
                    Field(
                        "tid",
                        int,
                        10,
                        10,
                        kwargs.get("tid")
                    ),
                    Field(
                        "iprt",
                        int,
                        20,
                        10,
                        kwargs.get("iprt")
                    ),
                    Field(
                        "cp",
                        float,
                        30,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "hconv",
                        float,
                        40,
                        10,
                        kwargs.get("hconv")
                    ),
                    Field(
                        "temp",
                        float,
                        50,
                        10,
                        kwargs.get("temp")
                    ),
                    Field(
                        "dudt",
                        float,
                        60,
                        10,
                        kwargs.get("dudt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        256,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

    @property
    def thame(self) -> typing.Optional[str]:
        """Get or set the Thermal material identifier
        """ # nopep8
        return self._cards[0].get_value("thame")

    @thame.setter
    def thame(self, value: str) -> None:
        self._cards[0].set_value("thame", value)

    @property
    def tid(self) -> typing.Optional[int]:
        """Get or set the Material identifier
        EQ.0:	Constant temperature mode.
        EQ.1 : Isothermal temperature with time.
        EQ.2 : Thermal coupling with LS - DYNA thermal solver
        """ # nopep8
        return self._cards[0].get_value("tid")

    @tid.setter
    def tid(self, value: int) -> None:
        self._cards[0].set_value("tid", value)

    @property
    def iprt(self) -> typing.Optional[int]:
        """Get or set the Data print in ASCII format
        EQ.0:	No data print out.
        EQ.1 : Time vs.heat flux print out for thermal solver.
        EQ.2 : Time vs.cell temperature print out
        """ # nopep8
        return self._cards[0].get_value("iprt")

    @iprt.setter
    def iprt(self, value: int) -> None:
        self._cards[0].set_value("iprt", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the The specific heat coefficient of the cell. (J/Kg K)
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def hconv(self) -> typing.Optional[float]:
        """Get or set the Convective heat transfer coefficient with external medium. (W/m2K)
        """ # nopep8
        return self._cards[0].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        self._cards[0].set_value("hconv", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Ambient temperature around the cell stack. (K)
        """ # nopep8
        return self._cards[0].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[0].set_value("temp", value)

    @property
    def dudt(self) -> typing.Optional[float]:
        """Get or set the The temperature coefficient of open circuit potential (V/K).
        EQ.0:	Constant coefficient given by MULT.
        EQ.1 : Coefficient as function of temperature
        """ # nopep8
        return self._cards[0].get_value("dudt")

    @dudt.setter
    def dudt(self, value: float) -> None:
        self._cards[0].set_value("dudt", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the battery cell output file (ASCII)
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

