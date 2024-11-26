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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SensorDefineCalcMath(KeywordBase):
    """DYNA SENSOR_DEFINE_CALC_MATH keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_CALC_MATH"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sensid",
                        int,
                        0,
                        10,
                        kwargs.get("sensid")
                    ),
                    Field(
                        "calc",
                        str,
                        10,
                        10,
                        kwargs.get("calc")
                    ),
                    Field(
                        "sens1",
                        int,
                        20,
                        10,
                        kwargs.get("sens1")
                    ),
                    Field(
                        "sens2",
                        int,
                        30,
                        10,
                        kwargs.get("sens2")
                    ),
                    Field(
                        "sens3",
                        int,
                        40,
                        10,
                        kwargs.get("sens3")
                    ),
                    Field(
                        "sens4",
                        int,
                        50,
                        10,
                        kwargs.get("sens4")
                    ),
                    Field(
                        "sens5",
                        int,
                        60,
                        10,
                        kwargs.get("sens5")
                    ),
                    Field(
                        "sens6",
                        int,
                        70,
                        10,
                        kwargs.get("sens6")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorDefineCalcMath.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the sensor ID
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        self._cards[0].set_value("sensid", value)

    @property
    def calc(self) -> typing.Optional[str]:
        """Get or set the Mathematic calculation.
        """ # nopep8
        return self._cards[0].get_value("calc")

    @calc.setter
    def calc(self, value: str) -> None:
        self._cards[0].set_value("calc", value)

    @property
    def sens1(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens1")

    @sens1.setter
    def sens1(self, value: int) -> None:
        self._cards[0].set_value("sens1", value)

    @property
    def sens2(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens2")

    @sens2.setter
    def sens2(self, value: int) -> None:
        self._cards[0].set_value("sens2", value)

    @property
    def sens3(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens3")

    @sens3.setter
    def sens3(self, value: int) -> None:
        self._cards[0].set_value("sens3", value)

    @property
    def sens4(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens4")

    @sens4.setter
    def sens4(self, value: int) -> None:
        self._cards[0].set_value("sens4", value)

    @property
    def sens5(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens5")

    @sens5.setter
    def sens5(self, value: int) -> None:
        self._cards[0].set_value("sens5", value)

    @property
    def sens6(self) -> typing.Optional[int]:
        """Get or set the ith Sensor ID
        """ # nopep8
        return self._cards[0].get_value("sens6")

    @sens6.setter
    def sens6(self, value: int) -> None:
        self._cards[0].set_value("sens6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

