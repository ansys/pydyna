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

class SensorDefineFunction(KeywordBase):
    """DYNA SENSOR_DEFINE_FUNCTION keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_FUNCTION"
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
                        "func",
                        int,
                        10,
                        10,
                        kwargs.get("func")
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
            Card(
                [
                    Field(
                        "sensi",
                        int,
                        0,
                        10,
                        kwargs.get("sensi")
                    ),
                    Field(
                        "sensi+1",
                        int,
                        10,
                        10,
                        kwargs.get("sensi+1")
                    ),
                    Field(
                        "sensi+2",
                        int,
                        20,
                        10,
                        kwargs.get("sensi+2")
                    ),
                    Field(
                        "sensi+3",
                        int,
                        30,
                        10,
                        kwargs.get("sensi+3")
                    ),
                    Field(
                        "sensi+4",
                        int,
                        40,
                        10,
                        kwargs.get("sensi+4")
                    ),
                    Field(
                        "sensi+5",
                        int,
                        50,
                        10,
                        kwargs.get("sensi+5")
                    ),
                    Field(
                        "sensi+6",
                        int,
                        60,
                        10,
                        kwargs.get("sensi+6")
                    ),
                    Field(
                        "sensi+7",
                        int,
                        70,
                        10,
                        kwargs.get("sensi+7")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorDefineFunction.option_specs[0],
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
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        self._cards[0].set_value("sensid", value)

    @property
    def func(self) -> typing.Optional[int]:
        """Get or set the Function ID.
        """ # nopep8
        return self._cards[0].get_value("func")

    @func.setter
    def func(self, value: int) -> None:
        self._cards[0].set_value("func", value)

    @property
    def sens1(self) -> typing.Optional[int]:
        """Get or set the 1st Sensor ID, the value of which will be used as the 1st argument of
        function FUNC. If defined as negative, the absolute value of SENS1,
        |SENS1|, is the number of sensors to be input. If |SENS1| > 5,
        additional cards will be needed to input the ID of all sensors. The number of sensor is limited to 15
        """ # nopep8
        return self._cards[0].get_value("sens1")

    @sens1.setter
    def sens1(self, value: int) -> None:
        self._cards[0].set_value("sens1", value)

    @property
    def sens2(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens2")

    @sens2.setter
    def sens2(self, value: int) -> None:
        self._cards[0].set_value("sens2", value)

    @property
    def sens3(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens3")

    @sens3.setter
    def sens3(self, value: int) -> None:
        self._cards[0].set_value("sens3", value)

    @property
    def sens4(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[0].get_value("sens4")

    @sens4.setter
    def sens4(self, value: int) -> None:
        self._cards[0].set_value("sens4", value)

    @property
    def sens5(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens5")

    @sens5.setter
    def sens5(self, value: int) -> None:
        self._cards[0].set_value("sens5", value)

    @property
    def sens6(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[0].get_value("sens6")

    @sens6.setter
    def sens6(self, value: int) -> None:
        self._cards[0].set_value("sens6", value)

    @property
    def sensi(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi")

    @sensi.setter
    def sensi(self, value: int) -> None:
        self._cards[1].set_value("sensi", value)

    @property
    def sensi_1(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi+1")

    @sensi_1.setter
    def sensi_1(self, value: int) -> None:
        self._cards[1].set_value("sensi+1", value)

    @property
    def sensi_2(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[1].get_value("sensi+2")

    @sensi_2.setter
    def sensi_2(self, value: int) -> None:
        self._cards[1].set_value("sensi+2", value)

    @property
    def sensi_3(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi+3")

    @sensi_3.setter
    def sensi_3(self, value: int) -> None:
        self._cards[1].set_value("sensi+3", value)

    @property
    def sensi_4(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi+4")

    @sensi_4.setter
    def sensi_4(self, value: int) -> None:
        self._cards[1].set_value("sensi+4", value)

    @property
    def sensi_5(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
        """ # nopep8
        return self._cards[1].get_value("sensi+5")

    @sensi_5.setter
    def sensi_5(self, value: int) -> None:
        self._cards[1].set_value("sensi+5", value)

    @property
    def sensi_6(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi+6")

    @sensi_6.setter
    def sensi_6(self, value: int) -> None:
        self._cards[1].set_value("sensi+6", value)

    @property
    def sensi_7(self) -> typing.Optional[int]:
        """Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
        """ # nopep8
        return self._cards[1].get_value("sensi+7")

    @sensi_7.setter
    def sensi_7(self, value: int) -> None:
        self._cards[1].set_value("sensi+7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

