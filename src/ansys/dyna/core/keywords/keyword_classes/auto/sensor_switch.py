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

class SensorSwitch(KeywordBase):
    """DYNA SENSOR_SWITCH keyword"""

    keyword = "SENSOR"
    subkeyword = "SWITCH"
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
                        "switid",
                        int,
                        0,
                        10,
                        kwargs.get("switid")
                    ),
                    Field(
                        "type",
                        str,
                        10,
                        10,
                        kwargs.get("type", "SENSOR")
                    ),
                    Field(
                        "sensid",
                        int,
                        20,
                        10,
                        kwargs.get("sensid")
                    ),
                    Field(
                        "logic",
                        str,
                        30,
                        10,
                        kwargs.get("logic", "LT")
                    ),
                    Field(
                        "value",
                        float,
                        40,
                        10,
                        kwargs.get("value")
                    ),
                    Field(
                        "filtrid",
                        int,
                        50,
                        10,
                        kwargs.get("filtrid")
                    ),
                    Field(
                        "timwin",
                        float,
                        60,
                        10,
                        kwargs.get("timwin")
                    ),
                    Field(
                        "tdelay",
                        float,
                        70,
                        10,
                        kwargs.get("tdelay")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorSwitch.option_specs[0],
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
    def switid(self) -> typing.Optional[int]:
        """Get or set the Switch ID can be referred directly by *SENSOR_CONTROL to control the status of entities like CONTACT and AIRBAG, or can be referred to by *SENSOR_SWITCH_CALC-LOGIC for logic computation.
        """ # nopep8
        return self._cards[0].get_value("switid")

    @switid.setter
    def switid(self, value: int) -> None:
        self._cards[0].set_value("switid", value)

    @property
    def type(self) -> str:
        """Get or set the Type:
        EQ.Sensor:
        EQ.Time:
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        if value not in ["SENSOR", "TIME"]:
            raise Exception("""type must be one of {"SENSOR","TIME"}""")
        self._cards[0].set_value("type", value)

    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the ID of the sensor whose value will be compared to the criterion to determine if a switch condition is met.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        self._cards[0].set_value("sensid", value)

    @property
    def logic(self) -> str:
        """Get or set the Logic:
        EQ.LT: less than
        EQ.GT: greater than
        """ # nopep8
        return self._cards[0].get_value("logic")

    @logic.setter
    def logic(self, value: str) -> None:
        if value not in ["LT", "GT"]:
            raise Exception("""logic must be one of {"LT","GT"}""")
        self._cards[0].set_value("logic", value)

    @property
    def value(self) -> typing.Optional[float]:
        """Get or set the Critical value
        """ # nopep8
        return self._cards[0].get_value("value")

    @value.setter
    def value(self, value: float) -> None:
        self._cards[0].set_value("value", value)

    @property
    def filtrid(self) -> typing.Optional[int]:
        """Get or set the Filter ID (optional).  Filters may be defined using *DEFINE_FILTER.
        """ # nopep8
        return self._cards[0].get_value("filtrid")

    @filtrid.setter
    def filtrid(self, value: int) -> None:
        self._cards[0].set_value("filtrid", value)

    @property
    def timwin(self) -> typing.Optional[float]:
        """Get or set the Trigger a status change when the value given by the sensor is less than or greater than (depending on LOGIC) the VALUE for a duration defined by TIMWIN.
        """ # nopep8
        return self._cards[0].get_value("timwin")

    @timwin.setter
    def timwin(self, value: float) -> None:
        self._cards[0].set_value("timwin", value)

    @property
    def tdelay(self) -> typing.Optional[float]:
        """Get or set the Optional time delay. The status change will not happen immediately when
        both the switch condition and TIMWIN are met. Instead the status change
        is delayed by TDELAY.
        """ # nopep8
        return self._cards[0].get_value("tdelay")

    @tdelay.setter
    def tdelay(self, value: float) -> None:
        self._cards[0].set_value("tdelay", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

