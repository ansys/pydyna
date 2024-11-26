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

class LoadHeatController(KeywordBase):
    """DYNA LOAD_HEAT_CONTROLLER keyword"""

    keyword = "LOAD"
    subkeyword = "HEAT_CONTROLLER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "node",
                        int,
                        0,
                        10,
                        kwargs.get("node")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "load",
                        float,
                        20,
                        10,
                        kwargs.get("load")
                    ),
                    Field(
                        "tset",
                        float,
                        30,
                        10,
                        kwargs.get("tset")
                    ),
                    Field(
                        "type",
                        int,
                        40,
                        10,
                        kwargs.get("type")
                    ),
                    Field(
                        "gp",
                        float,
                        50,
                        10,
                        kwargs.get("gp")
                    ),
                    Field(
                        "gi",
                        float,
                        60,
                        10,
                        kwargs.get("gi")
                    ),
                ],
            ),
        ]

    @property
    def node(self) -> typing.Optional[int]:
        """Get or set the Sensor is located at this node number.
        """ # nopep8
        return self._cards[0].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        self._cards[0].set_value("node", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID assigned to the elements modeling the heater or cooler being controlled
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def load(self) -> typing.Optional[float]:
        """Get or set the Heater output (q0) [typical units W/m3]
        """ # nopep8
        return self._cards[0].get_value("load")

    @load.setter
    def load(self, value: float) -> None:
        self._cards[0].set_value("load", value)

    @property
    def tset(self) -> typing.Optional[float]:
        """Get or set the Controller set point temperature at location identified by NODE
        """ # nopep8
        return self._cards[0].get_value("tset")

    @tset.setter
    def tset(self, value: float) -> None:
        self._cards[0].set_value("tset", value)

    @property
    def type(self) -> typing.Optional[int]:
        """Get or set the Type of control function:
        EQ.1: on-off
        EQ.2: proportional + integral
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        self._cards[0].set_value("type", value)

    @property
    def gp(self) -> typing.Optional[float]:
        """Get or set the Proportional gain
        """ # nopep8
        return self._cards[0].get_value("gp")

    @gp.setter
    def gp(self, value: float) -> None:
        self._cards[0].set_value("gp", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Integral gain
        """ # nopep8
        return self._cards[0].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[0].set_value("gi", value)

