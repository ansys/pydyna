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

class PartSensor(KeywordBase):
    """DYNA PART_SENSOR keyword"""

    keyword = "PART"
    subkeyword = "SENSOR"

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
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "sida",
                        int,
                        10,
                        10,
                        kwargs.get("sida", 0)
                    ),
                    Field(
                        "active",
                        int,
                        20,
                        10,
                        kwargs.get("active", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> int:
        """Get or set the Part ID, which is controlled by sensor.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def sida(self) -> int:
        """Get or set the Sensor ID to activate or deactivate part.
        """ # nopep8
        return self._cards[0].get_value("sida")

    @sida.setter
    def sida(self, value: int) -> None:
        self._cards[0].set_value("sida", value)

    @property
    def active(self) -> int:
        """Get or set the Flag. If zero, the part is active from time zero until a signal is received by the part to deactivate. If one, the part is inactive from time zero and becomes active when a signal is received by the part to activate. The history variables for inactive parts are initialized at time zero.
        """ # nopep8
        return self._cards[0].get_value("active")

    @active.setter
    def active(self, value: int) -> None:
        self._cards[0].set_value("active", value)

