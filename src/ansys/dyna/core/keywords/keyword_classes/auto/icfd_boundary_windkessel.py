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

class IcfdBoundaryWindkessel(KeywordBase):
    """DYNA ICFD_BOUNDARY_WINDKESSEL keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_WINDKESSEL"

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
                        "wtype",
                        int,
                        10,
                        10,
                        kwargs.get("wtype")
                    ),
                    Field(
                        "r1",
                        float,
                        20,
                        10,
                        kwargs.get("r1", 0.0)
                    ),
                    Field(
                        "c1",
                        float,
                        30,
                        10,
                        kwargs.get("c1", 0.0)
                    ),
                    Field(
                        "r2",
                        float,
                        40,
                        10,
                        kwargs.get("r2", 0.0)
                    ),
                    Field(
                        "l1",
                        float,
                        50,
                        10,
                        kwargs.get("l1", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def wtype(self) -> typing.Optional[int]:
        """Get or set the Circuit type (See Remarks) :
        EQ.1:	Windkessel circuit
        EQ.2:	Windkessel circuit with inverted flux
        EQ.3:	CV type circuit
        EQ.4:	CV type circuit with inverted flux.
        """ # nopep8
        return self._cards[0].get_value("wtype")

    @wtype.setter
    def wtype(self, value: int) -> None:
        self._cards[0].set_value("wtype", value)

    @property
    def r1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[0].set_value("r1", value)

    @property
    def c1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[0].set_value("c1", value)

    @property
    def r2(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[0].set_value("r2", value)

    @property
    def l1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("l1")

    @l1.setter
    def l1(self, value: float) -> None:
        self._cards[0].set_value("l1", value)

