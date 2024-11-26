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

class ControlPoreAir(KeywordBase):
    """DYNA CONTROL_PORE_AIR keyword"""

    keyword = "CONTROL"
    subkeyword = "PORE_AIR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pa_rho",
                        float,
                        0,
                        10,
                        kwargs.get("pa_rho")
                    ),
                    Field(
                        "air_p",
                        float,
                        10,
                        10,
                        kwargs.get("air_p")
                    ),
                    Field(
                        "eterm",
                        float,
                        20,
                        10,
                        kwargs.get("eterm")
                    ),
                    Field(
                        "anamsg",
                        int,
                        30,
                        10,
                        kwargs.get("anamsg", 0)
                    ),
                ],
            ),
        ]

    @property
    def pa_rho(self) -> typing.Optional[float]:
        """Get or set the Density of atmospheric air, = 1.184 kg/m3 at 25 C
        """ # nopep8
        return self._cards[0].get_value("pa_rho")

    @pa_rho.setter
    def pa_rho(self, value: float) -> None:
        self._cards[0].set_value("pa_rho", value)

    @property
    def air_p(self) -> typing.Optional[float]:
        """Get or set the Pressure of atmospheric air, = 101.325 kPa at 25 C
        """ # nopep8
        return self._cards[0].get_value("air_p")

    @air_p.setter
    def air_p(self, value: float) -> None:
        self._cards[0].set_value("air_p", value)

    @property
    def eterm(self) -> typing.Optional[float]:
        """Get or set the Event termination time, default to ENDTIME of *CONTROL_TERMINATION.
        """ # nopep8
        return self._cards[0].get_value("eterm")

    @eterm.setter
    def eterm(self, value: float) -> None:
        self._cards[0].set_value("eterm", value)

    @property
    def anamsg(self) -> int:
        """Get or set the Flag to turn off the printing of pore air analysis status message,
        including the analysis time, the node with the highest pressure change.
        EQ. 0 Status messages are printed, the default value.
        EQ. 1 Status messages are not printed
        """ # nopep8
        return self._cards[0].get_value("anamsg")

    @anamsg.setter
    def anamsg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""anamsg must be one of {0,1}""")
        self._cards[0].set_value("anamsg", value)

