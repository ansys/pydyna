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

class IcfdDatabaseDrag(KeywordBase):
    """DYNA ICFD_DATABASE_DRAG keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_DRAG"

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
                        "cpid",
                        int,
                        10,
                        10,
                        kwargs.get("cpid")
                    ),
                    Field(
                        "dtout",
                        float,
                        20,
                        10,
                        kwargs.get("dtout", 0.0)
                    ),
                    Field(
                        "perout",
                        int,
                        30,
                        10,
                        kwargs.get("perout", 0)
                    ),
                    Field(
                        "divi",
                        int,
                        40,
                        10,
                        kwargs.get("divi", 10)
                    ),
                    Field(
                        "elout",
                        int,
                        50,
                        10,
                        kwargs.get("elout", 0)
                    ),
                    Field(
                        "ssout",
                        int,
                        60,
                        10,
                        kwargs.get("ssout", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the surface where the drag force will be computed
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def cpid(self) -> typing.Optional[int]:
        """Get or set the Center point ID used for the calculation of the force's moment. By default the reference frame center is used (Coordinates (0,0,0)).
        """ # nopep8
        return self._cards[0].get_value("cpid")

    @cpid.setter
    def cpid(self, value: int) -> None:
        self._cards[0].set_value("cpid", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def perout(self) -> int:
        """Get or set the Outputs the contribution of the different elements on the total drag in fractions of the total drag in the d3plots.
        """ # nopep8
        return self._cards[0].get_value("perout")

    @perout.setter
    def perout(self, value: int) -> None:
        self._cards[0].set_value("perout", value)

    @property
    def divi(self) -> int:
        """Get or set the Number of drag divisions for PEROUT. Default is 10 which means the contributions will be grouped in 10 deciles.
        """ # nopep8
        return self._cards[0].get_value("divi")

    @divi.setter
    def divi(self, value: int) -> None:
        self._cards[0].set_value("divi", value)

    @property
    def elout(self) -> int:
        """Get or set the Outputs the drag value of each element in the d3plots.
        """ # nopep8
        return self._cards[0].get_value("elout")

    @elout.setter
    def elout(self, value: int) -> None:
        self._cards[0].set_value("elout", value)

    @property
    def ssout(self) -> int:
        """Get or set the Outputs the pressure loads caused by the fluid on each solid segment set in keyword format. FSI needs to be activated.
        """ # nopep8
        return self._cards[0].get_value("ssout")

    @ssout.setter
    def ssout(self, value: int) -> None:
        self._cards[0].set_value("ssout", value)

