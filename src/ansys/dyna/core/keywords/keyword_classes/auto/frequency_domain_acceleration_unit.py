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

class FrequencyDomainAccelerationUnit(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACCELERATION_UNIT keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACCELERATION_UNIT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "unit",
                        int,
                        0,
                        10,
                        kwargs.get("unit", 0)
                    ),
                    Field(
                        "umlt",
                        float,
                        10,
                        10,
                        kwargs.get("umlt")
                    ),
                ],
            ),
        ]

    @property
    def unit(self) -> int:
        """Get or set the Flag for acceleration unit conversion:
        EQ.0: use [length unit]/[time unit]2 as unit of acceleration.
        EQ.1: use g as unit for acceleration, and SI units (Newton, kg, meter, second, etc.) elsewhere.
        EQ.2: use g as unit for acceleration, and Engineering units (lbf,lbf*second2/inch, inch, second, etc.) elsewhere.
        EQ.3: use g as unit for acceleration, and units (kN, kg, mm, ms, GPa, etc.) elsewhere.
        EQ4:use g as unit for acceleration, and units (Newton, ton, mm, second, MPa, etc.) elsewhere.
        EQ.-1: use g as unit for acceleration and provide the multiplier for converting g to [length unit]/[time unit]2.
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, -1]:
            raise Exception("""unit must be one of {0,1,2,3,4,-1}""")
        self._cards[0].set_value("unit", value)

    @property
    def umlt(self) -> typing.Optional[float]:
        """Get or set the Multiplier for converting g to [length unit]/[time unit]2 (used only for UNIT=-1).
        """ # nopep8
        return self._cards[0].get_value("umlt")

    @umlt.setter
    def umlt(self, value: float) -> None:
        self._cards[0].set_value("umlt", value)

