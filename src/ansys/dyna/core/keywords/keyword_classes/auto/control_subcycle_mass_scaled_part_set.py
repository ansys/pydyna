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

class ControlSubcycleMassScaledPartSet(KeywordBase):
    """DYNA CONTROL_SUBCYCLE_MASS_SCALED_PART_SET keyword"""

    keyword = "CONTROL"
    subkeyword = "SUBCYCLE_MASS_SCALED_PART_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "ts",
                        float,
                        10,
                        10,
                        kwargs.get("ts")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def ts(self) -> typing.Optional[float]:
        """Get or set the Time step size at which mass scaling is invoked for the PSID
        """ # nopep8
        return self._cards[0].get_value("ts")

    @ts.setter
    def ts(self, value: float) -> None:
        self._cards[0].set_value("ts", value)

