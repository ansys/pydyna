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

class ControlAccuracy(KeywordBase):
    """DYNA CONTROL_ACCURACY keyword"""

    keyword = "CONTROL"
    subkeyword = "ACCURACY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "osu",
                        int,
                        0,
                        10,
                        kwargs.get("osu", 0)
                    ),
                    Field(
                        "inn",
                        int,
                        10,
                        10,
                        kwargs.get("inn", 1)
                    ),
                    Field(
                        "pidosu",
                        int,
                        20,
                        10,
                        kwargs.get("pidosu")
                    ),
                    Field(
                        "iacc",
                        int,
                        30,
                        10,
                        kwargs.get("iacc")
                    ),
                    Field(
                        "exacc",
                        float,
                        40,
                        10,
                        kwargs.get("exacc")
                    ),
                ],
            ),
        ]

    @property
    def osu(self) -> int:
        """Get or set the Global flag for 2nd order objective stress update:
        EQ.0: off (default)
        EQ.1: on
        """ # nopep8
        return self._cards[0].get_value("osu")

    @osu.setter
    def osu(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""osu must be one of {0,1}""")
        self._cards[0].set_value("osu", value)

    @property
    def inn(self) -> int:
        """Get or set the Invariant node numbering for shell and solid elements:
        EQ.1: off (default for explicit)
        EQ.2: on for shell and thick shell elements(default for implicit)
        EQ.3: On for solid elements
        EQ.4: On for shell, thick shell and solid elements
        EQ.-2:On for shell elements except triangular shells
        EQ.-4:On for both shell and solid elements except triangular shells
        """ # nopep8
        return self._cards[0].get_value("inn")

    @inn.setter
    def inn(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -2, -4]:
            raise Exception("""inn must be one of {1,2,3,4,-2,-4}""")
        self._cards[0].set_value("inn", value)

    @property
    def pidosu(self) -> typing.Optional[int]:
        """Get or set the Part set ID for objective stress updates. If this part set ID is given only those part IDs listed will use the objective stress update; therefore, OSU is ignored.
        """ # nopep8
        return self._cards[0].get_value("pidosu")

    @pidosu.setter
    def pidosu(self, value: int) -> None:
        self._cards[0].set_value("pidosu", value)

    @property
    def iacc(self) -> typing.Optional[int]:
        """Get or set the Implicit accuracy flag, turns on some specific accuracy considerations in implicit analysis at an extra CPU cost.
        EQ.0: Off (default)
        EQ.1: On
        EQ.2:	on (partially also for explicit, for compatibility when switching between implicit and explicit)
        """ # nopep8
        return self._cards[0].get_value("iacc")

    @iacc.setter
    def iacc(self, value: int) -> None:
        self._cards[0].set_value("iacc", value)

    @property
    def exacc(self) -> typing.Optional[float]:
        """Get or set the Explicit accuracy parameter:
        EQ.0.0:	Off(default)
        GT.0.0 : On(see Remark 5)
        """ # nopep8
        return self._cards[0].get_value("exacc")

    @exacc.setter
    def exacc(self, value: float) -> None:
        self._cards[0].set_value("exacc", value)

