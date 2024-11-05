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

class DampingFrequencyRange(KeywordBase):
    """DYNA DAMPING_FREQUENCY_RANGE keyword"""

    keyword = "DAMPING"
    subkeyword = "FREQUENCY_RANGE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cdamp",
                        float,
                        0,
                        10,
                        kwargs.get("cdamp", 0.0)
                    ),
                    Field(
                        "flow",
                        float,
                        10,
                        10,
                        kwargs.get("flow", 0.0)
                    ),
                    Field(
                        "fhigh",
                        float,
                        20,
                        10,
                        kwargs.get("fhigh", 0.0)
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "blank",
                        int,
                        40,
                        10,
                        kwargs.get("blank", 0)
                    ),
                    Field(
                        "pidrel",
                        int,
                        50,
                        10,
                        kwargs.get("pidrel", 0)
                    ),
                    Field(
                        "iflg",
                        int,
                        60,
                        10,
                        kwargs.get("iflg", 0)
                    ),
                ],
            ),
        ]

    @property
    def cdamp(self) -> float:
        """Get or set the Damping in fraction of critical.  Accurate application of this damping depends on the time step being small compared to the period of interest.
        """ # nopep8
        return self._cards[0].get_value("cdamp")

    @cdamp.setter
    def cdamp(self, value: float) -> None:
        self._cards[0].set_value("cdamp", value)

    @property
    def flow(self) -> float:
        """Get or set the Lowest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
        """ # nopep8
        return self._cards[0].get_value("flow")

    @flow.setter
    def flow(self, value: float) -> None:
        self._cards[0].set_value("flow", value)

    @property
    def fhigh(self) -> float:
        """Get or set the Highest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
        """ # nopep8
        return self._cards[0].get_value("fhigh")

    @fhigh.setter
    def fhigh(self, value: float) -> None:
        self._cards[0].set_value("fhigh", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID. The requested damping is applied only to the parts in the set. If PSID = 0, the damping is applied to all parts except those referred to by other *DAMPING_FREQUENCY_RANGE cards.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def blank(self) -> int:
        """Get or set the Damping in fraction of critical.
        """ # nopep8
        return self._cards[0].get_value("blank")

    @blank.setter
    def blank(self, value: int) -> None:
        self._cards[0].set_value("blank", value)

    @property
    def pidrel(self) -> int:
        """Get or set the Optional part ID of rigid body. Damping is then applied to the motion relative to the rigid body motion.  This input does not apply to the DEFORM option.
        """ # nopep8
        return self._cards[0].get_value("pidrel")

    @pidrel.setter
    def pidrel(self, value: int) -> None:
        self._cards[0].set_value("pidrel", value)

    @property
    def iflg(self) -> int:
        """Get or set the Method used for internal calculation of damping constants:
        EQ.0:	iterative(more accurate)
        EQ.1 : approximate(same as R9 and previous versions)
        """ # nopep8
        return self._cards[0].get_value("iflg")

    @iflg.setter
    def iflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iflg must be one of {0,1}""")
        self._cards[0].set_value("iflg", value)

