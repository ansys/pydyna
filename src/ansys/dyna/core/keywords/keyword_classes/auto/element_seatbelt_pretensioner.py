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

class ElementSeatbeltPretensioner(KeywordBase):
    """DYNA ELEMENT_SEATBELT_PRETENSIONER keyword"""

    keyword = "ELEMENT"
    subkeyword = "SEATBELT_PRETENSIONER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sbprid",
                        int,
                        0,
                        10,
                        kwargs.get("sbprid", 0)
                    ),
                    Field(
                        "sbprty",
                        int,
                        10,
                        10,
                        kwargs.get("sbprty", 1)
                    ),
                    Field(
                        "sbsid1",
                        int,
                        20,
                        10,
                        kwargs.get("sbsid1", 0)
                    ),
                    Field(
                        "sbsid2",
                        int,
                        30,
                        10,
                        kwargs.get("sbsid2", 0)
                    ),
                    Field(
                        "sbsid3",
                        int,
                        40,
                        10,
                        kwargs.get("sbsid3", 0)
                    ),
                    Field(
                        "sbsid4",
                        int,
                        50,
                        10,
                        kwargs.get("sbsid4", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sbrid",
                        int,
                        0,
                        10,
                        kwargs.get("sbrid", 0)
                    ),
                    Field(
                        "time",
                        float,
                        10,
                        10,
                        kwargs.get("time", 0.0)
                    ),
                    Field(
                        "ptlcid",
                        int,
                        20,
                        10,
                        kwargs.get("ptlcid", 0)
                    ),
                    Field(
                        "lmtfrc",
                        float,
                        30,
                        10,
                        kwargs.get("lmtfrc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def sbprid(self) -> int:
        """Get or set the Pretensioner ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("sbprid")

    @sbprid.setter
    def sbprid(self, value: int) -> None:
        self._cards[0].set_value("sbprid", value)

    @property
    def sbprty(self) -> int:
        """Get or set the Pretensioner type (see Activation):
        EQ.1:	Pyrotechnic retractor with force limits(see Type 1),
        EQ.2 : Pre - loaded spring becomes active(see Types 2 and 3),
        EQ.3 : Lock spring removed(see Types 2 and 3),
        EQ.4 : Force as a function of time retractor with optional force limiter, LMTFRC(see Type 4)
        EQ.5 : Pyrotechnic retractor(old type in version 950) but with optional force limiter, LMTFRC(see Type 1 and Type 5).
        EQ.6 : Combination of types 4 and 5 as described in Type 6 below.
        EQ.7 : Independent pretensioner / retractor with optional force limiter(see Type 7).
        EQ.8 : Energy as a function of time retractor pretensioner with optional force limiter, LMTFRC(see Type 8).
        EQ.9 : Energy as a function of time buckle or anchor pretensioner(see Type 9).
        """ # nopep8
        return self._cards[0].get_value("sbprty")

    @sbprty.setter
    def sbprty(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""sbprty must be one of {1,2,3,4,5,6,7,8,9}""")
        self._cards[0].set_value("sbprty", value)

    @property
    def sbsid1(self) -> int:
        """Get or set the Sensor 1, see *ELEMENT_SEATBELT_SENSOR.
        """ # nopep8
        return self._cards[0].get_value("sbsid1")

    @sbsid1.setter
    def sbsid1(self, value: int) -> None:
        self._cards[0].set_value("sbsid1", value)

    @property
    def sbsid2(self) -> int:
        """Get or set the Sensor 2, see *ELEMENT_SEATBELT_SENSOR.
        """ # nopep8
        return self._cards[0].get_value("sbsid2")

    @sbsid2.setter
    def sbsid2(self, value: int) -> None:
        self._cards[0].set_value("sbsid2", value)

    @property
    def sbsid3(self) -> int:
        """Get or set the Sensor 3, see *ELEMENT_SEATBELT_SENSOR.
        """ # nopep8
        return self._cards[0].get_value("sbsid3")

    @sbsid3.setter
    def sbsid3(self, value: int) -> None:
        self._cards[0].set_value("sbsid3", value)

    @property
    def sbsid4(self) -> int:
        """Get or set the Sensor 4, see *ELEMENT_SEATBELT_SENSOR.
        """ # nopep8
        return self._cards[0].get_value("sbsid4")

    @sbsid4.setter
    def sbsid4(self, value: int) -> None:
        self._cards[0].set_value("sbsid4", value)

    @property
    def sbrid(self) -> int:
        """Get or set the Retractor number (SBPRTY = 1, 4, 5, 6, 7 or 8) or spring element number (SBPRTY = 2, 3 or 9).
        """ # nopep8
        return self._cards[1].get_value("sbrid")

    @sbrid.setter
    def sbrid(self, value: int) -> None:
        self._cards[1].set_value("sbrid", value)

    @property
    def time(self) -> float:
        """Get or set the Time between sensor triggering and pretensioner acting.
        """ # nopep8
        return self._cards[1].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        self._cards[1].set_value("time", value)

    @property
    def ptlcid(self) -> int:
        """Get or set the Load curve for pretensioner (Time after activation, Pull-in) (SBPRTY = 1, 4, 5, 6, 7, 8 or 9).
        """ # nopep8
        return self._cards[1].get_value("ptlcid")

    @ptlcid.setter
    def ptlcid(self, value: int) -> None:
        self._cards[1].set_value("ptlcid", value)

    @property
    def lmtfrc(self) -> float:
        """Get or set the Optional limiting force for retractor types 4 through 8.  If zero, this option is ignored.
        """ # nopep8
        return self._cards[1].get_value("lmtfrc")

    @lmtfrc.setter
    def lmtfrc(self, value: float) -> None:
        self._cards[1].set_value("lmtfrc", value)

