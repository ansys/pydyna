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

class FrequencyDomainSeaInput(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_SEA_INPUT keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_SEA_INPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "subid",
                        int,
                        0,
                        10,
                        kwargs.get("subid")
                    ),
                    Field(
                        "subtyp",
                        int,
                        10,
                        10,
                        kwargs.get("subtyp", 1)
                    ),
                    Field(
                        "loadtyp",
                        int,
                        20,
                        10,
                        kwargs.get("loadtyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bwave",
                        float,
                        0,
                        10,
                        kwargs.get("bwave")
                    ),
                    Field(
                        "lwave",
                        float,
                        10,
                        10,
                        kwargs.get("lwave")
                    ),
                    Field(
                        "swave",
                        float,
                        20,
                        10,
                        kwargs.get("swave")
                    ),
                    Field(
                        "twave",
                        float,
                        30,
                        10,
                        kwargs.get("twave")
                    ),
                ],
            ),
        ]

    @property
    def subid(self) -> typing.Optional[int]:
        """Get or set the Subsystem ID.
        """ # nopep8
        return self._cards[0].get_value("subid")

    @subid.setter
    def subid(self, value: int) -> None:
        self._cards[0].set_value("subid", value)

    @property
    def subtyp(self) -> int:
        """Get or set the Subsystem type
        EQ.1: plate
        EQ.2: cavity
        EQ.3: beam.
        """ # nopep8
        return self._cards[0].get_value("subtyp")

    @subtyp.setter
    def subtyp(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""subtyp must be one of {1,2,3}""")
        self._cards[0].set_value("subtyp", value)

    @property
    def loadtyp(self) -> int:
        """Get or set the Input power type:
        EQ.0: power
        EQ.1: force
        EQ.2: velocity
        EQ.3: pressure.
        EQ.4:	bending wave power for plate
        EQ.5:	shear wave power for plate
        """ # nopep8
        return self._cards[0].get_value("loadtyp")

    @loadtyp.setter
    def loadtyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""loadtyp must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("loadtyp", value)

    @property
    def bwave(self) -> typing.Optional[float]:
        """Get or set the Input power value for bending wave.
        """ # nopep8
        return self._cards[1].get_value("bwave")

    @bwave.setter
    def bwave(self, value: float) -> None:
        self._cards[1].set_value("bwave", value)

    @property
    def lwave(self) -> typing.Optional[float]:
        """Get or set the Input power value for longitudinal wave.
        """ # nopep8
        return self._cards[1].get_value("lwave")

    @lwave.setter
    def lwave(self, value: float) -> None:
        self._cards[1].set_value("lwave", value)

    @property
    def swave(self) -> typing.Optional[float]:
        """Get or set the Input power value for shear wave.
        """ # nopep8
        return self._cards[1].get_value("swave")

    @swave.setter
    def swave(self, value: float) -> None:
        self._cards[1].set_value("swave", value)

    @property
    def twave(self) -> typing.Optional[float]:
        """Get or set the Input power value for shear wave.
        """ # nopep8
        return self._cards[1].get_value("twave")

    @twave.setter
    def twave(self, value: float) -> None:
        self._cards[1].set_value("twave", value)

