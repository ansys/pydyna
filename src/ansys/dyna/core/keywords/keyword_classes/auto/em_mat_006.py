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

class EmMat006(KeywordBase):
    """DYNA EM_MAT_006 keyword"""

    keyword = "EM"
    subkeyword = "MAT_006"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "mtype",
                        int,
                        10,
                        10,
                        kwargs.get("mtype", 0)
                    ),
                    Field(
                        "sigp",
                        float,
                        20,
                        10,
                        kwargs.get("sigp")
                    ),
                    Field(
                        "eosp",
                        int,
                        30,
                        10,
                        kwargs.get("eosp")
                    ),
                    Field(
                        "sign",
                        float,
                        40,
                        10,
                        kwargs.get("sign")
                    ),
                    Field(
                        "eosn",
                        int,
                        50,
                        10,
                        kwargs.get("eosn")
                    ),
                    Field(
                        "deatht",
                        float,
                        60,
                        10,
                        kwargs.get("deatht", 1.0E28)
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Defines the electromagnetism type of the material:
        EQ.0:Air or vacuum
        EQ.1: Insulator material:these materials have the same electromagnetism behavior as MTYPE = 0
        EQ.5:Material associated to *EM_RANDLES_BATMAC.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        if value not in [0, 1, 5]:
            raise Exception("""mtype must be one of {0,1,5}""")
        self._cards[0].set_value("mtype", value)

    @property
    def sigp(self) -> typing.Optional[float]:
        """Get or set the Conductivities of the Positive current collector materials.
        """ # nopep8
        return self._cards[0].get_value("sigp")

    @sigp.setter
    def sigp(self, value: float) -> None:
        self._cards[0].set_value("sigp", value)

    @property
    def eosp(self) -> typing.Optional[int]:
        """Get or set the Optional ID of the EOS to be used for the two conductivities.
        """ # nopep8
        return self._cards[0].get_value("eosp")

    @eosp.setter
    def eosp(self, value: int) -> None:
        self._cards[0].set_value("eosp", value)

    @property
    def sign(self) -> typing.Optional[float]:
        """Get or set the Conductivities of the Negative current collector materials.
        """ # nopep8
        return self._cards[0].get_value("sign")

    @sign.setter
    def sign(self, value: float) -> None:
        self._cards[0].set_value("sign", value)

    @property
    def eosn(self) -> typing.Optional[int]:
        """Get or set the Optional ID of the EOS to be used for the two conductivities.
        """ # nopep8
        return self._cards[0].get_value("eosn")

    @eosn.setter
    def eosn(self, value: int) -> None:
        self._cards[0].set_value("eosn", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and removed from the EM solve
        """ # nopep8
        return self._cards[0].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        self._cards[0].set_value("deatht", value)

