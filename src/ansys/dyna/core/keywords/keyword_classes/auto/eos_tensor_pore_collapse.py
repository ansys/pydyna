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

class EosTensorPoreCollapse(KeywordBase):
    """DYNA EOS_TENSOR_PORE_COLLAPSE keyword"""

    keyword = "EOS"
    subkeyword = "TENSOR_PORE_COLLAPSE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "nld",
                        int,
                        10,
                        10,
                        kwargs.get("nld")
                    ),
                    Field(
                        "ncr",
                        int,
                        20,
                        10,
                        kwargs.get("ncr")
                    ),
                    Field(
                        "mu1",
                        float,
                        30,
                        10,
                        kwargs.get("mu1", 0.0)
                    ),
                    Field(
                        "mu2",
                        float,
                        40,
                        10,
                        kwargs.get("mu2", 0.0)
                    ),
                    Field(
                        "ie0",
                        float,
                        50,
                        10,
                        kwargs.get("ie0", 0.0)
                    ),
                    Field(
                        "ec0",
                        float,
                        60,
                        10,
                        kwargs.get("ec0", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def nld(self) -> typing.Optional[int]:
        """Get or set the Virgin loading load curve ID.
        """ # nopep8
        return self._cards[0].get_value("nld")

    @nld.setter
    def nld(self, value: int) -> None:
        self._cards[0].set_value("nld", value)

    @property
    def ncr(self) -> typing.Optional[int]:
        """Get or set the Completely crushed load curve ID.
        """ # nopep8
        return self._cards[0].get_value("ncr")

    @ncr.setter
    def ncr(self, value: int) -> None:
        self._cards[0].set_value("ncr", value)

    @property
    def mu1(self) -> float:
        """Get or set the Excess compression required before any pores can collapse.
        """ # nopep8
        return self._cards[0].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        self._cards[0].set_value("mu1", value)

    @property
    def mu2(self) -> float:
        """Get or set the Excess compression point where the virgin loading curve and the completely crushed curve intersect.
        """ # nopep8
        return self._cards[0].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        self._cards[0].set_value("mu2", value)

    @property
    def ie0(self) -> float:
        """Get or set the Initial internal Energy.
        """ # nopep8
        return self._cards[0].get_value("ie0")

    @ie0.setter
    def ie0(self, value: float) -> None:
        self._cards[0].set_value("ie0", value)

    @property
    def ec0(self) -> float:
        """Get or set the Initial excess Compression.
        """ # nopep8
        return self._cards[0].get_value("ec0")

    @ec0.setter
    def ec0(self, value: float) -> None:
        self._cards[0].set_value("ec0", value)

