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

class ElementBeamSource(KeywordBase):
    """DYNA ELEMENT_BEAM_SOURCE keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_SOURCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bsid",
                        int,
                        0,
                        10,
                        kwargs.get("bsid", 0)
                    ),
                    Field(
                        "bsnid",
                        int,
                        10,
                        10,
                        kwargs.get("bsnid", 0)
                    ),
                    Field(
                        "bseid",
                        int,
                        20,
                        10,
                        kwargs.get("bseid", 0)
                    ),
                    Field(
                        "nele",
                        int,
                        30,
                        10,
                        kwargs.get("nele", 0)
                    ),
                    Field(
                        "lfed",
                        float,
                        40,
                        10,
                        kwargs.get("lfed", 0.0)
                    ),
                    Field(
                        "fpull",
                        float,
                        50,
                        10,
                        kwargs.get("fpull", 0.0)
                    ),
                    Field(
                        "lmin",
                        float,
                        60,
                        10,
                        kwargs.get("lmin", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def bsid(self) -> int:
        """Get or set the Beam Source ID.  A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        self._cards[0].set_value("bsid", value)

    @property
    def bsnid(self) -> int:
        """Get or set the Source node ID.
        """ # nopep8
        return self._cards[0].get_value("bsnid")

    @bsnid.setter
    def bsnid(self, value: int) -> None:
        self._cards[0].set_value("bsnid", value)

    @property
    def bseid(self) -> int:
        """Get or set the Source element ID.
        """ # nopep8
        return self._cards[0].get_value("bseid")

    @bseid.setter
    def bseid(self, value: int) -> None:
        self._cards[0].set_value("bseid", value)

    @property
    def nele(self) -> int:
        """Get or set the Number of elements to be pulled out.
        """ # nopep8
        return self._cards[0].get_value("nele")

    @nele.setter
    def nele(self, value: int) -> None:
        self._cards[0].set_value("nele", value)

    @property
    def lfed(self) -> float:
        """Get or set the Beam element fed length.
        """ # nopep8
        return self._cards[0].get_value("lfed")

    @lfed.setter
    def lfed(self, value: float) -> None:
        self._cards[0].set_value("lfed", value)

    @property
    def fpull(self) -> float:
        """Get or set the Pull-out force.
        """ # nopep8
        return self._cards[0].get_value("fpull")

    @fpull.setter
    def fpull(self, value: float) -> None:
        self._cards[0].set_value("fpull", value)

    @property
    def lmin(self) -> float:
        """Get or set the Minimum beam element length, see notes below.
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        self._cards[0].set_value("lmin", value)

