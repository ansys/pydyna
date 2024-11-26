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

class LoadHeatGenerationShell(KeywordBase):
    """DYNA LOAD_HEAT_GENERATION_SHELL keyword"""

    keyword = "LOAD"
    subkeyword = "HEAT_GENERATION_SHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "cmult",
                        float,
                        20,
                        10,
                        kwargs.get("cmult", 1.0)
                    ),
                    Field(
                        "wblcid",
                        int,
                        30,
                        10,
                        kwargs.get("wblcid")
                    ),
                    Field(
                        "cblcid",
                        int,
                        40,
                        10,
                        kwargs.get("cblcid")
                    ),
                    Field(
                        "tblcid",
                        int,
                        50,
                        10,
                        kwargs.get("tblcid")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the SHELL element ID, *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for volumetric heat generation rate:
        GT.0: function versus time,
        EQ.0: use multiplier value CMULT only,
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def cmult(self) -> float:
        """Get or set the Curve multiplier for  heat generation rate (LCID). Depending on the definition of LCID this value is either used for scaling or for constant heat generation.
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        self._cards[0].set_value("cmult", value)

    @property
    def wblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood persusion rate as a function of time
        """ # nopep8
        return self._cards[0].get_value("wblcid")

    @wblcid.setter
    def wblcid(self, value: int) -> None:
        self._cards[0].set_value("wblcid", value)

    @property
    def cblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood heat capacity as a function of the blood temperature
        """ # nopep8
        return self._cards[0].get_value("cblcid")

    @cblcid.setter
    def cblcid(self, value: int) -> None:
        self._cards[0].set_value("cblcid", value)

    @property
    def tblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the blood temperature as a function of time
        """ # nopep8
        return self._cards[0].get_value("tblcid")

    @tblcid.setter
    def tblcid(self, value: int) -> None:
        self._cards[0].set_value("tblcid", value)

