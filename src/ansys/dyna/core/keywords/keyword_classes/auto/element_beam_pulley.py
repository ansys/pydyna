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

class ElementBeamPulley(KeywordBase):
    """DYNA ELEMENT_BEAM_PULLEY keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_PULLEY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "puid",
                        int,
                        0,
                        10,
                        kwargs.get("puid", 0)
                    ),
                    Field(
                        "bid1",
                        int,
                        10,
                        10,
                        kwargs.get("bid1", 0)
                    ),
                    Field(
                        "bid2",
                        int,
                        20,
                        10,
                        kwargs.get("bid2", 0)
                    ),
                    Field(
                        "pnid",
                        int,
                        30,
                        10,
                        kwargs.get("pnid", 0)
                    ),
                    Field(
                        "fd",
                        float,
                        40,
                        10,
                        kwargs.get("fd", 0.0)
                    ),
                    Field(
                        "fs",
                        float,
                        50,
                        10,
                        kwargs.get("fs", 0.0)
                    ),
                    Field(
                        "lmin",
                        float,
                        60,
                        10,
                        kwargs.get("lmin", 0.0)
                    ),
                    Field(
                        "dc",
                        float,
                        70,
                        10,
                        kwargs.get("dc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def puid(self) -> int:
        """Get or set the Pulley ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("puid")

    @puid.setter
    def puid(self, value: int) -> None:
        self._cards[0].set_value("puid", value)

    @property
    def bid1(self) -> int:
        """Get or set the Truss beam element 1 ID.
        """ # nopep8
        return self._cards[0].get_value("bid1")

    @bid1.setter
    def bid1(self, value: int) -> None:
        self._cards[0].set_value("bid1", value)

    @property
    def bid2(self) -> int:
        """Get or set the Truss beam element 2 ID.
        """ # nopep8
        return self._cards[0].get_value("bid2")

    @bid2.setter
    def bid2(self, value: int) -> None:
        self._cards[0].set_value("bid2", value)

    @property
    def pnid(self) -> int:
        """Get or set the Pulley node, NID.
        """ # nopep8
        return self._cards[0].get_value("pnid")

    @pnid.setter
    def pnid(self, value: int) -> None:
        self._cards[0].set_value("pnid", value)

    @property
    def fd(self) -> float:
        """Get or set the Coulomb dynamic friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[0].set_value("fd", value)

    @property
    def fs(self) -> float:
        """Get or set the Optional Coulomb static friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[0].set_value("fs", value)

    @property
    def lmin(self) -> float:
        """Get or set the Minimum length, see notes below.
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        self._cards[0].set_value("lmin", value)

    @property
    def dc(self) -> float:
        """Get or set the Optional decay constant to allow smooth transition between the static and dynamic friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[0].set_value("dc", value)

