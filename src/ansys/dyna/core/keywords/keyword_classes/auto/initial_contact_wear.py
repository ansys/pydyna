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

class InitialContactWear(KeywordBase):
    """DYNA INITIAL_CONTACT_WEAR keyword"""

    keyword = "INITIAL"
    subkeyword = "CONTACT_WEAR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "wdepth",
                        float,
                        20,
                        10,
                        kwargs.get("wdepth")
                    ),
                    Field(
                        "nx",
                        float,
                        30,
                        10,
                        kwargs.get("nx")
                    ),
                    Field(
                        "ny",
                        float,
                        40,
                        10,
                        kwargs.get("ny")
                    ),
                    Field(
                        "nz",
                        float,
                        50,
                        10,
                        kwargs.get("nz")
                    ),
                    Field(
                        "iseq",
                        int,
                        60,
                        10,
                        kwargs.get("iseq")
                    ),
                    Field(
                        "ncyc",
                        int,
                        70,
                        10,
                        kwargs.get("ncyc")
                    ),
                ],
            ),
        ]

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the contact Interface ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def wdepth(self) -> typing.Optional[float]:
        """Get or set the wear depth, in units of length.
        """ # nopep8
        return self._cards[0].get_value("wdepth")

    @wdepth.setter
    def wdepth(self, value: float) -> None:
        self._cards[0].set_value("wdepth", value)

    @property
    def nx(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        self._cards[0].set_value("nz", value)

    @property
    def iseq(self) -> typing.Optional[int]:
        """Get or set the Simulation sequence number for the entire process.
        """ # nopep8
        return self._cards[0].get_value("iseq")

    @iseq.setter
    def iseq(self, value: int) -> None:
        self._cards[0].set_value("iseq", value)

    @property
    def ncyc(self) -> typing.Optional[int]:
        """Get or set the Number of process cycles this particular simulation corresponds to, a negative number means that LS-DYNA will not apply this card.
        """ # nopep8
        return self._cards[0].get_value("ncyc")

    @ncyc.setter
    def ncyc(self, value: int) -> None:
        self._cards[0].set_value("ncyc", value)

