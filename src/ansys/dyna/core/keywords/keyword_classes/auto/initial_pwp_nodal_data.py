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

class InitialPwpNodalData(KeywordBase):
    """DYNA INITIAL_PWP_NODAL_DATA keyword"""

    keyword = "INITIAL"
    subkeyword = "PWP_NODAL_DATA"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "nhisv",
                        int,
                        10,
                        10,
                        kwargs.get("nhisv", 0)
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hisv1",
                        float,
                        0,
                        16,
                        kwargs.get("hisv1")
                    ),
                    Field(
                        "hisv2",
                        float,
                        16,
                        16,
                        kwargs.get("hisv2")
                    ),
                    Field(
                        "hisv3",
                        float,
                        32,
                        16,
                        kwargs.get("hisv3")
                    ),
                    Field(
                        "hisv4",
                        float,
                        48,
                        16,
                        kwargs.get("hisv4")
                    ),
                    Field(
                        "hisv5",
                        float,
                        64,
                        16,
                        kwargs.get("hisv5")
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def nhisv(self) -> int:
        """Get or set the Number of nodal pore pressure history variables
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID with which this node is associated for purposes of evaluating pore fluid related properties
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def hisv1(self) -> typing.Optional[float]:
        """Get or set the Define NHISVhistory variables.
        """ # nopep8
        return self._cards[1].get_value("hisv1")

    @hisv1.setter
    def hisv1(self, value: float) -> None:
        self._cards[1].set_value("hisv1", value)

    @property
    def hisv2(self) -> typing.Optional[float]:
        """Get or set the Define NHISVhistory variables
        """ # nopep8
        return self._cards[1].get_value("hisv2")

    @hisv2.setter
    def hisv2(self, value: float) -> None:
        self._cards[1].set_value("hisv2", value)

    @property
    def hisv3(self) -> typing.Optional[float]:
        """Get or set the Define NHISVhistory variables
        """ # nopep8
        return self._cards[1].get_value("hisv3")

    @hisv3.setter
    def hisv3(self, value: float) -> None:
        self._cards[1].set_value("hisv3", value)

    @property
    def hisv4(self) -> typing.Optional[float]:
        """Get or set the Define NHISVhistory variables
        """ # nopep8
        return self._cards[1].get_value("hisv4")

    @hisv4.setter
    def hisv4(self, value: float) -> None:
        self._cards[1].set_value("hisv4", value)

    @property
    def hisv5(self) -> typing.Optional[float]:
        """Get or set the Define NHISVhistory variables
        """ # nopep8
        return self._cards[1].get_value("hisv5")

    @hisv5.setter
    def hisv5(self, value: float) -> None:
        self._cards[1].set_value("hisv5", value)

