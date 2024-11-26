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

class BoundaryThermalBulknode(KeywordBase):
    """DYNA BOUNDARY_THERMAL_BULKNODE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "THERMAL_BULKNODE"

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
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nbnseg",
                        int,
                        20,
                        10,
                        kwargs.get("nbnseg")
                    ),
                    Field(
                        "vol",
                        float,
                        30,
                        10,
                        kwargs.get("vol")
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "h",
                        float,
                        50,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "aexp",
                        float,
                        60,
                        10,
                        kwargs.get("aexp")
                    ),
                    Field(
                        "bexp",
                        float,
                        70,
                        10,
                        kwargs.get("bexp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        kwargs.get("n4")
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Bulk node number.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Bulk node part id.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nbnseg(self) -> typing.Optional[int]:
        """Get or set the Number of bulk node segments attached to this bulk node
        """ # nopep8
        return self._cards[0].get_value("nbnseg")

    @nbnseg.setter
    def nbnseg(self, value: int) -> None:
        self._cards[0].set_value("nbnseg", value)

    @property
    def vol(self) -> typing.Optional[float]:
        """Get or set the Bulk node volume..
        """ # nopep8
        return self._cards[0].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        self._cards[0].set_value("vol", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for H.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Heat transfer coefficient.
        """ # nopep8
        return self._cards[0].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[0].set_value("h", value)

    @property
    def aexp(self) -> typing.Optional[float]:
        """Get or set the A exponent.
        """ # nopep8
        return self._cards[0].get_value("aexp")

    @aexp.setter
    def aexp(self, value: float) -> None:
        self._cards[0].set_value("aexp", value)

    @property
    def bexp(self) -> typing.Optional[float]:
        """Get or set the B exponent.
        """ # nopep8
        return self._cards[0].get_value("bexp")

    @bexp.setter
    def bexp(self, value: float) -> None:
        self._cards[0].set_value("bexp", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point numbers.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[1].set_value("n4", value)

