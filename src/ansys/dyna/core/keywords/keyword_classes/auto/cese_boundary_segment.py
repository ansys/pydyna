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

class CeseBoundarySegment(KeywordBase):
    """DYNA CESE_BOUNDARY_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_SEGMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "n2 ",
                        int,
                        10,
                        10,
                        kwargs.get("n2 ")
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
                    Field(
                        "dof ",
                        int,
                        40,
                        10,
                        kwargs.get("dof ")
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        60,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2_(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n2 ")

    @n2_.setter
    def n2_(self, value: int) -> None:
        self._cards[0].set_value("n2 ", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[0].set_value("n4", value)

    @property
    def dof_(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("dof ")

    @dof_.setter
    def dof_(self, value: int) -> None:
        self._cards[0].set_value("dof ", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.  (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

