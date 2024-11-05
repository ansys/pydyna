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

class BoundaryElementMethodNeighbor(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_NEIGHBOR keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_NEIGHBOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nelem",
                        int,
                        0,
                        10,
                        kwargs.get("nelem")
                    ),
                    Field(
                        "nabor1",
                        int,
                        10,
                        10,
                        kwargs.get("nabor1")
                    ),
                    Field(
                        "nabor2",
                        int,
                        20,
                        10,
                        kwargs.get("nabor2")
                    ),
                    Field(
                        "nabor3",
                        int,
                        30,
                        10,
                        kwargs.get("nabor3")
                    ),
                    Field(
                        "nabor4",
                        int,
                        40,
                        10,
                        kwargs.get("nabor4")
                    ),
                ],
            ),
        ]

    @property
    def nelem(self) -> typing.Optional[int]:
        """Get or set the Element number.
        """ # nopep8
        return self._cards[0].get_value("nelem")

    @nelem.setter
    def nelem(self, value: int) -> None:
        self._cards[0].set_value("nelem", value)

    @property
    def nabor1(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 1 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor1")

    @nabor1.setter
    def nabor1(self, value: int) -> None:
        self._cards[0].set_value("nabor1", value)

    @property
    def nabor2(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 2 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor2")

    @nabor2.setter
    def nabor2(self, value: int) -> None:
        self._cards[0].set_value("nabor2", value)

    @property
    def nabor3(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 3 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor3")

    @nabor3.setter
    def nabor3(self, value: int) -> None:
        self._cards[0].set_value("nabor3", value)

    @property
    def nabor4(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 4 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor4")

    @nabor4.setter
    def nabor4(self, value: int) -> None:
        self._cards[0].set_value("nabor4", value)

