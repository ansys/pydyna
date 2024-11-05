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

class ElementGeneralizedSolid(KeywordBase):
    """DYNA ELEMENT_GENERALIZED_SOLID keyword"""

    keyword = "ELEMENT"
    subkeyword = "GENERALIZED_SOLID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nmnp",
                        int,
                        20,
                        10,
                        kwargs.get("nmnp")
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
                    Field(
                        "n5",
                        int,
                        40,
                        10,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        50,
                        10,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        int,
                        60,
                        10,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        int,
                        70,
                        10,
                        kwargs.get("n8")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. Chose a unique number with respect to other elements
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nmnp(self) -> typing.Optional[int]:
        """Get or set the Number of nodes to define this element.
        """ # nopep8
        return self._cards[0].get_value("nmnp")

    @nmnp.setter
    def nmnp(self, value: int) -> None:
        self._cards[0].set_value("nmnp", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[1].set_value("n8", value)

