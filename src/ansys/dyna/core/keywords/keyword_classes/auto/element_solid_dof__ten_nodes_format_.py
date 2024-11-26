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

class ElementSolidDofTenNodesFormat(KeywordBase):
    """DYNA ELEMENT_SOLID_DOF (ten nodes format) keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_DOF (ten nodes format)"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        8,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        8,
                        8,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        16,
                        8,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        24,
                        8,
                        kwargs.get("n4")
                    ),
                    Field(
                        "n5",
                        int,
                        32,
                        8,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        40,
                        8,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        int,
                        48,
                        8,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        int,
                        56,
                        8,
                        kwargs.get("n8")
                    ),
                    Field(
                        "n9",
                        int,
                        64,
                        8,
                        kwargs.get("n9")
                    ),
                    Field(
                        "n10",
                        int,
                        72,
                        8,
                        kwargs.get("n10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        8,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        8,
                        8,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ns1",
                        int,
                        16,
                        8,
                        kwargs.get("ns1")
                    ),
                    Field(
                        "ns2",
                        int,
                        24,
                        8,
                        kwargs.get("ns2")
                    ),
                    Field(
                        "ns3",
                        int,
                        32,
                        8,
                        kwargs.get("ns3")
                    ),
                    Field(
                        "ns4",
                        int,
                        40,
                        8,
                        kwargs.get("ns4")
                    ),
                    Field(
                        "ns5",
                        int,
                        48,
                        8,
                        kwargs.get("ns5")
                    ),
                    Field(
                        "ns6",
                        int,
                        56,
                        8,
                        kwargs.get("ns6")
                    ),
                    Field(
                        "ns7",
                        int,
                        64,
                        8,
                        kwargs.get("ns7")
                    ),
                    Field(
                        "ns8",
                        int,
                        72,
                        8,
                        kwargs.get("ns8")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
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
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point 5.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point 6.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point 7.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point 8.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[1].set_value("n8", value)

    @property
    def n9(self) -> typing.Optional[int]:
        """Get or set the Nodal point 9.
        """ # nopep8
        return self._cards[1].get_value("n9")

    @n9.setter
    def n9(self, value: int) -> None:
        self._cards[1].set_value("n9", value)

    @property
    def n10(self) -> typing.Optional[int]:
        """Get or set the Nodal point 10.
        """ # nopep8
        return self._cards[1].get_value("n10")

    @n10.setter
    def n10(self, value: int) -> None:
        self._cards[1].set_value("n10", value)

    @property
    def ns1(self) -> typing.Optional[int]:
        """Get or set the Scalar node 1.
        """ # nopep8
        return self._cards[2].get_value("ns1")

    @ns1.setter
    def ns1(self, value: int) -> None:
        self._cards[2].set_value("ns1", value)

    @property
    def ns2(self) -> typing.Optional[int]:
        """Get or set the Scalar node 2.
        """ # nopep8
        return self._cards[2].get_value("ns2")

    @ns2.setter
    def ns2(self, value: int) -> None:
        self._cards[2].set_value("ns2", value)

    @property
    def ns3(self) -> typing.Optional[int]:
        """Get or set the Scalar node 3.
        """ # nopep8
        return self._cards[2].get_value("ns3")

    @ns3.setter
    def ns3(self, value: int) -> None:
        self._cards[2].set_value("ns3", value)

    @property
    def ns4(self) -> typing.Optional[int]:
        """Get or set the Scalar node 4.
        """ # nopep8
        return self._cards[2].get_value("ns4")

    @ns4.setter
    def ns4(self, value: int) -> None:
        self._cards[2].set_value("ns4", value)

    @property
    def ns5(self) -> typing.Optional[int]:
        """Get or set the Scalar node 5.
        """ # nopep8
        return self._cards[2].get_value("ns5")

    @ns5.setter
    def ns5(self, value: int) -> None:
        self._cards[2].set_value("ns5", value)

    @property
    def ns6(self) -> typing.Optional[int]:
        """Get or set the Scalar node 6.
        """ # nopep8
        return self._cards[2].get_value("ns6")

    @ns6.setter
    def ns6(self, value: int) -> None:
        self._cards[2].set_value("ns6", value)

    @property
    def ns7(self) -> typing.Optional[int]:
        """Get or set the Scalar node 7.
        """ # nopep8
        return self._cards[2].get_value("ns7")

    @ns7.setter
    def ns7(self, value: int) -> None:
        self._cards[2].set_value("ns7", value)

    @property
    def ns8(self) -> typing.Optional[int]:
        """Get or set the Scalar node 8.
        """ # nopep8
        return self._cards[2].get_value("ns8")

    @ns8.setter
    def ns8(self, value: int) -> None:
        self._cards[2].set_value("ns8", value)

