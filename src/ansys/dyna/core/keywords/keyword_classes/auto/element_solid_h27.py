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

"""Module providing the ElementSolidH27 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ElementSolidH27(KeywordBase):
    """DYNA ELEMENT_SOLID_H27 keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_H27"

    def __init__(self, **kwargs):
        """Initialize the ElementSolidH27 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        8,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n3",
                        int,
                        16,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n4",
                        int,
                        24,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n5",
                        int,
                        32,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n6",
                        int,
                        40,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n7",
                        int,
                        48,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n8",
                        int,
                        56,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n9",
                        int,
                        64,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n10",
                        int,
                        72,
                        8,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n11",
                        int,
                        0,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n12",
                        int,
                        8,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n13",
                        int,
                        16,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n14",
                        int,
                        24,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n15",
                        int,
                        32,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n16",
                        int,
                        40,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n17",
                        int,
                        48,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n18",
                        int,
                        56,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n19",
                        int,
                        64,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n20",
                        int,
                        72,
                        8,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n21",
                        int,
                        0,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n22",
                        int,
                        8,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n23",
                        int,
                        16,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n24",
                        int,
                        24,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n25",
                        int,
                        32,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n26",
                        int,
                        40,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "n27",
                        int,
                        48,
                        8,
                        **kwargs,
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
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point 5.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point 6.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point 7.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point 8.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

    @property
    def n9(self) -> typing.Optional[int]:
        """Get or set the Nodal point 9.
        """ # nopep8
        return self._cards[1].get_value("n9")

    @n9.setter
    def n9(self, value: int) -> None:
        """Set the n9 property."""
        self._cards[1].set_value("n9", value)

    @property
    def n10(self) -> typing.Optional[int]:
        """Get or set the Nodal point 10.
        """ # nopep8
        return self._cards[1].get_value("n10")

    @n10.setter
    def n10(self, value: int) -> None:
        """Set the n10 property."""
        self._cards[1].set_value("n10", value)

    @property
    def n11(self) -> typing.Optional[int]:
        """Get or set the Nodal point 11.
        """ # nopep8
        return self._cards[2].get_value("n11")

    @n11.setter
    def n11(self, value: int) -> None:
        """Set the n11 property."""
        self._cards[2].set_value("n11", value)

    @property
    def n12(self) -> typing.Optional[int]:
        """Get or set the Nodal point 12.
        """ # nopep8
        return self._cards[2].get_value("n12")

    @n12.setter
    def n12(self, value: int) -> None:
        """Set the n12 property."""
        self._cards[2].set_value("n12", value)

    @property
    def n13(self) -> typing.Optional[int]:
        """Get or set the Nodal point 13.
        """ # nopep8
        return self._cards[2].get_value("n13")

    @n13.setter
    def n13(self, value: int) -> None:
        """Set the n13 property."""
        self._cards[2].set_value("n13", value)

    @property
    def n14(self) -> typing.Optional[int]:
        """Get or set the Nodal point 14.
        """ # nopep8
        return self._cards[2].get_value("n14")

    @n14.setter
    def n14(self, value: int) -> None:
        """Set the n14 property."""
        self._cards[2].set_value("n14", value)

    @property
    def n15(self) -> typing.Optional[int]:
        """Get or set the Nodal point 15.
        """ # nopep8
        return self._cards[2].get_value("n15")

    @n15.setter
    def n15(self, value: int) -> None:
        """Set the n15 property."""
        self._cards[2].set_value("n15", value)

    @property
    def n16(self) -> typing.Optional[int]:
        """Get or set the Nodal point 16.
        """ # nopep8
        return self._cards[2].get_value("n16")

    @n16.setter
    def n16(self, value: int) -> None:
        """Set the n16 property."""
        self._cards[2].set_value("n16", value)

    @property
    def n17(self) -> typing.Optional[int]:
        """Get or set the Nodal point 17.
        """ # nopep8
        return self._cards[2].get_value("n17")

    @n17.setter
    def n17(self, value: int) -> None:
        """Set the n17 property."""
        self._cards[2].set_value("n17", value)

    @property
    def n18(self) -> typing.Optional[int]:
        """Get or set the Nodal point 18.
        """ # nopep8
        return self._cards[2].get_value("n18")

    @n18.setter
    def n18(self, value: int) -> None:
        """Set the n18 property."""
        self._cards[2].set_value("n18", value)

    @property
    def n19(self) -> typing.Optional[int]:
        """Get or set the Nodal point 19.
        """ # nopep8
        return self._cards[2].get_value("n19")

    @n19.setter
    def n19(self, value: int) -> None:
        """Set the n19 property."""
        self._cards[2].set_value("n19", value)

    @property
    def n20(self) -> typing.Optional[int]:
        """Get or set the Nodal point 20.
        """ # nopep8
        return self._cards[2].get_value("n20")

    @n20.setter
    def n20(self, value: int) -> None:
        """Set the n20 property."""
        self._cards[2].set_value("n20", value)

    @property
    def n21(self) -> typing.Optional[int]:
        """Get or set the Nodal point 21.
        """ # nopep8
        return self._cards[3].get_value("n21")

    @n21.setter
    def n21(self, value: int) -> None:
        """Set the n21 property."""
        self._cards[3].set_value("n21", value)

    @property
    def n22(self) -> typing.Optional[int]:
        """Get or set the Nodal point 22.
        """ # nopep8
        return self._cards[3].get_value("n22")

    @n22.setter
    def n22(self, value: int) -> None:
        """Set the n22 property."""
        self._cards[3].set_value("n22", value)

    @property
    def n23(self) -> typing.Optional[int]:
        """Get or set the Nodal point 23.
        """ # nopep8
        return self._cards[3].get_value("n23")

    @n23.setter
    def n23(self, value: int) -> None:
        """Set the n23 property."""
        self._cards[3].set_value("n23", value)

    @property
    def n24(self) -> typing.Optional[int]:
        """Get or set the Nodal point 24.
        """ # nopep8
        return self._cards[3].get_value("n24")

    @n24.setter
    def n24(self, value: int) -> None:
        """Set the n24 property."""
        self._cards[3].set_value("n24", value)

    @property
    def n25(self) -> typing.Optional[int]:
        """Get or set the Nodal point 25.
        """ # nopep8
        return self._cards[3].get_value("n25")

    @n25.setter
    def n25(self, value: int) -> None:
        """Set the n25 property."""
        self._cards[3].set_value("n25", value)

    @property
    def n26(self) -> typing.Optional[int]:
        """Get or set the Nodal point 26.
        """ # nopep8
        return self._cards[3].get_value("n26")

    @n26.setter
    def n26(self, value: int) -> None:
        """Set the n26 property."""
        self._cards[3].set_value("n26", value)

    @property
    def n27(self) -> typing.Optional[int]:
        """Get or set the Nodal point 27.
        """ # nopep8
        return self._cards[3].get_value("n27")

    @n27.setter
    def n27(self, value: int) -> None:
        """Set the n27 property."""
        self._cards[3].set_value("n27", value)

