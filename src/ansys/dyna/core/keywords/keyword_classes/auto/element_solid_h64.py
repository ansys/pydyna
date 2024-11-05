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

class ElementSolidH64(KeywordBase):
    """DYNA ELEMENT_SOLID_H64 keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_H64"

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
                        "n11",
                        int,
                        0,
                        8,
                        kwargs.get("n11")
                    ),
                    Field(
                        "n12",
                        int,
                        8,
                        8,
                        kwargs.get("n12")
                    ),
                    Field(
                        "n13",
                        int,
                        16,
                        8,
                        kwargs.get("n13")
                    ),
                    Field(
                        "n14",
                        int,
                        24,
                        8,
                        kwargs.get("n14")
                    ),
                    Field(
                        "n15",
                        int,
                        32,
                        8,
                        kwargs.get("n15")
                    ),
                    Field(
                        "n16",
                        int,
                        40,
                        8,
                        kwargs.get("n16")
                    ),
                    Field(
                        "n17",
                        int,
                        48,
                        8,
                        kwargs.get("n17")
                    ),
                    Field(
                        "n18",
                        int,
                        56,
                        8,
                        kwargs.get("n18")
                    ),
                    Field(
                        "n19",
                        int,
                        64,
                        8,
                        kwargs.get("n19")
                    ),
                    Field(
                        "n20",
                        int,
                        72,
                        8,
                        kwargs.get("n20")
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
                        kwargs.get("n21")
                    ),
                    Field(
                        "n22",
                        int,
                        8,
                        8,
                        kwargs.get("n22")
                    ),
                    Field(
                        "n23",
                        int,
                        16,
                        8,
                        kwargs.get("n23")
                    ),
                    Field(
                        "n24",
                        int,
                        24,
                        8,
                        kwargs.get("n24")
                    ),
                    Field(
                        "n25",
                        int,
                        32,
                        8,
                        kwargs.get("n25")
                    ),
                    Field(
                        "n26",
                        int,
                        40,
                        8,
                        kwargs.get("n26")
                    ),
                    Field(
                        "n27",
                        int,
                        48,
                        8,
                        kwargs.get("n27")
                    ),
                    Field(
                        "n28",
                        int,
                        56,
                        8,
                        kwargs.get("n28")
                    ),
                    Field(
                        "n29",
                        int,
                        64,
                        8,
                        kwargs.get("n29")
                    ),
                    Field(
                        "n30",
                        int,
                        72,
                        8,
                        kwargs.get("n30")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n31",
                        int,
                        0,
                        8,
                        kwargs.get("n31")
                    ),
                    Field(
                        "n32",
                        int,
                        8,
                        8,
                        kwargs.get("n32")
                    ),
                    Field(
                        "n33",
                        int,
                        16,
                        8,
                        kwargs.get("n33")
                    ),
                    Field(
                        "n34",
                        int,
                        24,
                        8,
                        kwargs.get("n34")
                    ),
                    Field(
                        "n35",
                        int,
                        32,
                        8,
                        kwargs.get("n35")
                    ),
                    Field(
                        "n36",
                        int,
                        40,
                        8,
                        kwargs.get("n36")
                    ),
                    Field(
                        "n37",
                        int,
                        48,
                        8,
                        kwargs.get("n37")
                    ),
                    Field(
                        "n38",
                        int,
                        56,
                        8,
                        kwargs.get("n38")
                    ),
                    Field(
                        "n39",
                        int,
                        64,
                        8,
                        kwargs.get("n39")
                    ),
                    Field(
                        "n40",
                        int,
                        72,
                        8,
                        kwargs.get("n40")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n41",
                        int,
                        0,
                        8,
                        kwargs.get("n41")
                    ),
                    Field(
                        "n42",
                        int,
                        8,
                        8,
                        kwargs.get("n42")
                    ),
                    Field(
                        "n43",
                        int,
                        16,
                        8,
                        kwargs.get("n43")
                    ),
                    Field(
                        "n44",
                        int,
                        24,
                        8,
                        kwargs.get("n44")
                    ),
                    Field(
                        "n45",
                        int,
                        32,
                        8,
                        kwargs.get("n45")
                    ),
                    Field(
                        "n46",
                        int,
                        40,
                        8,
                        kwargs.get("n46")
                    ),
                    Field(
                        "n47",
                        int,
                        48,
                        8,
                        kwargs.get("n47")
                    ),
                    Field(
                        "n48",
                        int,
                        56,
                        8,
                        kwargs.get("n48")
                    ),
                    Field(
                        "n49",
                        int,
                        64,
                        8,
                        kwargs.get("n49")
                    ),
                    Field(
                        "n50",
                        int,
                        72,
                        8,
                        kwargs.get("n50")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n51",
                        int,
                        0,
                        8,
                        kwargs.get("n51")
                    ),
                    Field(
                        "n52",
                        int,
                        8,
                        8,
                        kwargs.get("n52")
                    ),
                    Field(
                        "n53",
                        int,
                        16,
                        8,
                        kwargs.get("n53")
                    ),
                    Field(
                        "n54",
                        int,
                        24,
                        8,
                        kwargs.get("n54")
                    ),
                    Field(
                        "n55",
                        int,
                        32,
                        8,
                        kwargs.get("n55")
                    ),
                    Field(
                        "n56",
                        int,
                        40,
                        8,
                        kwargs.get("n56")
                    ),
                    Field(
                        "n57",
                        int,
                        48,
                        8,
                        kwargs.get("n57")
                    ),
                    Field(
                        "n58",
                        int,
                        56,
                        8,
                        kwargs.get("n58")
                    ),
                    Field(
                        "n59",
                        int,
                        64,
                        8,
                        kwargs.get("n59")
                    ),
                    Field(
                        "n50",
                        int,
                        72,
                        8,
                        kwargs.get("n50")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n61",
                        int,
                        0,
                        8,
                        kwargs.get("n61")
                    ),
                    Field(
                        "n62",
                        int,
                        8,
                        8,
                        kwargs.get("n62")
                    ),
                    Field(
                        "n63",
                        int,
                        16,
                        8,
                        kwargs.get("n63")
                    ),
                    Field(
                        "n64",
                        int,
                        24,
                        8,
                        kwargs.get("n64")
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
    def n11(self) -> typing.Optional[int]:
        """Get or set the Nodal point 11.
        """ # nopep8
        return self._cards[2].get_value("n11")

    @n11.setter
    def n11(self, value: int) -> None:
        self._cards[2].set_value("n11", value)

    @property
    def n12(self) -> typing.Optional[int]:
        """Get or set the Nodal point 12.
        """ # nopep8
        return self._cards[2].get_value("n12")

    @n12.setter
    def n12(self, value: int) -> None:
        self._cards[2].set_value("n12", value)

    @property
    def n13(self) -> typing.Optional[int]:
        """Get or set the Nodal point 13.
        """ # nopep8
        return self._cards[2].get_value("n13")

    @n13.setter
    def n13(self, value: int) -> None:
        self._cards[2].set_value("n13", value)

    @property
    def n14(self) -> typing.Optional[int]:
        """Get or set the Nodal point 14.
        """ # nopep8
        return self._cards[2].get_value("n14")

    @n14.setter
    def n14(self, value: int) -> None:
        self._cards[2].set_value("n14", value)

    @property
    def n15(self) -> typing.Optional[int]:
        """Get or set the Nodal point 15.
        """ # nopep8
        return self._cards[2].get_value("n15")

    @n15.setter
    def n15(self, value: int) -> None:
        self._cards[2].set_value("n15", value)

    @property
    def n16(self) -> typing.Optional[int]:
        """Get or set the Nodal point 16.
        """ # nopep8
        return self._cards[2].get_value("n16")

    @n16.setter
    def n16(self, value: int) -> None:
        self._cards[2].set_value("n16", value)

    @property
    def n17(self) -> typing.Optional[int]:
        """Get or set the Nodal point 17.
        """ # nopep8
        return self._cards[2].get_value("n17")

    @n17.setter
    def n17(self, value: int) -> None:
        self._cards[2].set_value("n17", value)

    @property
    def n18(self) -> typing.Optional[int]:
        """Get or set the Nodal point 18.
        """ # nopep8
        return self._cards[2].get_value("n18")

    @n18.setter
    def n18(self, value: int) -> None:
        self._cards[2].set_value("n18", value)

    @property
    def n19(self) -> typing.Optional[int]:
        """Get or set the Nodal point 19.
        """ # nopep8
        return self._cards[2].get_value("n19")

    @n19.setter
    def n19(self, value: int) -> None:
        self._cards[2].set_value("n19", value)

    @property
    def n20(self) -> typing.Optional[int]:
        """Get or set the Nodal point 20.
        """ # nopep8
        return self._cards[2].get_value("n20")

    @n20.setter
    def n20(self, value: int) -> None:
        self._cards[2].set_value("n20", value)

    @property
    def n21(self) -> typing.Optional[int]:
        """Get or set the Nodal point 21.
        """ # nopep8
        return self._cards[3].get_value("n21")

    @n21.setter
    def n21(self, value: int) -> None:
        self._cards[3].set_value("n21", value)

    @property
    def n22(self) -> typing.Optional[int]:
        """Get or set the Nodal point 22.
        """ # nopep8
        return self._cards[3].get_value("n22")

    @n22.setter
    def n22(self, value: int) -> None:
        self._cards[3].set_value("n22", value)

    @property
    def n23(self) -> typing.Optional[int]:
        """Get or set the Nodal point 23.
        """ # nopep8
        return self._cards[3].get_value("n23")

    @n23.setter
    def n23(self, value: int) -> None:
        self._cards[3].set_value("n23", value)

    @property
    def n24(self) -> typing.Optional[int]:
        """Get or set the Nodal point 24.
        """ # nopep8
        return self._cards[3].get_value("n24")

    @n24.setter
    def n24(self, value: int) -> None:
        self._cards[3].set_value("n24", value)

    @property
    def n25(self) -> typing.Optional[int]:
        """Get or set the Nodal point 25.
        """ # nopep8
        return self._cards[3].get_value("n25")

    @n25.setter
    def n25(self, value: int) -> None:
        self._cards[3].set_value("n25", value)

    @property
    def n26(self) -> typing.Optional[int]:
        """Get or set the Nodal point 26.
        """ # nopep8
        return self._cards[3].get_value("n26")

    @n26.setter
    def n26(self, value: int) -> None:
        self._cards[3].set_value("n26", value)

    @property
    def n27(self) -> typing.Optional[int]:
        """Get or set the Nodal point 27.
        """ # nopep8
        return self._cards[3].get_value("n27")

    @n27.setter
    def n27(self, value: int) -> None:
        self._cards[3].set_value("n27", value)

    @property
    def n28(self) -> typing.Optional[int]:
        """Get or set the Nodal point 28.
        """ # nopep8
        return self._cards[3].get_value("n28")

    @n28.setter
    def n28(self, value: int) -> None:
        self._cards[3].set_value("n28", value)

    @property
    def n29(self) -> typing.Optional[int]:
        """Get or set the Nodal point 29.
        """ # nopep8
        return self._cards[3].get_value("n29")

    @n29.setter
    def n29(self, value: int) -> None:
        self._cards[3].set_value("n29", value)

    @property
    def n30(self) -> typing.Optional[int]:
        """Get or set the Nodal point 30.
        """ # nopep8
        return self._cards[3].get_value("n30")

    @n30.setter
    def n30(self, value: int) -> None:
        self._cards[3].set_value("n30", value)

    @property
    def n31(self) -> typing.Optional[int]:
        """Get or set the Nodal point 31.
        """ # nopep8
        return self._cards[4].get_value("n31")

    @n31.setter
    def n31(self, value: int) -> None:
        self._cards[4].set_value("n31", value)

    @property
    def n32(self) -> typing.Optional[int]:
        """Get or set the Nodal point 32.
        """ # nopep8
        return self._cards[4].get_value("n32")

    @n32.setter
    def n32(self, value: int) -> None:
        self._cards[4].set_value("n32", value)

    @property
    def n33(self) -> typing.Optional[int]:
        """Get or set the Nodal point 33.
        """ # nopep8
        return self._cards[4].get_value("n33")

    @n33.setter
    def n33(self, value: int) -> None:
        self._cards[4].set_value("n33", value)

    @property
    def n34(self) -> typing.Optional[int]:
        """Get or set the Nodal point 34.
        """ # nopep8
        return self._cards[4].get_value("n34")

    @n34.setter
    def n34(self, value: int) -> None:
        self._cards[4].set_value("n34", value)

    @property
    def n35(self) -> typing.Optional[int]:
        """Get or set the Nodal point 35.
        """ # nopep8
        return self._cards[4].get_value("n35")

    @n35.setter
    def n35(self, value: int) -> None:
        self._cards[4].set_value("n35", value)

    @property
    def n36(self) -> typing.Optional[int]:
        """Get or set the Nodal point 36.
        """ # nopep8
        return self._cards[4].get_value("n36")

    @n36.setter
    def n36(self, value: int) -> None:
        self._cards[4].set_value("n36", value)

    @property
    def n37(self) -> typing.Optional[int]:
        """Get or set the Nodal point 37.
        """ # nopep8
        return self._cards[4].get_value("n37")

    @n37.setter
    def n37(self, value: int) -> None:
        self._cards[4].set_value("n37", value)

    @property
    def n38(self) -> typing.Optional[int]:
        """Get or set the Nodal point 38.
        """ # nopep8
        return self._cards[4].get_value("n38")

    @n38.setter
    def n38(self, value: int) -> None:
        self._cards[4].set_value("n38", value)

    @property
    def n39(self) -> typing.Optional[int]:
        """Get or set the Nodal point 39.
        """ # nopep8
        return self._cards[4].get_value("n39")

    @n39.setter
    def n39(self, value: int) -> None:
        self._cards[4].set_value("n39", value)

    @property
    def n40(self) -> typing.Optional[int]:
        """Get or set the Nodal point 40.
        """ # nopep8
        return self._cards[4].get_value("n40")

    @n40.setter
    def n40(self, value: int) -> None:
        self._cards[4].set_value("n40", value)

    @property
    def n41(self) -> typing.Optional[int]:
        """Get or set the Nodal point 41.
        """ # nopep8
        return self._cards[5].get_value("n41")

    @n41.setter
    def n41(self, value: int) -> None:
        self._cards[5].set_value("n41", value)

    @property
    def n42(self) -> typing.Optional[int]:
        """Get or set the Nodal point 42.
        """ # nopep8
        return self._cards[5].get_value("n42")

    @n42.setter
    def n42(self, value: int) -> None:
        self._cards[5].set_value("n42", value)

    @property
    def n43(self) -> typing.Optional[int]:
        """Get or set the Nodal point 43.
        """ # nopep8
        return self._cards[5].get_value("n43")

    @n43.setter
    def n43(self, value: int) -> None:
        self._cards[5].set_value("n43", value)

    @property
    def n44(self) -> typing.Optional[int]:
        """Get or set the Nodal point 44.
        """ # nopep8
        return self._cards[5].get_value("n44")

    @n44.setter
    def n44(self, value: int) -> None:
        self._cards[5].set_value("n44", value)

    @property
    def n45(self) -> typing.Optional[int]:
        """Get or set the Nodal point 45.
        """ # nopep8
        return self._cards[5].get_value("n45")

    @n45.setter
    def n45(self, value: int) -> None:
        self._cards[5].set_value("n45", value)

    @property
    def n46(self) -> typing.Optional[int]:
        """Get or set the Nodal point 46.
        """ # nopep8
        return self._cards[5].get_value("n46")

    @n46.setter
    def n46(self, value: int) -> None:
        self._cards[5].set_value("n46", value)

    @property
    def n47(self) -> typing.Optional[int]:
        """Get or set the Nodal point 47.
        """ # nopep8
        return self._cards[5].get_value("n47")

    @n47.setter
    def n47(self, value: int) -> None:
        self._cards[5].set_value("n47", value)

    @property
    def n48(self) -> typing.Optional[int]:
        """Get or set the Nodal point 48.
        """ # nopep8
        return self._cards[5].get_value("n48")

    @n48.setter
    def n48(self, value: int) -> None:
        self._cards[5].set_value("n48", value)

    @property
    def n49(self) -> typing.Optional[int]:
        """Get or set the Nodal point 49.
        """ # nopep8
        return self._cards[5].get_value("n49")

    @n49.setter
    def n49(self, value: int) -> None:
        self._cards[5].set_value("n49", value)

    @property
    def n50(self) -> typing.Optional[int]:
        """Get or set the Nodal point 50.
        """ # nopep8
        return self._cards[5].get_value("n50")

    @n50.setter
    def n50(self, value: int) -> None:
        self._cards[5].set_value("n50", value)

    @property
    def n51(self) -> typing.Optional[int]:
        """Get or set the Nodal point 51.
        """ # nopep8
        return self._cards[6].get_value("n51")

    @n51.setter
    def n51(self, value: int) -> None:
        self._cards[6].set_value("n51", value)

    @property
    def n52(self) -> typing.Optional[int]:
        """Get or set the Nodal point 52.
        """ # nopep8
        return self._cards[6].get_value("n52")

    @n52.setter
    def n52(self, value: int) -> None:
        self._cards[6].set_value("n52", value)

    @property
    def n53(self) -> typing.Optional[int]:
        """Get or set the Nodal point 53.
        """ # nopep8
        return self._cards[6].get_value("n53")

    @n53.setter
    def n53(self, value: int) -> None:
        self._cards[6].set_value("n53", value)

    @property
    def n54(self) -> typing.Optional[int]:
        """Get or set the Nodal point 54.
        """ # nopep8
        return self._cards[6].get_value("n54")

    @n54.setter
    def n54(self, value: int) -> None:
        self._cards[6].set_value("n54", value)

    @property
    def n55(self) -> typing.Optional[int]:
        """Get or set the Nodal point 55.
        """ # nopep8
        return self._cards[6].get_value("n55")

    @n55.setter
    def n55(self, value: int) -> None:
        self._cards[6].set_value("n55", value)

    @property
    def n56(self) -> typing.Optional[int]:
        """Get or set the Nodal point 56.
        """ # nopep8
        return self._cards[6].get_value("n56")

    @n56.setter
    def n56(self, value: int) -> None:
        self._cards[6].set_value("n56", value)

    @property
    def n57(self) -> typing.Optional[int]:
        """Get or set the Nodal point 57.
        """ # nopep8
        return self._cards[6].get_value("n57")

    @n57.setter
    def n57(self, value: int) -> None:
        self._cards[6].set_value("n57", value)

    @property
    def n58(self) -> typing.Optional[int]:
        """Get or set the Nodal point 58.
        """ # nopep8
        return self._cards[6].get_value("n58")

    @n58.setter
    def n58(self, value: int) -> None:
        self._cards[6].set_value("n58", value)

    @property
    def n59(self) -> typing.Optional[int]:
        """Get or set the Nodal point 59.
        """ # nopep8
        return self._cards[6].get_value("n59")

    @n59.setter
    def n59(self, value: int) -> None:
        self._cards[6].set_value("n59", value)

    @property
    def n50(self) -> typing.Optional[int]:
        """Get or set the Nodal point 60.
        """ # nopep8
        return self._cards[6].get_value("n50")

    @n50.setter
    def n50(self, value: int) -> None:
        self._cards[6].set_value("n50", value)

    @property
    def n61(self) -> typing.Optional[int]:
        """Get or set the Nodal point 61.
        """ # nopep8
        return self._cards[7].get_value("n61")

    @n61.setter
    def n61(self, value: int) -> None:
        self._cards[7].set_value("n61", value)

    @property
    def n62(self) -> typing.Optional[int]:
        """Get or set the Nodal point 62.
        """ # nopep8
        return self._cards[7].get_value("n62")

    @n62.setter
    def n62(self, value: int) -> None:
        self._cards[7].set_value("n62", value)

    @property
    def n63(self) -> typing.Optional[int]:
        """Get or set the Nodal point 63.
        """ # nopep8
        return self._cards[7].get_value("n63")

    @n63.setter
    def n63(self, value: int) -> None:
        self._cards[7].set_value("n63", value)

    @property
    def n64(self) -> typing.Optional[int]:
        """Get or set the Nodal point 64.
        """ # nopep8
        return self._cards[7].get_value("n64")

    @n64.setter
    def n64(self, value: int) -> None:
        self._cards[7].set_value("n64", value)

