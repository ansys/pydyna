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

"""Module providing the ElementSolidH64 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSOLIDH64_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
)

_ELEMENTSOLIDH64_CARD1 = (
    FieldSchema("n1", int, 0, 8, None),
    FieldSchema("n2", int, 8, 8, None),
    FieldSchema("n3", int, 16, 8, None),
    FieldSchema("n4", int, 24, 8, None),
    FieldSchema("n5", int, 32, 8, None),
    FieldSchema("n6", int, 40, 8, None),
    FieldSchema("n7", int, 48, 8, None),
    FieldSchema("n8", int, 56, 8, None),
    FieldSchema("n9", int, 64, 8, None),
    FieldSchema("n10", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD2 = (
    FieldSchema("n11", int, 0, 8, None),
    FieldSchema("n12", int, 8, 8, None),
    FieldSchema("n13", int, 16, 8, None),
    FieldSchema("n14", int, 24, 8, None),
    FieldSchema("n15", int, 32, 8, None),
    FieldSchema("n16", int, 40, 8, None),
    FieldSchema("n17", int, 48, 8, None),
    FieldSchema("n18", int, 56, 8, None),
    FieldSchema("n19", int, 64, 8, None),
    FieldSchema("n20", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD3 = (
    FieldSchema("n21", int, 0, 8, None),
    FieldSchema("n22", int, 8, 8, None),
    FieldSchema("n23", int, 16, 8, None),
    FieldSchema("n24", int, 24, 8, None),
    FieldSchema("n25", int, 32, 8, None),
    FieldSchema("n26", int, 40, 8, None),
    FieldSchema("n27", int, 48, 8, None),
    FieldSchema("n28", int, 56, 8, None),
    FieldSchema("n29", int, 64, 8, None),
    FieldSchema("n30", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD4 = (
    FieldSchema("n31", int, 0, 8, None),
    FieldSchema("n32", int, 8, 8, None),
    FieldSchema("n33", int, 16, 8, None),
    FieldSchema("n34", int, 24, 8, None),
    FieldSchema("n35", int, 32, 8, None),
    FieldSchema("n36", int, 40, 8, None),
    FieldSchema("n37", int, 48, 8, None),
    FieldSchema("n38", int, 56, 8, None),
    FieldSchema("n39", int, 64, 8, None),
    FieldSchema("n40", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD5 = (
    FieldSchema("n41", int, 0, 8, None),
    FieldSchema("n42", int, 8, 8, None),
    FieldSchema("n43", int, 16, 8, None),
    FieldSchema("n44", int, 24, 8, None),
    FieldSchema("n45", int, 32, 8, None),
    FieldSchema("n46", int, 40, 8, None),
    FieldSchema("n47", int, 48, 8, None),
    FieldSchema("n48", int, 56, 8, None),
    FieldSchema("n49", int, 64, 8, None),
    FieldSchema("n50", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD6 = (
    FieldSchema("n51", int, 0, 8, None),
    FieldSchema("n52", int, 8, 8, None),
    FieldSchema("n53", int, 16, 8, None),
    FieldSchema("n54", int, 24, 8, None),
    FieldSchema("n55", int, 32, 8, None),
    FieldSchema("n56", int, 40, 8, None),
    FieldSchema("n57", int, 48, 8, None),
    FieldSchema("n58", int, 56, 8, None),
    FieldSchema("n59", int, 64, 8, None),
    FieldSchema("n50", int, 72, 8, None),
)

_ELEMENTSOLIDH64_CARD7 = (
    FieldSchema("n61", int, 0, 8, None),
    FieldSchema("n62", int, 8, 8, None),
    FieldSchema("n63", int, 16, 8, None),
    FieldSchema("n64", int, 24, 8, None),
)

class ElementSolidH64(KeywordBase):
    """DYNA ELEMENT_SOLID_H64 keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_H64"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "n9": LinkType.NODE,
        "n10": LinkType.NODE,
        "n11": LinkType.NODE,
        "n12": LinkType.NODE,
        "n13": LinkType.NODE,
        "n14": LinkType.NODE,
        "n15": LinkType.NODE,
        "n16": LinkType.NODE,
        "n17": LinkType.NODE,
        "n18": LinkType.NODE,
        "n19": LinkType.NODE,
        "n20": LinkType.NODE,
        "n21": LinkType.NODE,
        "n22": LinkType.NODE,
        "n23": LinkType.NODE,
        "n24": LinkType.NODE,
        "n25": LinkType.NODE,
        "n26": LinkType.NODE,
        "n27": LinkType.NODE,
        "n28": LinkType.NODE,
        "n29": LinkType.NODE,
        "n30": LinkType.NODE,
        "n31": LinkType.NODE,
        "n32": LinkType.NODE,
        "n33": LinkType.NODE,
        "n34": LinkType.NODE,
        "n35": LinkType.NODE,
        "n36": LinkType.NODE,
        "n37": LinkType.NODE,
        "n38": LinkType.NODE,
        "n39": LinkType.NODE,
        "n40": LinkType.NODE,
        "n41": LinkType.NODE,
        "n42": LinkType.NODE,
        "n43": LinkType.NODE,
        "n44": LinkType.NODE,
        "n45": LinkType.NODE,
        "n46": LinkType.NODE,
        "n47": LinkType.NODE,
        "n48": LinkType.NODE,
        "n49": LinkType.NODE,
        "n50": LinkType.NODE,
        "n51": LinkType.NODE,
        "n52": LinkType.NODE,
        "n53": LinkType.NODE,
        "n54": LinkType.NODE,
        "n55": LinkType.NODE,
        "n56": LinkType.NODE,
        "n57": LinkType.NODE,
        "n58": LinkType.NODE,
        "n59": LinkType.NODE,
        "n50": LinkType.NODE,
        "n61": LinkType.NODE,
        "n62": LinkType.NODE,
        "n63": LinkType.NODE,
        "n64": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementSolidH64 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDH64_CARD7,
                **kwargs,
            ),        ]
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

    @property
    def n28(self) -> typing.Optional[int]:
        """Get or set the Nodal point 28.
        """ # nopep8
        return self._cards[3].get_value("n28")

    @n28.setter
    def n28(self, value: int) -> None:
        """Set the n28 property."""
        self._cards[3].set_value("n28", value)

    @property
    def n29(self) -> typing.Optional[int]:
        """Get or set the Nodal point 29.
        """ # nopep8
        return self._cards[3].get_value("n29")

    @n29.setter
    def n29(self, value: int) -> None:
        """Set the n29 property."""
        self._cards[3].set_value("n29", value)

    @property
    def n30(self) -> typing.Optional[int]:
        """Get or set the Nodal point 30.
        """ # nopep8
        return self._cards[3].get_value("n30")

    @n30.setter
    def n30(self, value: int) -> None:
        """Set the n30 property."""
        self._cards[3].set_value("n30", value)

    @property
    def n31(self) -> typing.Optional[int]:
        """Get or set the Nodal point 31.
        """ # nopep8
        return self._cards[4].get_value("n31")

    @n31.setter
    def n31(self, value: int) -> None:
        """Set the n31 property."""
        self._cards[4].set_value("n31", value)

    @property
    def n32(self) -> typing.Optional[int]:
        """Get or set the Nodal point 32.
        """ # nopep8
        return self._cards[4].get_value("n32")

    @n32.setter
    def n32(self, value: int) -> None:
        """Set the n32 property."""
        self._cards[4].set_value("n32", value)

    @property
    def n33(self) -> typing.Optional[int]:
        """Get or set the Nodal point 33.
        """ # nopep8
        return self._cards[4].get_value("n33")

    @n33.setter
    def n33(self, value: int) -> None:
        """Set the n33 property."""
        self._cards[4].set_value("n33", value)

    @property
    def n34(self) -> typing.Optional[int]:
        """Get or set the Nodal point 34.
        """ # nopep8
        return self._cards[4].get_value("n34")

    @n34.setter
    def n34(self, value: int) -> None:
        """Set the n34 property."""
        self._cards[4].set_value("n34", value)

    @property
    def n35(self) -> typing.Optional[int]:
        """Get or set the Nodal point 35.
        """ # nopep8
        return self._cards[4].get_value("n35")

    @n35.setter
    def n35(self, value: int) -> None:
        """Set the n35 property."""
        self._cards[4].set_value("n35", value)

    @property
    def n36(self) -> typing.Optional[int]:
        """Get or set the Nodal point 36.
        """ # nopep8
        return self._cards[4].get_value("n36")

    @n36.setter
    def n36(self, value: int) -> None:
        """Set the n36 property."""
        self._cards[4].set_value("n36", value)

    @property
    def n37(self) -> typing.Optional[int]:
        """Get or set the Nodal point 37.
        """ # nopep8
        return self._cards[4].get_value("n37")

    @n37.setter
    def n37(self, value: int) -> None:
        """Set the n37 property."""
        self._cards[4].set_value("n37", value)

    @property
    def n38(self) -> typing.Optional[int]:
        """Get or set the Nodal point 38.
        """ # nopep8
        return self._cards[4].get_value("n38")

    @n38.setter
    def n38(self, value: int) -> None:
        """Set the n38 property."""
        self._cards[4].set_value("n38", value)

    @property
    def n39(self) -> typing.Optional[int]:
        """Get or set the Nodal point 39.
        """ # nopep8
        return self._cards[4].get_value("n39")

    @n39.setter
    def n39(self, value: int) -> None:
        """Set the n39 property."""
        self._cards[4].set_value("n39", value)

    @property
    def n40(self) -> typing.Optional[int]:
        """Get or set the Nodal point 40.
        """ # nopep8
        return self._cards[4].get_value("n40")

    @n40.setter
    def n40(self, value: int) -> None:
        """Set the n40 property."""
        self._cards[4].set_value("n40", value)

    @property
    def n41(self) -> typing.Optional[int]:
        """Get or set the Nodal point 41.
        """ # nopep8
        return self._cards[5].get_value("n41")

    @n41.setter
    def n41(self, value: int) -> None:
        """Set the n41 property."""
        self._cards[5].set_value("n41", value)

    @property
    def n42(self) -> typing.Optional[int]:
        """Get or set the Nodal point 42.
        """ # nopep8
        return self._cards[5].get_value("n42")

    @n42.setter
    def n42(self, value: int) -> None:
        """Set the n42 property."""
        self._cards[5].set_value("n42", value)

    @property
    def n43(self) -> typing.Optional[int]:
        """Get or set the Nodal point 43.
        """ # nopep8
        return self._cards[5].get_value("n43")

    @n43.setter
    def n43(self, value: int) -> None:
        """Set the n43 property."""
        self._cards[5].set_value("n43", value)

    @property
    def n44(self) -> typing.Optional[int]:
        """Get or set the Nodal point 44.
        """ # nopep8
        return self._cards[5].get_value("n44")

    @n44.setter
    def n44(self, value: int) -> None:
        """Set the n44 property."""
        self._cards[5].set_value("n44", value)

    @property
    def n45(self) -> typing.Optional[int]:
        """Get or set the Nodal point 45.
        """ # nopep8
        return self._cards[5].get_value("n45")

    @n45.setter
    def n45(self, value: int) -> None:
        """Set the n45 property."""
        self._cards[5].set_value("n45", value)

    @property
    def n46(self) -> typing.Optional[int]:
        """Get or set the Nodal point 46.
        """ # nopep8
        return self._cards[5].get_value("n46")

    @n46.setter
    def n46(self, value: int) -> None:
        """Set the n46 property."""
        self._cards[5].set_value("n46", value)

    @property
    def n47(self) -> typing.Optional[int]:
        """Get or set the Nodal point 47.
        """ # nopep8
        return self._cards[5].get_value("n47")

    @n47.setter
    def n47(self, value: int) -> None:
        """Set the n47 property."""
        self._cards[5].set_value("n47", value)

    @property
    def n48(self) -> typing.Optional[int]:
        """Get or set the Nodal point 48.
        """ # nopep8
        return self._cards[5].get_value("n48")

    @n48.setter
    def n48(self, value: int) -> None:
        """Set the n48 property."""
        self._cards[5].set_value("n48", value)

    @property
    def n49(self) -> typing.Optional[int]:
        """Get or set the Nodal point 49.
        """ # nopep8
        return self._cards[5].get_value("n49")

    @n49.setter
    def n49(self, value: int) -> None:
        """Set the n49 property."""
        self._cards[5].set_value("n49", value)

    @property
    def n50(self) -> typing.Optional[int]:
        """Get or set the Nodal point 50.
        """ # nopep8
        return self._cards[5].get_value("n50")

    @n50.setter
    def n50(self, value: int) -> None:
        """Set the n50 property."""
        self._cards[5].set_value("n50", value)

    @property
    def n51(self) -> typing.Optional[int]:
        """Get or set the Nodal point 51.
        """ # nopep8
        return self._cards[6].get_value("n51")

    @n51.setter
    def n51(self, value: int) -> None:
        """Set the n51 property."""
        self._cards[6].set_value("n51", value)

    @property
    def n52(self) -> typing.Optional[int]:
        """Get or set the Nodal point 52.
        """ # nopep8
        return self._cards[6].get_value("n52")

    @n52.setter
    def n52(self, value: int) -> None:
        """Set the n52 property."""
        self._cards[6].set_value("n52", value)

    @property
    def n53(self) -> typing.Optional[int]:
        """Get or set the Nodal point 53.
        """ # nopep8
        return self._cards[6].get_value("n53")

    @n53.setter
    def n53(self, value: int) -> None:
        """Set the n53 property."""
        self._cards[6].set_value("n53", value)

    @property
    def n54(self) -> typing.Optional[int]:
        """Get or set the Nodal point 54.
        """ # nopep8
        return self._cards[6].get_value("n54")

    @n54.setter
    def n54(self, value: int) -> None:
        """Set the n54 property."""
        self._cards[6].set_value("n54", value)

    @property
    def n55(self) -> typing.Optional[int]:
        """Get or set the Nodal point 55.
        """ # nopep8
        return self._cards[6].get_value("n55")

    @n55.setter
    def n55(self, value: int) -> None:
        """Set the n55 property."""
        self._cards[6].set_value("n55", value)

    @property
    def n56(self) -> typing.Optional[int]:
        """Get or set the Nodal point 56.
        """ # nopep8
        return self._cards[6].get_value("n56")

    @n56.setter
    def n56(self, value: int) -> None:
        """Set the n56 property."""
        self._cards[6].set_value("n56", value)

    @property
    def n57(self) -> typing.Optional[int]:
        """Get or set the Nodal point 57.
        """ # nopep8
        return self._cards[6].get_value("n57")

    @n57.setter
    def n57(self, value: int) -> None:
        """Set the n57 property."""
        self._cards[6].set_value("n57", value)

    @property
    def n58(self) -> typing.Optional[int]:
        """Get or set the Nodal point 58.
        """ # nopep8
        return self._cards[6].get_value("n58")

    @n58.setter
    def n58(self, value: int) -> None:
        """Set the n58 property."""
        self._cards[6].set_value("n58", value)

    @property
    def n59(self) -> typing.Optional[int]:
        """Get or set the Nodal point 59.
        """ # nopep8
        return self._cards[6].get_value("n59")

    @n59.setter
    def n59(self, value: int) -> None:
        """Set the n59 property."""
        self._cards[6].set_value("n59", value)

    @property
    def n50(self) -> typing.Optional[int]:
        """Get or set the Nodal point 60.
        """ # nopep8
        return self._cards[6].get_value("n50")

    @n50.setter
    def n50(self, value: int) -> None:
        """Set the n50 property."""
        self._cards[6].set_value("n50", value)

    @property
    def n61(self) -> typing.Optional[int]:
        """Get or set the Nodal point 61.
        """ # nopep8
        return self._cards[7].get_value("n61")

    @n61.setter
    def n61(self, value: int) -> None:
        """Set the n61 property."""
        self._cards[7].set_value("n61", value)

    @property
    def n62(self) -> typing.Optional[int]:
        """Get or set the Nodal point 62.
        """ # nopep8
        return self._cards[7].get_value("n62")

    @n62.setter
    def n62(self, value: int) -> None:
        """Set the n62 property."""
        self._cards[7].set_value("n62", value)

    @property
    def n63(self) -> typing.Optional[int]:
        """Get or set the Nodal point 63.
        """ # nopep8
        return self._cards[7].get_value("n63")

    @n63.setter
    def n63(self, value: int) -> None:
        """Set the n63 property."""
        self._cards[7].set_value("n63", value)

    @property
    def n64(self) -> typing.Optional[int]:
        """Get or set the Nodal point 64.
        """ # nopep8
        return self._cards[7].get_value("n64")

    @n64.setter
    def n64(self, value: int) -> None:
        """Set the n64 property."""
        self._cards[7].set_value("n64", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def n5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def n7_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n7."""
        return self._get_link_by_attr("NODE", "nid", self.n7, "parts")

    @property
    def n8_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n8."""
        return self._get_link_by_attr("NODE", "nid", self.n8, "parts")

    @property
    def n9_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n9."""
        return self._get_link_by_attr("NODE", "nid", self.n9, "parts")

    @property
    def n10_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n10."""
        return self._get_link_by_attr("NODE", "nid", self.n10, "parts")

    @property
    def n11_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n11."""
        return self._get_link_by_attr("NODE", "nid", self.n11, "parts")

    @property
    def n12_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n12."""
        return self._get_link_by_attr("NODE", "nid", self.n12, "parts")

    @property
    def n13_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n13."""
        return self._get_link_by_attr("NODE", "nid", self.n13, "parts")

    @property
    def n14_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n14."""
        return self._get_link_by_attr("NODE", "nid", self.n14, "parts")

    @property
    def n15_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n15."""
        return self._get_link_by_attr("NODE", "nid", self.n15, "parts")

    @property
    def n16_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n16."""
        return self._get_link_by_attr("NODE", "nid", self.n16, "parts")

    @property
    def n17_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n17."""
        return self._get_link_by_attr("NODE", "nid", self.n17, "parts")

    @property
    def n18_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n18."""
        return self._get_link_by_attr("NODE", "nid", self.n18, "parts")

    @property
    def n19_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n19."""
        return self._get_link_by_attr("NODE", "nid", self.n19, "parts")

    @property
    def n20_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n20."""
        return self._get_link_by_attr("NODE", "nid", self.n20, "parts")

    @property
    def n21_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n21."""
        return self._get_link_by_attr("NODE", "nid", self.n21, "parts")

    @property
    def n22_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n22."""
        return self._get_link_by_attr("NODE", "nid", self.n22, "parts")

    @property
    def n23_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n23."""
        return self._get_link_by_attr("NODE", "nid", self.n23, "parts")

    @property
    def n24_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n24."""
        return self._get_link_by_attr("NODE", "nid", self.n24, "parts")

    @property
    def n25_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n25."""
        return self._get_link_by_attr("NODE", "nid", self.n25, "parts")

    @property
    def n26_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n26."""
        return self._get_link_by_attr("NODE", "nid", self.n26, "parts")

    @property
    def n27_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n27."""
        return self._get_link_by_attr("NODE", "nid", self.n27, "parts")

    @property
    def n28_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n28."""
        return self._get_link_by_attr("NODE", "nid", self.n28, "parts")

    @property
    def n29_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n29."""
        return self._get_link_by_attr("NODE", "nid", self.n29, "parts")

    @property
    def n30_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n30."""
        return self._get_link_by_attr("NODE", "nid", self.n30, "parts")

    @property
    def n31_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n31."""
        return self._get_link_by_attr("NODE", "nid", self.n31, "parts")

    @property
    def n32_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n32."""
        return self._get_link_by_attr("NODE", "nid", self.n32, "parts")

    @property
    def n33_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n33."""
        return self._get_link_by_attr("NODE", "nid", self.n33, "parts")

    @property
    def n34_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n34."""
        return self._get_link_by_attr("NODE", "nid", self.n34, "parts")

    @property
    def n35_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n35."""
        return self._get_link_by_attr("NODE", "nid", self.n35, "parts")

    @property
    def n36_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n36."""
        return self._get_link_by_attr("NODE", "nid", self.n36, "parts")

    @property
    def n37_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n37."""
        return self._get_link_by_attr("NODE", "nid", self.n37, "parts")

    @property
    def n38_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n38."""
        return self._get_link_by_attr("NODE", "nid", self.n38, "parts")

    @property
    def n39_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n39."""
        return self._get_link_by_attr("NODE", "nid", self.n39, "parts")

    @property
    def n40_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n40."""
        return self._get_link_by_attr("NODE", "nid", self.n40, "parts")

    @property
    def n41_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n41."""
        return self._get_link_by_attr("NODE", "nid", self.n41, "parts")

    @property
    def n42_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n42."""
        return self._get_link_by_attr("NODE", "nid", self.n42, "parts")

    @property
    def n43_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n43."""
        return self._get_link_by_attr("NODE", "nid", self.n43, "parts")

    @property
    def n44_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n44."""
        return self._get_link_by_attr("NODE", "nid", self.n44, "parts")

    @property
    def n45_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n45."""
        return self._get_link_by_attr("NODE", "nid", self.n45, "parts")

    @property
    def n46_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n46."""
        return self._get_link_by_attr("NODE", "nid", self.n46, "parts")

    @property
    def n47_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n47."""
        return self._get_link_by_attr("NODE", "nid", self.n47, "parts")

    @property
    def n48_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n48."""
        return self._get_link_by_attr("NODE", "nid", self.n48, "parts")

    @property
    def n49_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n49."""
        return self._get_link_by_attr("NODE", "nid", self.n49, "parts")

    @property
    def n50_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n50."""
        return self._get_link_by_attr("NODE", "nid", self.n50, "parts")

    @property
    def n51_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n51."""
        return self._get_link_by_attr("NODE", "nid", self.n51, "parts")

    @property
    def n52_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n52."""
        return self._get_link_by_attr("NODE", "nid", self.n52, "parts")

    @property
    def n53_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n53."""
        return self._get_link_by_attr("NODE", "nid", self.n53, "parts")

    @property
    def n54_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n54."""
        return self._get_link_by_attr("NODE", "nid", self.n54, "parts")

    @property
    def n55_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n55."""
        return self._get_link_by_attr("NODE", "nid", self.n55, "parts")

    @property
    def n56_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n56."""
        return self._get_link_by_attr("NODE", "nid", self.n56, "parts")

    @property
    def n57_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n57."""
        return self._get_link_by_attr("NODE", "nid", self.n57, "parts")

    @property
    def n58_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n58."""
        return self._get_link_by_attr("NODE", "nid", self.n58, "parts")

    @property
    def n59_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n59."""
        return self._get_link_by_attr("NODE", "nid", self.n59, "parts")

    @property
    def n50_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n50."""
        return self._get_link_by_attr("NODE", "nid", self.n50, "parts")

    @property
    def n61_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n61."""
        return self._get_link_by_attr("NODE", "nid", self.n61, "parts")

    @property
    def n62_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n62."""
        return self._get_link_by_attr("NODE", "nid", self.n62, "parts")

    @property
    def n63_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n63."""
        return self._get_link_by_attr("NODE", "nid", self.n63, "parts")

    @property
    def n64_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n64."""
        return self._get_link_by_attr("NODE", "nid", self.n64, "parts")

