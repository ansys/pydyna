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

"""Module providing the BatteryDatabaseHistoryNodelistOnEleset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BATTERYDATABASEHISTORYNODELISTONELESET_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("lcur", int, 10, 10, None),
    FieldSchema("ioopt", int, 20, 10, None),
    FieldSchema("esid", int, 30, 10, None),
)

_BATTERYDATABASEHISTORYNODELISTONELESET_CARD1 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
    FieldSchema("nid4", int, 30, 10, None),
    FieldSchema("nid5", int, 40, 10, None),
    FieldSchema("nid6", int, 50, 10, None),
    FieldSchema("nid7", int, 60, 10, None),
    FieldSchema("nid8", int, 70, 10, None),
)

class BatteryDatabaseHistoryNodelistOnEleset(KeywordBase):
    """DYNA BATTERY_DATABASE_HISTORY_NODELIST_ON_ELESET keyword"""

    keyword = "BATTERY"
    subkeyword = "DATABASE_HISTORY_NODELIST_ON_ELESET"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nid4": LinkType.NODE,
        "nid5": LinkType.NODE,
        "nid6": LinkType.NODE,
        "nid7": LinkType.NODE,
        "nid8": LinkType.NODE,
        "lcur": LinkType.DEFINE_CURVE,
        "esid": LinkType.SET_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the BatteryDatabaseHistoryNodelistOnEleset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYDATABASEHISTORYNODELISTONELESET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYDATABASEHISTORYNODELISTONELESET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval between outputs. If DT is zero, no output is generated.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcur(self) -> typing.Optional[int]:
        """Get or set the Optional curve ID specifying the time interval between outputs. Use *DEFINE_CURVE to define the curve; the abscissa is time, and the ordinate is time interval between outputs.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> typing.Optional[int]:
        """Get or set the Flag to govern behavior of the output frequency load curve defined by LCUR:
        EQ.1:When output is generated at time t_n, the next output time  t_(n + 1) is computed as t_(n + 1) = t_n + LCUR (t_n).This is the default behavior.
        EQ.2:When output is generated at time t_n, the next output time t_(n + 1) is computed ast_(n + 1) = t_n + LCUR (t_(n + 1)).
        EQ.3:Output is generated for each abscissa point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        """Set the ioopt property."""
        self._cards[0].set_value("ioopt", value)

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the ID of a *SET_SOLID element set to use for the binout operation.  The selected elements in the element set of volume structural elements must also be battery electrochemistry elements.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        """Set the esid property."""
        self._cards[0].set_value("esid", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        """Set the nid7 property."""
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Node ID
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        """Set the nid8 property."""
        self._cards[1].set_value("nid8", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid4."""
        return self._get_link_by_attr("NODE", "nid", self.nid4, "parts")

    @property
    def nid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid5."""
        return self._get_link_by_attr("NODE", "nid", self.nid5, "parts")

    @property
    def nid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid6."""
        return self._get_link_by_attr("NODE", "nid", self.nid6, "parts")

    @property
    def nid7_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid7."""
        return self._get_link_by_attr("NODE", "nid", self.nid7, "parts")

    @property
    def nid8_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid8."""
        return self._get_link_by_attr("NODE", "nid", self.nid8, "parts")

    @property
    def lcur_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcur."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcur:
                return kwd
        return None

    @lcur_link.setter
    def lcur_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcur."""
        self.lcur = value.lcid

    @property
    def esid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for esid."""
        return self._get_set_link("SOLID", self.esid)

    @esid_link.setter
    def esid_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for esid."""
        self.esid = value.sid

