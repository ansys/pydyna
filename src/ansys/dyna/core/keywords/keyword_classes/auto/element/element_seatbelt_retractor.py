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

"""Module providing the ElementSeatbeltRetractor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ELEMENTSEATBELTRETRACTOR_CARD0 = (
    FieldSchema("sbrid", int, 0, 10, None),
    FieldSchema("sbrnid", int, 10, 10, None),
    FieldSchema("sbid", int, 20, 10, None),
    FieldSchema("sid1", int, 30, 10, 0),
    FieldSchema("sid2", int, 40, 10, 0),
    FieldSchema("sid3", int, 50, 10, 0),
    FieldSchema("sid4", int, 60, 10, 0),
)

_ELEMENTSEATBELTRETRACTOR_CARD1 = (
    FieldSchema("tdel", float, 0, 10, 0.0),
    FieldSchema("pull", float, 10, 10, 0.0),
    FieldSchema("llcid", int, 20, 10, 0),
    FieldSchema("ulcid", int, 30, 10, 0),
    FieldSchema("lfed", float, 40, 10, 0.0),
)

class ElementSeatbeltRetractor(KeywordBase):
    """DYNA ELEMENT_SEATBELT_RETRACTOR keyword"""

    keyword = "ELEMENT"
    subkeyword = "SEATBELT_RETRACTOR"
    _link_fields = {
        "sbrnid": LinkType.NODE,
        "llcid": LinkType.DEFINE_CURVE,
        "ulcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementSeatbeltRetractor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSEATBELTRETRACTOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSEATBELTRETRACTOR_CARD1,
                **kwargs,
            ),        ]
    @property
    def sbrid(self) -> typing.Optional[int]:
        """Get or set the Retractor ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("sbrid")

    @sbrid.setter
    def sbrid(self, value: int) -> None:
        """Set the sbrid property."""
        self._cards[0].set_value("sbrid", value)

    @property
    def sbrnid(self) -> typing.Optional[int]:
        """Get or set the Retractor node ID
        """ # nopep8
        return self._cards[0].get_value("sbrnid")

    @sbrnid.setter
    def sbrnid(self, value: int) -> None:
        """Set the sbrnid property."""
        self._cards[0].set_value("sbrnid", value)

    @property
    def sbid(self) -> typing.Optional[int]:
        """Get or set the Seat belt element ID
        """ # nopep8
        return self._cards[0].get_value("sbid")

    @sbid.setter
    def sbid(self, value: int) -> None:
        """Set the sbid property."""
        self._cards[0].set_value("sbid", value)

    @property
    def sid1(self) -> int:
        """Get or set the Sensor ID 1
        """ # nopep8
        return self._cards[0].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[0].set_value("sid1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Sensor ID 2
        """ # nopep8
        return self._cards[0].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[0].set_value("sid2", value)

    @property
    def sid3(self) -> int:
        """Get or set the Sensor ID 3
        """ # nopep8
        return self._cards[0].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        """Set the sid3 property."""
        self._cards[0].set_value("sid3", value)

    @property
    def sid4(self) -> int:
        """Get or set the Sensor ID 4
        """ # nopep8
        return self._cards[0].get_value("sid4")

    @sid4.setter
    def sid4(self, value: int) -> None:
        """Set the sid4 property."""
        self._cards[0].set_value("sid4", value)

    @property
    def tdel(self) -> float:
        """Get or set the Time delay after sensor triggers.
        """ # nopep8
        return self._cards[1].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        """Set the tdel property."""
        self._cards[1].set_value("tdel", value)

    @property
    def pull(self) -> float:
        """Get or set the Amount of pull-out between time delay ending and retractor locking, a length value.
        """ # nopep8
        return self._cards[1].get_value("pull")

    @pull.setter
    def pull(self, value: float) -> None:
        """Set the pull property."""
        self._cards[1].set_value("pull", value)

    @property
    def llcid(self) -> int:
        """Get or set the Load curve for loading (Pull-out, Force).
        """ # nopep8
        return self._cards[1].get_value("llcid")

    @llcid.setter
    def llcid(self, value: int) -> None:
        """Set the llcid property."""
        self._cards[1].set_value("llcid", value)

    @property
    def ulcid(self) -> int:
        """Get or set the Load curve for unloading (Pull-out, Force).
        """ # nopep8
        return self._cards[1].get_value("ulcid")

    @ulcid.setter
    def ulcid(self, value: int) -> None:
        """Set the ulcid property."""
        self._cards[1].set_value("ulcid", value)

    @property
    def lfed(self) -> float:
        """Get or set the Fed length.
        """ # nopep8
        return self._cards[1].get_value("lfed")

    @lfed.setter
    def lfed(self, value: float) -> None:
        """Set the lfed property."""
        self._cards[1].set_value("lfed", value)

    @property
    def sbrnid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given sbrnid."""
        return self._get_link_by_attr("NODE", "nid", self.sbrnid, "parts")

    @property
    def llcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for llcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.llcid:
                return kwd
        return None

    @llcid_link.setter
    def llcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for llcid."""
        self.llcid = value.lcid

    @property
    def ulcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ulcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ulcid:
                return kwd
        return None

    @ulcid_link.setter
    def ulcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ulcid."""
        self.ulcid = value.lcid

