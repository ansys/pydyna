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

"""Module providing the ElementBeamPulley class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTBEAMPULLEY_CARD0 = (
    FieldSchema("puid", int, 0, 10, 0),
    FieldSchema("bid1", int, 10, 10, 0),
    FieldSchema("bid2", int, 20, 10, 0),
    FieldSchema("pnid", int, 30, 10, 0),
    FieldSchema("fd", float, 40, 10, 0.0),
    FieldSchema("fs", float, 50, 10, 0.0),
    FieldSchema("lmin", float, 60, 10, 0.0),
    FieldSchema("dc", float, 70, 10, 0.0),
)

class ElementBeamPulley(KeywordBase):
    """DYNA ELEMENT_BEAM_PULLEY keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_PULLEY"
    _link_fields = {
        "pnid": LinkType.NODE,
        "bid1": LinkType.ELEMENT_BEAM,
        "bid2": LinkType.ELEMENT_BEAM,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementBeamPulley class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTBEAMPULLEY_CARD0,
                **kwargs,
            ),        ]
    @property
    def puid(self) -> int:
        """Get or set the Pulley ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("puid")

    @puid.setter
    def puid(self, value: int) -> None:
        """Set the puid property."""
        self._cards[0].set_value("puid", value)

    @property
    def bid1(self) -> int:
        """Get or set the Truss beam element 1 ID.
        """ # nopep8
        return self._cards[0].get_value("bid1")

    @bid1.setter
    def bid1(self, value: int) -> None:
        """Set the bid1 property."""
        self._cards[0].set_value("bid1", value)

    @property
    def bid2(self) -> int:
        """Get or set the Truss beam element 2 ID.
        """ # nopep8
        return self._cards[0].get_value("bid2")

    @bid2.setter
    def bid2(self, value: int) -> None:
        """Set the bid2 property."""
        self._cards[0].set_value("bid2", value)

    @property
    def pnid(self) -> int:
        """Get or set the Pulley node, NID.
        """ # nopep8
        return self._cards[0].get_value("pnid")

    @pnid.setter
    def pnid(self, value: int) -> None:
        """Set the pnid property."""
        self._cards[0].set_value("pnid", value)

    @property
    def fd(self) -> float:
        """Get or set the Coulomb dynamic friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[0].set_value("fd", value)

    @property
    def fs(self) -> float:
        """Get or set the Optional Coulomb static friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def lmin(self) -> float:
        """Get or set the Minimum length, see notes below.
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        """Set the lmin property."""
        self._cards[0].set_value("lmin", value)

    @property
    def dc(self) -> float:
        """Get or set the Optional decay constant to allow smooth transition between the static and dynamic friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[0].set_value("dc", value)

    @property
    def pnid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given pnid."""
        return self._get_link_by_attr("NODE", "nid", self.pnid, "parts")

    @property
    def bid1_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given bid1."""
        return self._get_link_by_attr("ELEMENT", "eid", self.bid1, "parts")

    @property
    def bid2_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given bid2."""
        return self._get_link_by_attr("ELEMENT", "eid", self.bid2, "parts")

