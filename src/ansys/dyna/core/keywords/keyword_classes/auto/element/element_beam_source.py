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

"""Module providing the ElementBeamSource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTBEAMSOURCE_CARD0 = (
    FieldSchema("bsid", int, 0, 10, 0),
    FieldSchema("bsnid", int, 10, 10, 0),
    FieldSchema("bseid", int, 20, 10, 0),
    FieldSchema("nele", int, 30, 10, 0),
    FieldSchema("lfed", float, 40, 10, 0.0),
    FieldSchema("fpull", float, 50, 10, 0.0),
    FieldSchema("lmin", float, 60, 10, 0.0),
)

class ElementBeamSource(KeywordBase):
    """DYNA ELEMENT_BEAM_SOURCE keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_SOURCE"
    _link_fields = {
        "bsnid": LinkType.NODE,
        "bseid": LinkType.ELEMENT_BEAM,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementBeamSource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTBEAMSOURCE_CARD0,
                **kwargs,
            ),        ]
    @property
    def bsid(self) -> int:
        """Get or set the Beam Source ID.  A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        """Set the bsid property."""
        self._cards[0].set_value("bsid", value)

    @property
    def bsnid(self) -> int:
        """Get or set the Source node ID.
        """ # nopep8
        return self._cards[0].get_value("bsnid")

    @bsnid.setter
    def bsnid(self, value: int) -> None:
        """Set the bsnid property."""
        self._cards[0].set_value("bsnid", value)

    @property
    def bseid(self) -> int:
        """Get or set the Source element ID.
        """ # nopep8
        return self._cards[0].get_value("bseid")

    @bseid.setter
    def bseid(self, value: int) -> None:
        """Set the bseid property."""
        self._cards[0].set_value("bseid", value)

    @property
    def nele(self) -> int:
        """Get or set the Number of elements to be pulled out.
        """ # nopep8
        return self._cards[0].get_value("nele")

    @nele.setter
    def nele(self, value: int) -> None:
        """Set the nele property."""
        self._cards[0].set_value("nele", value)

    @property
    def lfed(self) -> float:
        """Get or set the Beam element fed length.
        """ # nopep8
        return self._cards[0].get_value("lfed")

    @lfed.setter
    def lfed(self, value: float) -> None:
        """Set the lfed property."""
        self._cards[0].set_value("lfed", value)

    @property
    def fpull(self) -> float:
        """Get or set the Pull-out force.
        """ # nopep8
        return self._cards[0].get_value("fpull")

    @fpull.setter
    def fpull(self, value: float) -> None:
        """Set the fpull property."""
        self._cards[0].set_value("fpull", value)

    @property
    def lmin(self) -> float:
        """Get or set the Minimum beam element length, see notes below.
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        """Set the lmin property."""
        self._cards[0].set_value("lmin", value)

    @property
    def bsnid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given bsnid."""
        return self._get_link_by_attr("NODE", "nid", self.bsnid, "parts")

    @property
    def bseid_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given bseid."""
        return self._get_link_by_attr("ELEMENT", "eid", self.bseid, "parts")

