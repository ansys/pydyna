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

"""Module providing the DualcesePointSourceStructnodeSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_DUALCESEPOINTSOURCESTRUCTNODESET_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
)

_DUALCESEPOINTSOURCESTRUCTNODESET_CARD1 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("vid", int, 10, 10, None),
    FieldSchema("orifa", float, 20, 10, 1.0),
)

class DualcesePointSourceStructnodeSet(KeywordBase):
    """DYNA DUALCESE_POINT_SOURCE_STRUCTNODE_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "POINT_SOURCE_STRUCTNODE_SET"
    _link_fields = {
        "nid": LinkType.NODE,
        "vid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the DualcesePointSourceStructnodeSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSOURCESTRUCTNODESET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSOURCESTRUCTNODESET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the ID of this set of orifice definitions
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Structural node that gives the position of the orifice.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the ID of a vector defined by *DEFINE_VECTOR that gives the direction of the flow away from the position defined by NID
        """ # nopep8
        return self._cards[1].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[1].set_value("vid", value)

    @property
    def orifa(self) -> float:
        """Get or set the Surface area of the orifice.
        """ # nopep8
        return self._cards[1].get_value("orifa")

    @orifa.setter
    def orifa(self, value: float) -> None:
        """Set the orifa property."""
        self._cards[1].set_value("orifa", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def vid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

