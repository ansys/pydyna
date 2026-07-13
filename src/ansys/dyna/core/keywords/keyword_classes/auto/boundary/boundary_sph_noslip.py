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

"""Module providing the BoundarySphNoslip class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_BOUNDARYSPHNOSLIP_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtyp", int, 10, 10, 0),
    FieldSchema("cid", int, 20, 10, None),
    FieldSchema("vid", int, 30, 10, None),
    FieldSchema("lcid", int, 40, 10, None),
)

class BoundarySphNoslip(KeywordBase):
    """DYNA BOUNDARY_SPH_NOSLIP keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_NOSLIP"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "vid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySphNoslip class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHNOSLIP_CARD0,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node, nodal set, part, or part set ID. used to identify dummy particles.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def idtyp(self) -> int:
        """Get or set the ID type:
        EQ.0: Node set ID, see *SET_NODE,
        EQ.1: Node ID, see *NODE,
        EQ.2: Part set ID, see *SET_PART,
        EQ.3: Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("idtyp")

    @idtyp.setter
    def idtyp(self, value: int) -> None:
        """Set the idtyp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""idtyp must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("idtyp", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID used to define the components of the velocity vector; see *DEFINE_COORDINATE_SYSTEM for example.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for defining the fictitious velocity applied to no-slip particles; see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the scaling factor for the velocity vector as a function of time; see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

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

