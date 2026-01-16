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

"""Module providing the ConstrainedNodeToNurbsPatchSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDNODETONURBSPATCHSET_CARD0 = (
    FieldSchema("patchid", int, 0, 10, None),
    FieldSchema("nsid", int, 10, 10, None),
    FieldSchema("con", str, 20, 10, "000000"),
    FieldSchema("cid", int, 30, 10, None),
    FieldSchema("sf", float, 40, 10, 1.0),
    FieldSchema("dbflg", int, 50, 10, 0),
)

class ConstrainedNodeToNurbsPatchSet(KeywordBase):
    """DYNA CONSTRAINED_NODE_TO_NURBS_PATCH_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODE_TO_NURBS_PATCH_SET"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedNodeToNurbsPatchSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDNODETONURBSPATCHSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Patch ID.
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def con(self) -> str:
        """Get or set the Constraint parameter for extra node(s) of NSID.  Its definition is same as that of CON2 when CM0=-1 as described in MAT_RIGID.  For example ‘1110’ means constrained z-translation, x-rotation and y-rotation.
        """ # nopep8
        return self._cards[0].get_value("con")

    @con.setter
    def con(self, value: str) -> None:
        """Set the con property."""
        self._cards[0].set_value("con", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for constraint
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def sf(self) -> float:
        """Get or set the Penalty force scale factor for the penalty-based constraint
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def dbflg(self) -> int:
        """Get or set the Discrete beam flag. If CON = 0 and displacement boundary conditions are applied to nodes specified in NSID, then this flag must be set to 1.
        When DBFLG = 1, discrete beam elements are created to connect nodes in NSID to the patch.
        """ # nopep8
        return self._cards[0].get_value("dbflg")

    @dbflg.setter
    def dbflg(self, value: int) -> None:
        """Set the dbflg property."""
        if value not in [0, 1, None]:
            raise Exception("""dbflg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dbflg", value)

    @property
    def cid_link(self) -> DefineCoordinateSystem:
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

