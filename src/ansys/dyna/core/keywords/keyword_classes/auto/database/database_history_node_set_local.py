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

"""Module providing the DatabaseHistoryNodeSetLocal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DATABASEHISTORYNODESETLOCAL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("ref", int, 20, 10, 0),
    FieldSchema("hfo", int, 30, 10, 0),
)

class DatabaseHistoryNodeSetLocal(KeywordBase):
    """DYNA DATABASE_HISTORY_NODE_SET_LOCAL keyword"""

    keyword = "DATABASE"
    subkeyword = "HISTORY_NODE_SET_LOCAL"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "id": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseHistoryNodeSetLocal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEHISTORYNODESETLOCAL_CARD0,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node set ID. The contents of the files are given in Table 9.1 in the Keyword Manual section 9.14 for nodes.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for nodal output. See *DEFINE_COORDINATE options.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def ref(self) -> int:
        """Get or set the Output reference:
        EQ.0: Output is in the local system fixed for all time from the beginning of the calculation (default),
        EQ.1: Output is in the local system which is defined by the *DEFINE_COORDINATE_NODES. The local system can change orientation depending on the movement of the three defining nodes. The defining nodes can belong to either deformable or rigid parts,
        EQ.2: Output is relative to the local system which is defined by the *DEFINE_COORDINATE_NODES option. The local system can change orientation depending on the movement of the three defining nodes. If dynamic relaxation is used, the reference location is reset when convergence is achieved.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: int) -> None:
        """Set the ref property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ref must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ref", value)

    @property
    def hfo(self) -> int:
        """Get or set the Flag for high frequency output into NODOUTHF.
        EQ.0: Nodal data written to NODOUT file only,
        EQ.1: Nodal data also written to NODOUTHF at the higher frequency.
        """ # nopep8
        return self._cards[0].get_value("hfo")

    @hfo.setter
    def hfo(self, value: int) -> None:
        """Set the hfo property."""
        if value not in [0, 1, None]:
            raise Exception("""hfo must be `None` or one of {0,1}.""")
        self._cards[0].set_value("hfo", value)

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
    def id_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for id."""
        return self._get_set_link("NODE", self.id)

    @id_link.setter
    def id_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for id."""
        self.id = value.sid

