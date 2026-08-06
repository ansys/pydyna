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

"""Module providing the EmSolverBemReassembly class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMSOLVERBEMREASSEMBLY_CARD0 = (
    FieldSchema("ioct", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_EMSOLVERBEMREASSEMBLY_CARD1 = (
    FieldSchema("psid", int, 0, 10, None),
)

class EmSolverBemReassembly(KeywordBase):
    """DYNA EM_SOLVER_BEM_REASSEMBLY keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_BEM_REASSEMBLY"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmSolverBemReassembly class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMSOLVERBEMREASSEMBLY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMSOLVERBEMREASSEMBLY_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ioct(self) -> int:
        """Get or set the Octree domain decomposition method.
        EQ.0: BEM faces are grouped in domains based on their proximity.
        EQ.1 : Same as 0 except the BEM faces from a given domain must belong to the same part.In addition, portions of the BEM matrix can be stored by specifying the parts for storage in PSID on Card 2.
        """ # nopep8
        return self._cards[0].get_value("ioct")

    @ioct.setter
    def ioct(self, value: int) -> None:
        """Set the ioct property."""
        if value not in [0, 1, None]:
            raise Exception("""ioct must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ioct", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part sets IDs for which BEM information is kept. Ignored for IOCT=0. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

