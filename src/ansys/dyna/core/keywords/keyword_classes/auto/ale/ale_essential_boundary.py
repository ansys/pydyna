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

"""Module providing the AleEssentialBoundary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ALEESSENTIALBOUNDARY_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtype", int, 10, 10, 0),
    FieldSchema("ictype", int, 20, 10, 1),
    FieldSchema("iexcl", int, 30, 10, None),
)

class AleEssentialBoundary(KeywordBase):
    """DYNA ALE_ESSENTIAL_BOUNDARY keyword"""

    keyword = "ALE"
    subkeyword = "ESSENTIAL_BOUNDARY"
    _link_fields = {
        "iexcl": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the AleEssentialBoundary class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEESSENTIALBOUNDARY_CARD0,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set or segment set ID of the ALE mesh boundary.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the Type of set ID:
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        EQ.2: segment set ID (SGSID).
        """ # nopep8
        return self._cards[0].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        """Set the idtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""idtype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("idtype", value)

    @property
    def ictype(self) -> int:
        """Get or set the Constraint type:
        EQ.1: No flow through all directions.
        EQ.2: No flow through normal direction. (slip condition)
        """ # nopep8
        return self._cards[0].get_value("ictype")

    @ictype.setter
    def ictype(self, value: int) -> None:
        """Set the ictype property."""
        if value not in [1, 2, None]:
            raise Exception("""ictype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("ictype", value)

    @property
    def iexcl(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.
        """ # nopep8
        return self._cards[0].get_value("iexcl")

    @iexcl.setter
    def iexcl(self, value: int) -> None:
        """Set the iexcl property."""
        self._cards[0].set_value("iexcl", value)

    @property
    def iexcl_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for iexcl."""
        return self._get_set_link("SEGMENT", self.iexcl)

    @iexcl_link.setter
    def iexcl_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for iexcl."""
        self.iexcl = value.sid

