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

"""Module providing the InitialLagMappingWrite class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INITIALLAGMAPPINGWRITE_CARD0 = (
    FieldSchema("setid", int, 0, 10, None),
)

class InitialLagMappingWrite(KeywordBase):
    """DYNA INITIAL_LAG_MAPPING_WRITE keyword"""

    keyword = "INITIAL"
    subkeyword = "LAG_MAPPING_WRITE"
    _link_fields = {
        "setid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialLagMappingWrite class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALLAGMAPPINGWRITE_CARD0,
                **kwargs,
            ),        ]
    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the part set ID
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def setid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for setid."""
        return self._get_set_link("PART", self.setid)

    @setid_link.setter
    def setid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for setid."""
        self.setid = value.sid

