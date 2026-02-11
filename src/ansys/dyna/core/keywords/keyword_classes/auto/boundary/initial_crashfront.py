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

"""Module providing the InitialCrashfront class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALCRASHFRONT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, None),
)

class InitialCrashfront(KeywordBase):
    """DYNA INITIAL_CRASHFRONT keyword"""

    keyword = "INITIAL"
    subkeyword = "CRASHFRONT"
    _link_fields = {
        "stype": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialCrashfront class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALCRASHFRONT_CARD0,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID from which the initial crashfront nodes are defined
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> typing.Optional[int]:
        """Get or set the ID type of SID:
        EQ.0:	segment set ID,
        EQ.1 : shell element set ID,
        EQ.2 : part set ID,
        EQ.3 : part ID,
        EQ.4 : node set ID..
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        self._cards[0].set_value("stype", value)

    @property
    def stype_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given stype."""
        return self._get_link_by_attr("NODE", "nid", self.stype, "parts")

