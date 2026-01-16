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

"""Module providing the EmIsopotentialRogo class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMISOPOTENTIALROGO_CARD0 = (
    FieldSchema("isoid", int, 0, 10, None),
    FieldSchema("settype", int, 10, 10, 1),
    FieldSchema("setid", int, 20, 10, None),
)

class EmIsopotentialRogo(KeywordBase):
    """DYNA EM_ISOPOTENTIAL_ROGO keyword"""

    keyword = "EM"
    subkeyword = "ISOPOTENTIAL_ROGO"
    _link_fields = {
        "setid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmIsopotentialRogo class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMISOPOTENTIALROGO_CARD0,
                **kwargs,
            ),        ]
    @property
    def isoid(self) -> typing.Optional[int]:
        """Get or set the ID of the Rogo coil.
        """ # nopep8
        return self._cards[0].get_value("isoid")

    @isoid.setter
    def isoid(self, value: int) -> None:
        """Set the isoid property."""
        self._cards[0].set_value("isoid", value)

    @property
    def settype(self) -> int:
        """Get or set the Set type:
        EQ.1:Segment Set.
        """ # nopep8
        return self._cards[0].get_value("settype")

    @settype.setter
    def settype(self, value: int) -> None:
        """Set the settype property."""
        self._cards[0].set_value("settype", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def setid_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for setid."""
        return self._get_set_link("SEGMENT", self.setid)

    @setid_link.setter
    def setid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for setid."""
        self.setid = value.sid

