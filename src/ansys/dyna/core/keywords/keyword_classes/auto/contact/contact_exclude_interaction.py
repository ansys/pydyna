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

"""Module providing the ContactExcludeInteraction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTEXCLUDEINTERACTION_CARD0 = (
    FieldSchema("ceid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
)

_CONTACTEXCLUDEINTERACTION_CARD1 = (
    FieldSchema("sida", int, 0, 10, None),
    FieldSchema("sidb", int, 10, 10, None),
    FieldSchema("typea", int, 20, 10, 0),
    FieldSchema("typeb", int, 30, 10, 0),
)

class ContactExcludeInteraction(KeywordBase):
    """DYNA CONTACT_EXCLUDE_INTERACTION keyword"""

    keyword = "CONTACT"
    subkeyword = "EXCLUDE_INTERACTION"

    def __init__(self, **kwargs):
        """Initialize the ContactExcludeInteraction class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTEXCLUDEINTERACTION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTACTEXCLUDEINTERACTION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ceid(self) -> typing.Optional[int]:
        """Get or set the Contact exclusion ID for output only
        """ # nopep8
        return self._cards[0].get_value("ceid")

    @ceid.setter
    def ceid(self, value: int) -> None:
        """Set the ceid property."""
        self._cards[0].set_value("ceid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact interface ID for limiting exclusions to an interface
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def sida(self) -> typing.Optional[int]:
        """Get or set the Set ID of set A
        """ # nopep8
        return self._cards[1].get_value("sida")

    @sida.setter
    def sida(self, value: int) -> None:
        """Set the sida property."""
        self._cards[1].set_value("sida", value)

    @property
    def sidb(self) -> typing.Optional[int]:
        """Get or set the Set ID of set B
        """ # nopep8
        return self._cards[1].get_value("sidb")

    @sidb.setter
    def sidb(self, value: int) -> None:
        """Set the sidb property."""
        self._cards[1].set_value("sidb", value)

    @property
    def typea(self) -> int:
        """Get or set the ID type of set SIDA:
        EQ.0: Segment set
        EQ.1: Shell element set
        EQ.2: Part set
        """ # nopep8
        return self._cards[1].get_value("typea")

    @typea.setter
    def typea(self, value: int) -> None:
        """Set the typea property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""typea must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("typea", value)

    @property
    def typeb(self) -> int:
        """Get or set the ID type of set SIDB:
        EQ.0: Segment set
        EQ.1: Shell element set
        EQ.2: Part set
        """ # nopep8
        return self._cards[1].get_value("typeb")

    @typeb.setter
    def typeb(self, value: int) -> None:
        """Set the typeb property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""typeb must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("typeb", value)

