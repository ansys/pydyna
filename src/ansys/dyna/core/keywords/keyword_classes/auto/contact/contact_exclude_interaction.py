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
    FieldSchema("sid2", int, 0, 10, None),
    FieldSchema("sid1", int, 10, 10, None),
    FieldSchema("type2", int, 20, 10, 0),
    FieldSchema("type1", int, 30, 10, 0),
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
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTEXCLUDEINTERACTION_CARD1,
                **kwargs,
            ),        ]
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
    def sid2(self) -> typing.Optional[int]:
        """Get or set the Set ID of set 2
        """ # nopep8
        return self._cards[1].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[1].set_value("sid2", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Set ID of set 1
        """ # nopep8
        return self._cards[1].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[1].set_value("sid1", value)

    @property
    def type2(self) -> int:
        """Get or set the ID type of set SID2:
        EQ.0:	Segment set
        EQ.1 : Shell element set
        EQ.2 : Part set
        """ # nopep8
        return self._cards[1].get_value("type2")

    @type2.setter
    def type2(self, value: int) -> None:
        """Set the type2 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""type2 must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("type2", value)

    @property
    def type1(self) -> int:
        """Get or set the ID type of set SID1:
        EQ.0:	Segment set
        EQ.1 : Shell element set
        EQ.2 : Part set
        """ # nopep8
        return self._cards[1].get_value("type1")

    @type1.setter
    def type1(self, value: int) -> None:
        """Set the type1 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""type1 must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("type1", value)

