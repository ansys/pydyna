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

"""Module providing the EmContactResistance class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCONTACTRESISTANCE_CARD0 = (
    FieldSchema("crid", int, 0, 10, None),
    FieldSchema("contid", int, 10, 10, None),
    FieldSchema("ctype", int, 20, 10, 1),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("jhrtype", int, 40, 10, 0),
)

_EMCONTACTRESISTANCE_CARD1 = (
    FieldSchema("dfid", int, 0, 10, None),
)

class EmContactResistance(KeywordBase):
    """DYNA EM_CONTACT_RESISTANCE keyword"""

    keyword = "EM"
    subkeyword = "CONTACT_RESISTANCE"

    def __init__(self, **kwargs):
        """Initialize the EmContactResistance class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTACTRESISTANCE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMCONTACTRESISTANCE_CARD1,
                **kwargs,
            ),        ]
    @property
    def crid(self) -> typing.Optional[int]:
        """Get or set the Resistive contact ID.
        """ # nopep8
        return self._cards[0].get_value("crid")

    @crid.setter
    def crid(self, value: int) -> None:
        """Set the crid property."""
        self._cards[0].set_value("crid", value)

    @property
    def contid(self) -> typing.Optional[int]:
        """Get or set the EM contact ID defined in *EM_CONTACT.
        """ # nopep8
        return self._cards[0].get_value("contid")

    @contid.setter
    def contid(self, value: int) -> None:
        """Set the contid property."""
        self._cards[0].set_value("contid", value)

    @property
    def ctype(self) -> int:
        """Get or set the Contact Resistance type :
        EQ.1: Contact resistance defined by user defined load curve.
        EQ.2: Classic Holm's formula for contact resistances (See Remark 1).
        EQ.3: Modified contact resistance for cases with plastic deformation in the contact area (See Remarks 2 and 3).
        EQ.4: Modified contact resistance for cases with elasticdeformation in the contact area (See Remarks 2 and 3).
        EQ.5: Basic contact resistance definition (See Remark 4).
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""ctype must be `None` or one of {1,2,3,4,5}.""")
        self._cards[0].set_value("ctype", value)

    @property
    def jhrtype(self) -> int:
        """Get or set the Indicates how the Joule heating calculated by the contact resistance shall be taken into account:
        EQ.0: No addition: The Joule heating calculated by the contact resistance is not taken into account.
        EQ.1: The Joule heating coming from the contact resistance is divided and distributed evenly among all elements neighboring the contact surface.
        """ # nopep8
        return self._cards[0].get_value("jhrtype")

    @jhrtype.setter
    def jhrtype(self, value: int) -> None:
        """Set the jhrtype property."""
        if value not in [0, 1, None]:
            raise Exception("""jhrtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("jhrtype", value)

    @property
    def dfid(self) -> typing.Optional[int]:
        """Get or set the Load Function ID defining the contact resistance.
        """ # nopep8
        return self._cards[1].get_value("dfid")

    @dfid.setter
    def dfid(self, value: int) -> None:
        """Set the dfid property."""
        self._cards[1].set_value("dfid", value)

