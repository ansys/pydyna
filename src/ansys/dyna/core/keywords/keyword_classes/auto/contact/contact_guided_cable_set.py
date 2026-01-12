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

"""Module providing the ContactGuidedCableSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTGUIDEDCABLESET_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_CONTACTGUIDEDCABLESET_CARD1 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("psid", int, 10, 10, None),
    FieldSchema("soft", int, 20, 10, 0),
    FieldSchema("ssfac", float, 30, 10, 1.0),
    FieldSchema("fric", float, 40, 10, None),
    FieldSchema("endtol", float, 50, 10, None),
)

class ContactGuidedCableSet(KeywordBase):
    """DYNA CONTACT_GUIDED_CABLE_SET keyword"""

    keyword = "CONTACT"
    subkeyword = "GUIDED_CABLE_SET"

    def __init__(self, **kwargs):
        """Initialize the ContactGuidedCableSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTGUIDEDCABLESET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTGUIDEDCABLESET_CARD1,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact interface ID. This must be a unique number.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID that guides the 1D elements
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID if SET is included in the keyword line
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def soft(self) -> int:
        """Get or set the Flag for soft constraint option.  Set to 1 for soft constraint.
        """ # nopep8
        return self._cards[1].get_value("soft")

    @soft.setter
    def soft(self, value: int) -> None:
        """Set the soft property."""
        self._cards[1].set_value("soft", value)

    @property
    def ssfac(self) -> float:
        """Get or set the Stiffness scale factor for penalty stiffness value.  The default value is unity.  This applies to SOFT set to 0 and 1.
        """ # nopep8
        return self._cards[1].get_value("ssfac")

    @ssfac.setter
    def ssfac(self, value: float) -> None:
        """Set the ssfac property."""
        self._cards[1].set_value("ssfac", value)

    @property
    def fric(self) -> typing.Optional[float]:
        """Get or set the Coefficient of friction.
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[1].set_value("fric", value)

    @property
    def endtol(self) -> typing.Optional[float]:
        """Get or set the Tolerance, in length units, applied at the ends of the cable elements beyond which contact will pass to the next cable element. The default is 0.002 times the element length
        """ # nopep8
        return self._cards[1].get_value("endtol")

    @endtol.setter
    def endtol(self, value: float) -> None:
        """Set the endtol property."""
        self._cards[1].set_value("endtol", value)

