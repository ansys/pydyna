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

"""Module providing the EmContact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMCONTACT_CARD0 = (
    FieldSchema("contid", int, 0, 10, None),
    FieldSchema("dtype", int, 10, 10, 0),
    FieldSchema("psidr", int, 20, 10, None),
    FieldSchema("psidt", int, 30, 10, None),
    FieldSchema("eps1", float, 40, 10, 0.3),
    FieldSchema("eps2", float, 50, 10, 0.3),
    FieldSchema("eps3", float, 60, 10, 0.3),
    FieldSchema("d0", float, 70, 10, None),
)

class EmContact(KeywordBase):
    """DYNA EM_CONTACT keyword"""

    keyword = "EM"
    subkeyword = "CONTACT"
    _link_fields = {
        "psidr": LinkType.SET_PART,
        "psidt": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmContact class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTACT_CARD0,
                **kwargs,
            ),
        ]
    @property
    def contid(self) -> typing.Optional[int]:
        """Get or set the Electromagnetic contact ID.
        """ # nopep8
        return self._cards[0].get_value("contid")

    @contid.setter
    def contid(self, value: int) -> None:
        """Set the contid property."""
        self._cards[0].set_value("contid", value)

    @property
    def dtype(self) -> int:
        """Get or set the Detection type
        EQ.0: Contact type 0 (default)
        EQ.1: Contact type 1.
        """ # nopep8
        return self._cards[0].get_value("dtype")

    @dtype.setter
    def dtype(self, value: int) -> None:
        """Set the dtype property."""
        if value not in [0, 1, None]:
            raise Exception("""dtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dtype", value)

    @property
    def psidr(self) -> typing.Optional[int]:
        """Get or set the Reference surface part set s ID.
        """ # nopep8
        return self._cards[0].get_value("psidr")

    @psidr.setter
    def psidr(self, value: int) -> None:
        """Set the psidr property."""
        self._cards[0].set_value("psidr", value)

    @property
    def psidt(self) -> typing.Optional[int]:
        """Get or set the Tracked surface part set ID.
        """ # nopep8
        return self._cards[0].get_value("psidt")

    @psidt.setter
    def psidt(self, value: int) -> None:
        """Set the psidt property."""
        self._cards[0].set_value("psidt", value)

    @property
    def eps1(self) -> float:
        """Get or set the Contact coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[0].set_value("eps1", value)

    @property
    def eps2(self) -> float:
        """Get or set the Contact coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[0].set_value("eps2", value)

    @property
    def eps3(self) -> float:
        """Get or set the Contact coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[0].set_value("eps3", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Contact condition 3 when COTYPE = 1.
        """ # nopep8
        return self._cards[0].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        """Set the d0 property."""
        self._cards[0].set_value("d0", value)

    @property
    def psidr_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psidr."""
        return self._get_set_link("PART", self.psidr)

    @psidr_link.setter
    def psidr_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psidr."""
        self.psidr = value.sid

    @property
    def psidt_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psidt."""
        return self._get_set_link("PART", self.psidt)

    @psidt_link.setter
    def psidt_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psidt."""
        self.psidt = value.sid

