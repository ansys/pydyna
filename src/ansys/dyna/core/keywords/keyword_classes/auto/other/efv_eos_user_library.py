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

"""Module providing the EfvEosUserLibrary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EFVEOSUSERLIBRARY_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("sesmid", int, 10, 10, None),
)

class EfvEosUserLibrary(KeywordBase):
    """DYNA EFV_EOS_USER_LIBRARY keyword"""

    keyword = "EFV"
    subkeyword = "EOS_USER_LIBRARY"
    _link_fields = {
        "sesmid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the EfvEosUserLibrary class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSUSERLIBRARY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def sesmid(self) -> typing.Optional[int]:
        """Get or set the Material ID from seslib
        """ # nopep8
        return self._cards[0].get_value("sesmid")

    @sesmid.setter
    def sesmid(self, value: int) -> None:
        """Set the sesmid property."""
        self._cards[0].set_value("sesmid", value)

    @property
    def sesmid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for sesmid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.sesmid:
                return kwd
        return None

    @sesmid_link.setter
    def sesmid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for sesmid."""
        self.sesmid = value.mid

