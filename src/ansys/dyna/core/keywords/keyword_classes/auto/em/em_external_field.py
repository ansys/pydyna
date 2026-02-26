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

"""Module providing the EmExternalField class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMEXTERNALFIELD_CARD0 = (
    FieldSchema("fieldid", int, 0, 10, None),
    FieldSchema("ftype", int, 10, 10, 1),
    FieldSchema("fdef", int, 20, 10, 1),
    FieldSchema("lcidx", int, 30, 10, None),
    FieldSchema("lcidy", int, 40, 10, None),
    FieldSchema("lcidz", int, 50, 10, None),
)

class EmExternalField(KeywordBase):
    """DYNA EM_EXTERNAL_FIELD keyword"""

    keyword = "EM"
    subkeyword = "EXTERNAL_FIELD"
    _link_fields = {
        "lcidx": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmExternalField class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEXTERNALFIELD_CARD0,
                **kwargs,
            ),        ]
    @property
    def fieldid(self) -> typing.Optional[int]:
        """Get or set the External Field ID.
        """ # nopep8
        return self._cards[0].get_value("fieldid")

    @fieldid.setter
    def fieldid(self, value: int) -> None:
        """Set the fieldid property."""
        self._cards[0].set_value("fieldid", value)

    @property
    def ftype(self) -> int:
        """Get or set the Field type:
        EQ.1: Magnetic field.
        EQ.2: Electric field.
        EQ.3: charge density (resistive heating solver only).
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        """Set the ftype property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""ftype must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("ftype", value)

    @property
    def fdef(self) -> int:
        """Get or set the Field defined by:
        EQ.1:Load curves.
        EQ.2: define function (FTYPE = 3 only). If a define function is used, the following parameters are accepted : x, y, z,time, emdt, pot, curr, sigma.
        """ # nopep8
        return self._cards[0].get_value("fdef")

    @fdef.setter
    def fdef(self, value: int) -> None:
        """Set the fdef property."""
        if value not in [1, 2, None]:
            raise Exception("""fdef must be `None` or one of {1,2}.""")
        self._cards[0].set_value("fdef", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the X component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[0].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Y component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Z component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[0].set_value("lcidz", value)

    @property
    def lcidx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidx:
                return kwd
        return None

    @lcidx_link.setter
    def lcidx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidx."""
        self.lcidx = value.lcid

    @property
    def lcidy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidy:
                return kwd
        return None

    @lcidy_link.setter
    def lcidy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidy."""
        self.lcidy = value.lcid

    @property
    def lcidz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidz:
                return kwd
        return None

    @lcidz_link.setter
    def lcidz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidz."""
        self.lcidz = value.lcid

