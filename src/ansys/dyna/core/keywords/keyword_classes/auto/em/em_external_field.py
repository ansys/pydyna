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

_EMEXTERNALFIELD_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("lcidpx", int, 30, 10, None),
    FieldSchema("lcidpy", int, 40, 10, None),
    FieldSchema("lcidpz", int, 50, 10, None),
)

_EMEXTERNALFIELD_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("lcidxi", int, 30, 10, None),
    FieldSchema("lcidyi", int, 40, 10, None),
    FieldSchema("lcidzi", int, 50, 10, None),
)

class EmExternalField(KeywordBase):
    """DYNA EM_EXTERNAL_FIELD keyword"""

    keyword = "EM"
    subkeyword = "EXTERNAL_FIELD"
    _link_fields = {
        "lcidx": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
        "lcidpx": LinkType.DEFINE_CURVE,
        "lcidpy": LinkType.DEFINE_CURVE,
        "lcidpz": LinkType.DEFINE_CURVE,
        "lcidxi": LinkType.DEFINE_CURVE,
        "lcidyi": LinkType.DEFINE_CURVE,
        "lcidzi": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmExternalField class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEXTERNALFIELD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMEXTERNALFIELD_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMEXTERNALFIELD_CARD2,
                **kwargs,
            ),
        ]
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
        EQ.2: Electric field(not available yet).
        EQ.3: Charge density (resistive heating solver only, EMSOL=3).
        EQ.4: Planar Electric field wave defined by direction and polarization(Helmholtz wave equation solver only, EMSOL = 7)..
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        """Set the ftype property."""
        if value not in [1, 3, 4, None]:
            raise Exception("""ftype must be `None` or one of {1,3,4}.""")
        self._cards[0].set_value("ftype", value)

    @property
    def fdef(self) -> int:
        """Get or set the Field defined by:
        EQ.1: Load curves
        EQ.2: Define function(see *DEFINE_FUNCTION).If a define function is used, the following parameters are accepted: x, y, z, time.See Remark 2. For FTYPE = 3, the additional following parameters are accepted: emdt, pot, curr, sigma.
        EQ.4: Subroutine usermat_getextfield in the usermat package.See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("fdef")

    @fdef.setter
    def fdef(self, value: int) -> None:
        """Set the fdef property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""fdef must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("fdef", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the X component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.. For FTYPE=4, this represents the planar wave direction.
        """ # nopep8
        return self._cards[0].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[0].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Y component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.. For FTYPE=4, this represents the planar wave direction.
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Z component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.. For FTYPE=4, this represents the planar wave direction.
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[0].set_value("lcidz", value)

    @property
    def lcidpx(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[1].get_value("lcidpx")

    @lcidpx.setter
    def lcidpx(self, value: int) -> None:
        """Set the lcidpx property."""
        self._cards[1].set_value("lcidpx", value)

    @property
    def lcidpy(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[1].get_value("lcidpy")

    @lcidpy.setter
    def lcidpy(self, value: int) -> None:
        """Set the lcidpy property."""
        self._cards[1].set_value("lcidpy", value)

    @property
    def lcidpz(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[1].get_value("lcidpz")

    @lcidpz.setter
    def lcidpz(self, value: int) -> None:
        """Set the lcidpz property."""
        self._cards[1].set_value("lcidpz", value)

    @property
    def lcidxi(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[2].get_value("lcidxi")

    @lcidxi.setter
    def lcidxi(self, value: int) -> None:
        """Set the lcidxi property."""
        self._cards[2].set_value("lcidxi", value)

    @property
    def lcidyi(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[2].get_value("lcidyi")

    @lcidyi.setter
    def lcidyi(self, value: int) -> None:
        """Set the lcidyi property."""
        self._cards[2].set_value("lcidyi", value)

    @property
    def lcidzi(self) -> typing.Optional[int]:
        """Get or set the Only available for EMSOL=4 in *EM_CONTROL. Load curve ID defining the (X,Y,Z) component of the optional phase shift function of time for FTYPE = 1. Unit is in degrees.For FTYPE=4, this represents the planar wave polarization.
        """ # nopep8
        return self._cards[2].get_value("lcidzi")

    @lcidzi.setter
    def lcidzi(self, value: int) -> None:
        """Set the lcidzi property."""
        self._cards[2].set_value("lcidzi", value)

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

    @property
    def lcidpx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidpx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidpx:
                return kwd
        return None

    @lcidpx_link.setter
    def lcidpx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidpx."""
        self.lcidpx = value.lcid

    @property
    def lcidpy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidpy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidpy:
                return kwd
        return None

    @lcidpy_link.setter
    def lcidpy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidpy."""
        self.lcidpy = value.lcid

    @property
    def lcidpz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidpz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidpz:
                return kwd
        return None

    @lcidpz_link.setter
    def lcidpz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidpz."""
        self.lcidpz = value.lcid

    @property
    def lcidxi_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidxi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidxi:
                return kwd
        return None

    @lcidxi_link.setter
    def lcidxi_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidxi."""
        self.lcidxi = value.lcid

    @property
    def lcidyi_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidyi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidyi:
                return kwd
        return None

    @lcidyi_link.setter
    def lcidyi_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidyi."""
        self.lcidyi = value.lcid

    @property
    def lcidzi_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidzi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidzi:
                return kwd
        return None

    @lcidzi_link.setter
    def lcidzi_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidzi."""
        self.lcidzi = value.lcid

