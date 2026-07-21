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

"""Module providing the IcfdDefineSource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDDEFINESOURCE_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("lcidx", int, 10, 10, None),
    FieldSchema("lcidy", int, 20, 10, None),
    FieldSchema("lcidz", int, 30, 10, None),
    FieldSchema("shape", int, 40, 10, 1),
    FieldSchema("r", float, 50, 10, None),
    FieldSchema("pid1", int, 60, 10, None),
    FieldSchema("pid2", int, 70, 10, None),
)

class IcfdDefineSource(KeywordBase):
    """DYNA ICFD_DEFINE_SOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_SOURCE"
    _link_fields = {
        "lcidx": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineSource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINESOURCE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the source ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs specifying the evolution of the volumetric force as a function of time for the three global components
        """ # nopep8
        return self._cards[0].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[0].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs specifying the evolution of the volumetric force as a function of time for the three global components
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs specifying the evolution of the volumetric force as a function of time for the three global components
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[0].set_value("lcidz", value)

    @property
    def shape(self) -> int:
        """Get or set the Shape of the external source:
        EQ.1: Box shape
        EQ.2: Cylinder shape
        EQ.3: Sphere shape
        """ # nopep8
        return self._cards[0].get_value("shape")

    @shape.setter
    def shape(self, value: int) -> None:
        """Set the shape property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""shape must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("shape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the cylinder or sphere is SHAPE=2 or 3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Point ID (see *ICFD_DEFINE_POINT) whose meaning depends on SHAPE:
        SHAPE.EQ.1: Minimum coordinates of the box
        SHAPE.EQ.2: Tail point of the cylinder
        SHAPE.EQ.3: Origin of the sphere
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Point ID (see *ICFD_DEFINE_POINT) whose meaning depends on SHAPE:
        SHAPE.EQ.1: Maximum coordinates of the box
        SHAPE.EQ.2: Head point of the cylinder
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

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

