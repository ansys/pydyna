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

"""Module providing the IcfdDefineSptranspsource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDDEFINESPTRANSPSOURCE_CARD0 = (
    FieldSchema("sptrsid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("ishape", int, 20, 10, None),
    FieldSchema("r", float, 30, 10, None),
    FieldSchema("ptid1", int, 40, 10, None),
    FieldSchema("ptid2", int, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("spind", float, 70, 10, None),
)

class IcfdDefineSptranspsource(KeywordBase):
    """DYNA ICFD_DEFINE_SPTRANSPSOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_SPTRANSPSOURCE"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineSptranspsource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINESPTRANSPSOURCE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def sptrsid(self) -> typing.Optional[int]:
        """Get or set the Species transport source ID
        """ # nopep8
        return self._cards[0].get_value("sptrsid")

    @sptrsid.setter
    def sptrsid(self, value: int) -> None:
        """Set the sptrsid property."""
        self._cards[0].set_value("sptrsid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying the evolution of the species source term as a function of time for the X, Y, and Z degrees of freedom (see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, and *DEFINE_FUNCTION). If a *DEFINE_FUNCTION is used, the following parameters are allowed: f(x, y,z, vx, vy, vz, temp, pres, time)
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the Shape of the volumetric species source:
        EQ.1: Box
        EQ.2: Cylinder
        EQ.3: Sphere
        """ # nopep8
        return self._cards[0].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[0].set_value("ishape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the cylinder if ISHAPE = 2 or radius of the sphere if ISHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the ID of a point (see *ICFD_DEFINE_POINT) giving the minimum coordinate of the box if ISHAPE = 1, the tail point for the cylinder if ISHAPE = 2, or the origin of the sphere if ISHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        """Set the ptid1 property."""
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the ID of a point giving the maximum coordinate of the box if ISHAPE = 1 or the head point of the cylinder if ISHAPE = 2.
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        """Set the ptid2 property."""
        self._cards[0].set_value("ptid2", value)

    @property
    def spind(self) -> typing.Optional[float]:
        """Get or set the Index of the species for which the source term applies. For instance, if the source term is for the second species in a multi-species transport problem, this index is 2.
        """ # nopep8
        return self._cards[0].get_value("spind")

    @spind.setter
    def spind(self, value: float) -> None:
        """Set the spind property."""
        self._cards[0].set_value("spind", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

