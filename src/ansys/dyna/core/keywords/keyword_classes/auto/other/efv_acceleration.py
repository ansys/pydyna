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

"""Module providing the EfvAcceleration class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EFVACCELERATION_CARD0 = (
    FieldSchema("accx", float, 0, 10, None),
    FieldSchema("accy", float, 10, 10, None),
    FieldSchema("accz", float, 20, 10, None),
    FieldSchema("accl", int, 30, 10, None),
)

class EfvAcceleration(KeywordBase):
    """DYNA EFV_ACCELERATION keyword"""

    keyword = "EFV"
    subkeyword = "ACCELERATION"
    _link_fields = {
        "accl": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EfvAcceleration class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVACCELERATION_CARD0,
                **kwargs,
            ),
        ]
    @property
    def accx(self) -> typing.Optional[float]:
        """Get or set the Constant X-acceleration in the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("accx")

    @accx.setter
    def accx(self, value: float) -> None:
        """Set the accx property."""
        self._cards[0].set_value("accx", value)

    @property
    def accy(self) -> typing.Optional[float]:
        """Get or set the Constant Y-acceleration in the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("accy")

    @accy.setter
    def accy(self, value: float) -> None:
        """Set the accy property."""
        self._cards[0].set_value("accy", value)

    @property
    def accz(self) -> typing.Optional[float]:
        """Get or set the Constant Z-acceleration in the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("accz")

    @accz.setter
    def accz(self, value: float) -> None:
        """Set the accz property."""
        self._cards[0].set_value("accz", value)

    @property
    def accl(self) -> typing.Optional[int]:
        """Get or set the ID of a curve specified with *DEFINE_CURVE for time dependent accelerations. These magnitudes are scaled by ACCX, ACCY and ACCZ to orient the acceleration.
        """ # nopep8
        return self._cards[0].get_value("accl")

    @accl.setter
    def accl(self, value: int) -> None:
        """Set the accl property."""
        self._cards[0].set_value("accl", value)

    @property
    def accl_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for accl."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.accl:
                return kwd
        return None

    @accl_link.setter
    def accl_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for accl."""
        self.accl = value.lcid

