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

"""Module providing the LoadVolumeLoss class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_LOADVOLUMELOSS_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("coord", int, 10, 10, None),
    FieldSchema("lcur", int, 20, 10, 0),
    FieldSchema("fx", float, 30, 10, 1.0),
    FieldSchema("fy", float, 40, 10, 1.0),
    FieldSchema("fz", float, 50, 10, 1.0),
    FieldSchema("pmin", float, 60, 10, -1e+21),
    FieldSchema("factor", float, 70, 10, 0.01),
)

class LoadVolumeLoss(KeywordBase):
    """DYNA LOAD_VOLUME_LOSS keyword"""

    keyword = "LOAD"
    subkeyword = "VOLUME_LOSS"
    _link_fields = {
        "lcur": LinkType.DEFINE_CURVE,
        "coord": LinkType.DEFINE_COORDINATE_SYSTEM,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadVolumeLoss class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADVOLUMELOSS_CARD0,
                **kwargs,
            ),        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def coord(self) -> typing.Optional[int]:
        """Get or set the Coordinate System ID (default - global coordinate system).
        """ # nopep8
        return self._cards[0].get_value("coord")

    @coord.setter
    def coord(self, value: int) -> None:
        """Set the coord property."""
        self._cards[0].set_value("coord", value)

    @property
    def lcur(self) -> int:
        """Get or set the Curve ID containing volume fraction lost as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def fx(self) -> float:
        """Get or set the Fraction of strain occurring in x-direction.
        """ # nopep8
        return self._cards[0].get_value("fx")

    @fx.setter
    def fx(self, value: float) -> None:
        """Set the fx property."""
        self._cards[0].set_value("fx", value)

    @property
    def fy(self) -> float:
        """Get or set the Fraction of strain occurring in y-direction.
        """ # nopep8
        return self._cards[0].get_value("fy")

    @fy.setter
    def fy(self, value: float) -> None:
        """Set the fy property."""
        self._cards[0].set_value("fy", value)

    @property
    def fz(self) -> float:
        """Get or set the Fraction of strain occurring in z-direction.
        """ # nopep8
        return self._cards[0].get_value("fz")

    @fz.setter
    def fz(self, value: float) -> None:
        """Set the fz property."""
        self._cards[0].set_value("fz", value)

    @property
    def pmin(self) -> float:
        """Get or set the (Leave blank).
        """ # nopep8
        return self._cards[0].get_value("pmin")

    @pmin.setter
    def pmin(self, value: float) -> None:
        """Set the pmin property."""
        self._cards[0].set_value("pmin", value)

    @property
    def factor(self) -> float:
        """Get or set the Feedback factor.
        """ # nopep8
        return self._cards[0].get_value("factor")

    @factor.setter
    def factor(self, value: float) -> None:
        """Set the factor property."""
        self._cards[0].set_value("factor", value)

    @property
    def lcur_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcur."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcur:
                return kwd
        return None

    @lcur_link.setter
    def lcur_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcur."""
        self.lcur = value.lcid

    @property
    def coord_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for coord."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.coord:
                return kwd
        return None

    @coord_link.setter
    def coord_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for coord."""
        self.coord = value.cid

    @property
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

