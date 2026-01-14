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

"""Module providing the IcfdBoundaryFswave class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDBOUNDARYFSWAVE_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("wtype", int, 10, 10, None),
    FieldSchema("h0", float, 20, 10, None),
    FieldSchema("wamp", float, 30, 10, None),
    FieldSchema("wleng", float, 40, 10, None),
    FieldSchema("wmax", float, 50, 10, None),
    FieldSchema("sflcid", int, 60, 10, None),
    FieldSchema("wang", float, 70, 10, None),
)

_ICFDBOUNDARYFSWAVE_CARD1 = (
    FieldSchema("wpeak", float, 0, 10, None),
)

class IcfdBoundaryFswave(KeywordBase):
    """DYNA ICFD_BOUNDARY_FSWAVE keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_FSWAVE"
    _link_fields = {
        "sflcid": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryFswave class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYFSWAVE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYFSWAVE_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def wtype(self) -> typing.Optional[int]:
        """Get or set the Wave Type:
        EQ.1:Stokes wave of first order.
        EQ.2:	Stokes wave of second order
        EQ.3:	Stokes wave of fifth order
        EQ.4 : Solitary wave
        EQ.5 : Irregular waves using JONSWAP spectrum
        EQ.6 : Irregular waves using One Parameter Pierson - Moskowitz spectrum
        EQ.7 : Irregular waves using Two Parameter Pierson - Moskowitz spectrum
        """ # nopep8
        return self._cards[0].get_value("wtype")

    @wtype.setter
    def wtype(self, value: int) -> None:
        """Set the wtype property."""
        self._cards[0].set_value("wtype", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Water level (from the bottom of the channel) for the unperturbed condition
        """ # nopep8
        return self._cards[0].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        """Set the h0 property."""
        self._cards[0].set_value("h0", value)

    @property
    def wamp(self) -> typing.Optional[float]:
        """Get or set the Wave amplitude or height for WTYPE=1 to WTYPE=4. Significant wave height for WTYPE=5,6,7.
        """ # nopep8
        return self._cards[0].get_value("wamp")

    @wamp.setter
    def wamp(self, value: float) -> None:
        """Set the wamp property."""
        self._cards[0].set_value("wamp", value)

    @property
    def wleng(self) -> typing.Optional[float]:
        """Get or set the Wave Length for WTYPE=1 and WTYPE=2. Wave Period for WTYPE=3. Not used for WTYPE=4. Minimum wave frequency in spectrum (rad/sec) for WTYPE=5,6,7
        """ # nopep8
        return self._cards[0].get_value("wleng")

    @wleng.setter
    def wleng(self, value: float) -> None:
        """Set the wleng property."""
        self._cards[0].set_value("wleng", value)

    @property
    def wmax(self) -> typing.Optional[float]:
        """Get or set the Maximum wave frequency in spectrum (rad/sec) for WTYPE = 5, 6, and 7. Angle between the boundary and the incident waves (in degrees) for WTYPE = 3.
        """ # nopep8
        return self._cards[0].get_value("wmax")

    @wmax.setter
    def wmax(self, value: float) -> None:
        """Set the wmax property."""
        self._cards[0].set_value("wmax", value)

    @property
    def sflcid(self) -> typing.Optional[int]:
        """Get or set the Scale factor LCID on the wave amplitude for WTYPE=1, WTYPE=2 and WTYPE=3. Number of Wave modes (default=1024) for WTYPE=5,6,7
        """ # nopep8
        return self._cards[0].get_value("sflcid")

    @sflcid.setter
    def sflcid(self, value: int) -> None:
        """Set the sflcid property."""
        self._cards[0].set_value("sflcid", value)

    @property
    def wang(self) -> typing.Optional[float]:
        """Get or set the Angle between incoming wave direction and x-axis for z and y-aligned gravity vector, or angle between incoming wave direction and y-axis for x-aligned gravity vector.).
        """ # nopep8
        return self._cards[0].get_value("wang")

    @wang.setter
    def wang(self, value: float) -> None:
        """Set the wang property."""
        self._cards[0].set_value("wang", value)

    @property
    def wpeak(self) -> typing.Optional[float]:
        """Get or set the Peak wave frequency in spectrum [rad/sec] for wtype=7. For wtype=6, WPEAK= 0.4*sqrt(g/Hs)) with g the gravity and Hs the significant wave height
        """ # nopep8
        return self._cards[1].get_value("wpeak")

    @wpeak.setter
    def wpeak(self, value: float) -> None:
        """Set the wpeak property."""
        self._cards[1].set_value("wpeak", value)

    @property
    def sflcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for sflcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.sflcid:
                return kwd
        return None

    @sflcid_link.setter
    def sflcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for sflcid."""
        self.sflcid = value.lcid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

