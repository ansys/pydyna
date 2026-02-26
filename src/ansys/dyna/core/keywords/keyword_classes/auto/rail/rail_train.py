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

"""Module providing the RailTrain class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_RAILTRAIN_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("nsetid", int, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("finit", float, 30, 10, 0.0),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("trid", int, 50, 10, 0),
    FieldSchema("lcur", int, 60, 10, None),
    FieldSchema("offs", float, 70, 10, 0.0),
)

_RAILTRAIN_CARD1 = (
    FieldSchema("vertstf", float, 0, 10, 0.0),
    FieldSchema("latstf", float, 10, 10, 0.0),
    FieldSchema("v2", float, 20, 10, 0.0),
    FieldSchema("v3", float, 30, 10, 0.0),
    FieldSchema("l2", float, 40, 10, 0.0),
    FieldSchema("l3", float, 50, 10, 0.0),
    FieldSchema("latdir", float, 60, 10, 0.0),
    FieldSchema("fric", float, 70, 10, 0.0),
)

class RailTrain(KeywordBase):
    """DYNA RAIL_TRAIN keyword"""

    keyword = "RAIL"
    subkeyword = "TRAIN"
    _link_fields = {
        "lcur": LinkType.DEFINE_CURVE,
        "nsetid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the RailTrain class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RAILTRAIN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _RAILTRAIN_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Train ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def nsetid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing all nodes that are in contact with rails.
        """ # nopep8
        return self._cards[0].get_value("nsetid")

    @nsetid.setter
    def nsetid(self, value: int) -> None:
        """Set the nsetid property."""
        self._cards[0].set_value("nsetid", value)

    @property
    def finit(self) -> float:
        """Get or set the Estimate of initial vertical force on each wheel (optional)   speeds up the process of initial settling down under gravity loading.
        """ # nopep8
        return self._cards[0].get_value("finit")

    @finit.setter
    def finit(self, value: float) -> None:
        """Set the finit property."""
        self._cards[0].set_value("finit", value)

    @property
    def trid(self) -> int:
        """Get or set the ID of track for this train, see *RAIL_TRACK.
        """ # nopep8
        return self._cards[0].get_value("trid")

    @trid.setter
    def trid(self, value: int) -> None:
        """Set the trid property."""
        self._cards[0].set_value("trid", value)

    @property
    def lcur(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) containing wheel roughness (distance of wheel surface away from perfect circle) vs. distance travelled. The curve does not repeat with each rotation of the wheel   the last point should be at a greater distance than the train is expected to travel. Default: no wheel roughness.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def offs(self) -> float:
        """Get or set the Offset distance used to generate different roughness curves for each wheel from the roughness curve LCUR. The curve is offset on the x axis by a different whole number multiple of OFFS for each wheel.
        """ # nopep8
        return self._cards[0].get_value("offs")

    @offs.setter
    def offs(self, value: float) -> None:
        """Set the offs property."""
        self._cards[0].set_value("offs", value)

    @property
    def vertstf(self) -> float:
        """Get or set the Vertical stiffness of rail contact.
        """ # nopep8
        return self._cards[1].get_value("vertstf")

    @vertstf.setter
    def vertstf(self, value: float) -> None:
        """Set the vertstf property."""
        self._cards[1].set_value("vertstf", value)

    @property
    def latstf(self) -> float:
        """Get or set the Lateral stiffness of rail contact.
        """ # nopep8
        return self._cards[1].get_value("latstf")

    @latstf.setter
    def latstf(self, value: float) -> None:
        """Set the latstf property."""
        self._cards[1].set_value("latstf", value)

    @property
    def v2(self) -> float:
        """Get or set the Unused variables - leave blank.
        """ # nopep8
        return self._cards[1].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[1].set_value("v2", value)

    @property
    def v3(self) -> float:
        """Get or set the Unused variables - leave blank.
        """ # nopep8
        return self._cards[1].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[1].set_value("v3", value)

    @property
    def l2(self) -> float:
        """Get or set the Lateral clearance from rail to wheel rim. Lateral force is applied to a wheel only when it has moved more than L2 away from the other rail, i.e. the wheel rims are assumed to be near the inner face of the rail.
        """ # nopep8
        return self._cards[1].get_value("l2")

    @l2.setter
    def l2(self, value: float) -> None:
        """Set the l2 property."""
        self._cards[1].set_value("l2", value)

    @property
    def l3(self) -> float:
        """Get or set the Further lateral distance before full lateral stiffness applies (force-deflection curve follows a parabola up to this point).
        """ # nopep8
        return self._cards[1].get_value("l3")

    @l3.setter
    def l3(self, value: float) -> None:
        """Set the l3 property."""
        self._cards[1].set_value("l3", value)

    @property
    def latdir(self) -> float:
        """Get or set the Determines the lateral direction (relative to the track) in which wheel movement is resisted by flange contact. If two wheels are fixed to an axle, lateral force is generally applied to one or other of the two wheels, depending on the direction of lateral movement.
        EQ.0.0:	Wheel flanges run on inside faces of rails
        EQ.1.0 : Wheel flanges run on outside faces of rails
        EQ.2.0 : Wheel flanges on both faces of rails(both wheels resist lateral motion in both directions).
        """ # nopep8
        return self._cards[1].get_value("latdir")

    @latdir.setter
    def latdir(self, value: float) -> None:
        """Set the latdir property."""
        self._cards[1].set_value("latdir", value)

    @property
    def fric(self) -> float:
        """Get or set the Coefficient for additional friction force resisting lateral motion of wheel relative to rail.
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[1].set_value("fric", value)

    @property
    def lcur_link(self) -> typing.Optional[DefineCurve]:
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
    def nsetid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsetid."""
        return self._get_set_link("NODE", self.nsetid)

    @nsetid_link.setter
    def nsetid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsetid."""
        self.nsetid = value.sid

