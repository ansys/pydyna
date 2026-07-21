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

"""Module providing the ContactAutoMove class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_CONTACTAUTOMOVE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("contid", int, 10, 10, None),
    FieldSchema("vid", int, 20, 10, None),
    FieldSchema("lcid", int, 30, 10, None),
    FieldSchema("atime", float, 40, 10, None),
    FieldSchema("offset", float, 50, 10, 0.0),
)

class ContactAutoMove(KeywordBase):
    """DYNA CONTACT_AUTO_MOVE keyword"""

    keyword = "CONTACT"
    subkeyword = "AUTO_MOVE"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "vid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the ContactAutoMove class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTAUTOMOVE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Move ID for this automatic move input:
        GT.0: Velocity controlled tool kinematics(the variable VAD = 0 in *BOUNDARY_PRESCRIBED_MOTION_RIGID)
        LT.0: Displacement controlled tool kinematics(VAD = 2)
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def contid(self) -> typing.Optional[int]:
        """Get or set the Contact ID, as in *CONTACT_FORMING_..._ID, which defines the surfa and surfb part set IDs.
        """ # nopep8
        return self._cards[0].get_value("contid")

    @contid.setter
    def contid(self, value: int) -> None:
        """Set the contid property."""
        self._cards[0].set_value("contid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID of a vector oriented in the direction of movement of the surfb surface, as in *DEFINE_VECTOR. The origin of the vector is unimportant since the direction cosines of the vector are computed and used
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve defining tooling kinematics, either by velocity as a function of time or by displacement as a function of time. This load curve will be adjusted automatically during a simulation to close the empty tool travel
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def atime(self) -> typing.Optional[float]:
        """Get or set the Activation time specifying the moment the surfb surface (tool) will be moved
        """ # nopep8
        return self._cards[0].get_value("atime")

    @atime.setter
    def atime(self, value: float) -> None:
        """Set the atime property."""
        self._cards[0].set_value("atime", value)

    @property
    def offset(self) -> float:
        """Get or set the Time at which a surfb surface will move to close a gap distance, which may happen following the move of another surfb surface. This is useful for sequential multiple flanging or press hemming simulations. Simulation time (CPU) is much faster based on the shortened tool travel (no change to the termination time).
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[0].set_value("offset", value)

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

    @property
    def vid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

