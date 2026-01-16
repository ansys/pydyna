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

"""Module providing the InitialVelocityRigidBody class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_INITIALVELOCITYRIGIDBODY_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("vx", float, 10, 10, 0.0),
    FieldSchema("vy", float, 20, 10, 0.0),
    FieldSchema("vz", float, 30, 10, 0.0),
    FieldSchema("vxr", float, 40, 10, 0.0),
    FieldSchema("vyr", float, 50, 10, 0.0),
    FieldSchema("vzr", float, 60, 10, 0.0),
    FieldSchema("icid", int, 70, 10, None),
)

class InitialVelocityRigidBody(KeywordBase):
    """DYNA INITIAL_VELOCITY_RIGID_BODY keyword"""

    keyword = "INITIAL"
    subkeyword = "VELOCITY_RIGID_BODY"
    _link_fields = {
        "icid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialVelocityRigidBody class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALVELOCITYRIGIDBODY_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid body.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial translational velocity in x-direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial translational velocity in y-direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial translational velocity in z-direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def vxr(self) -> float:
        """Get or set the Initial rotational velocity about the x-axis.
        """ # nopep8
        return self._cards[0].get_value("vxr")

    @vxr.setter
    def vxr(self, value: float) -> None:
        """Set the vxr property."""
        self._cards[0].set_value("vxr", value)

    @property
    def vyr(self) -> float:
        """Get or set the Initial rotational velocity about the y-axis.
        """ # nopep8
        return self._cards[0].get_value("vyr")

    @vyr.setter
    def vyr(self, value: float) -> None:
        """Set the vyr property."""
        self._cards[0].set_value("vyr", value)

    @property
    def vzr(self) -> float:
        """Get or set the Initial rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("vzr")

    @vzr.setter
    def vzr(self, value: float) -> None:
        """Set the vzr property."""
        self._cards[0].set_value("vzr", value)

    @property
    def icid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID. The specified velocities are in the local system if ICID is greater than zero.
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        """Set the icid property."""
        self._cards[0].set_value("icid", value)

    @property
    def icid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for icid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.icid:
                return kwd
        return None

    @icid_link.setter
    def icid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for icid."""
        self.icid = value.cid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

