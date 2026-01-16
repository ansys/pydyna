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

"""Module providing the ControlFormingAutopositionParameter class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONTROLFORMINGAUTOPOSITIONPARAMETER_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("dir", int, 20, 10, 1),
    FieldSchema("mpid", int, 30, 10, None),
    FieldSchema("position", int, 40, 10, 1),
    FieldSchema("premove", float, 50, 10, None),
    FieldSchema("thick", float, 60, 10, None),
    FieldSchema("porder", str, 70, 10, None),
)

class ControlFormingAutopositionParameter(KeywordBase):
    """DYNA CONTROL_FORMING_AUTOPOSITION_PARAMETER keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_AUTOPOSITION_PARAMETER"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingAutopositionParameter class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGAUTOPOSITIONPARAMETER_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. This part will be moved based on the following controlling parameters.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID set with *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR. The default is the global coordinate system.
        LT.0: | CID | is vector ID giving the direction the part will be moved
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction that the part will be moved
        .EQ.1:  x direction
        .EQ.2:  y direction
        .EQ.3:  z direction
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("dir", value)

    @property
    def mpid(self) -> typing.Optional[int]:
        """Get or set the Part ID. The part (with PID) will be moved based on part defined by MPID.
        """ # nopep8
        return self._cards[0].get_value("mpid")

    @mpid.setter
    def mpid(self, value: int) -> None:
        """Set the mpid property."""
        self._cards[0].set_value("mpid", value)

    @property
    def position(self) -> int:
        """Get or set the .EQ1: means that PID is above MPID	.EQ-1: means that PID is below MPID
        """ # nopep8
        return self._cards[0].get_value("position")

    @position.setter
    def position(self, value: int) -> None:
        """Set the position property."""
        if value not in [1, -1, None]:
            raise Exception("""position must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("position", value)

    @property
    def premove(self) -> typing.Optional[float]:
        """Get or set the PID is moved with a value of PREMOVE. If this parameter is defined, it is unnecessary to define MPID
        """ # nopep8
        return self._cards[0].get_value("premove")

    @premove.setter
    def premove(self, value: float) -> None:
        """Set the premove property."""
        self._cards[0].set_value("premove", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Part thickness
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[0].set_value("thick", value)

    @property
    def porder(self) -> typing.Optional[str]:
        """Get or set the The name of the parameters in the parameter list.
        """ # nopep8
        return self._cards[0].get_value("porder")

    @porder.setter
    def porder(self, value: str) -> None:
        """Set the porder property."""
        self._cards[0].set_value("porder", value)

    @property
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

