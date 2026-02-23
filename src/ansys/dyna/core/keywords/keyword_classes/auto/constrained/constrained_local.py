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

"""Module providing the ConstrainedLocal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDLOCAL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", int, 10, 70, None),
)

_CONSTRAINEDLOCAL_CARD1 = (
    FieldSchema("tc", int, 0, 10, 1),
    FieldSchema("rc", int, 10, 10, 1),
    FieldSchema("dir", int, 20, 10, 1),
    FieldSchema("x", float, 30, 10, None),
    FieldSchema("y", float, 40, 10, None),
    FieldSchema("z", float, 50, 10, None),
    FieldSchema("cid", int, 60, 10, None),
    FieldSchema("tol", float, 70, 10, 0.0),
)

class ConstrainedLocal(KeywordBase):
    """DYNA CONSTRAINED_LOCAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "LOCAL"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedLocal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDLOCAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDLOCAL_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional ID which can be referred to by *SENSOR_CONTROL.
        This ID must be unique and cannot be shared with * BOUNDARY_SPC.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[int]:
        """Get or set the An optional descriptor that will be written into the d3hsp file and the spcforc file.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: int) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def tc(self) -> int:
        """Get or set the Translational Constraint:
        EQ.1: constrained x translation,
        EQ.2: constrained y translation
        EQ.3: constrained z translation,
        EQ.4: constrained x and y translation,
        EQ.5: constrained y and z translation,
        EQ.6: constrained z and x translation,
        EQ.7: constrained x,y and z translation.
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: int) -> None:
        """Set the tc property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""tc must be `None` or one of {1,2,3,4,5,6,7}.""")
        self._cards[1].set_value("tc", value)

    @property
    def rc(self) -> int:
        """Get or set the Rotaional Constraint:
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotaion
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x,y and z rotations.
        """ # nopep8
        return self._cards[1].get_value("rc")

    @rc.setter
    def rc(self, value: int) -> None:
        """Set the rc property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""rc must be `None` or one of {1,2,3,4,5,6,7}.""")
        self._cards[1].set_value("rc", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction of normal
        EQ.1:local x,
        EQ.2: local y,
        EQ.3:local z
        """ # nopep8
        return self._cards[1].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("dir", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Local x-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Local y-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Local z-coordinate of a point on the local constraint plane
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for orientation of the local coordinate system
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def tol(self) -> float:
        """Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[1].set_value("tol", value)

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
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

