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

"""Module providing the ConstrainedRigidBodyInsert class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDRIGIDBODYINSERT_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("pidl", int, 10, 10, None),
    FieldSchema("pidc", int, 20, 10, None),
    FieldSchema("coordid", int, 30, 10, None),
    FieldSchema("idir", int, 40, 10, 3),
)

_CONSTRAINEDRIGIDBODYINSERT_CARD1 = (
    FieldSchema("mflag", int, 0, 10, 0),
    FieldSchema("mcid", int, 10, 10, None),
    FieldSchema("deathm", float, 20, 10, 0.0),
)

_CONSTRAINEDRIGIDBODYINSERT_CARD2 = (
    FieldSchema("partb", int, 0, 10, None),
    FieldSchema("deathb", float, 10, 10, 0.0),
)

class ConstrainedRigidBodyInsert(KeywordBase):
    """DYNA CONSTRAINED_RIGID_BODY_INSERT keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "RIGID_BODY_INSERT"
    _link_fields = {
        "mcid": LinkType.DEFINE_CURVE,
        "coordid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pidl": LinkType.PART,
        "pidc": LinkType.PART,
        "partb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedRigidBodyInsert class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDRIGIDBODYINSERT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDRIGIDBODYINSERT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDRIGIDBODYINSERT_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Insert ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def pidl(self) -> typing.Optional[int]:
        """Get or set the Lead (die) rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidl")

    @pidl.setter
    def pidl(self, value: int) -> None:
        """Set the pidl property."""
        self._cards[0].set_value("pidl", value)

    @property
    def pidc(self) -> typing.Optional[int]:
        """Get or set the Constraned (die insert) rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidc")

    @pidc.setter
    def pidc(self, value: int) -> None:
        """Set the pidc property."""
        self._cards[0].set_value("pidc", value)

    @property
    def coordid(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID. The x direction is the direction the insert moves independently of the die.
        """ # nopep8
        return self._cards[0].get_value("coordid")

    @coordid.setter
    def coordid(self, value: int) -> None:
        """Set the coordid property."""
        self._cards[0].set_value("coordid", value)

    @property
    def idir(self) -> int:
        """Get or set the Direction in which the insert moves independently of the die:
        EQ.1:	Local x - direction
        EQ.2 : Local y - direction
        EQ.3 : Local z - direction(default)
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        if value not in [3, 0, 1, 2, None]:
            raise Exception("""idir must be `None` or one of {3,0,1,2}.""")
        self._cards[0].set_value("idir", value)

    @property
    def mflag(self) -> int:
        """Get or set the Motion flag.
        EQ.0:	Relative motion is unconstrained.
        EQ.1:	The displacement of the insert relative to the die is imposed.
        EQ.2:	The velocity of the insert relative to the die is imposed.
        EQ.3:	The acceleration of the insert relative to the die is imposed..
        """ # nopep8
        return self._cards[1].get_value("mflag")

    @mflag.setter
    def mflag(self, value: int) -> None:
        """Set the mflag property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""mflag must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("mflag", value)

    @property
    def mcid(self) -> typing.Optional[int]:
        """Get or set the Curve defining the motion of the die insert relative to the die.
        """ # nopep8
        return self._cards[1].get_value("mcid")

    @mcid.setter
    def mcid(self, value: int) -> None:
        """Set the mcid property."""
        self._cards[1].set_value("mcid", value)

    @property
    def deathm(self) -> float:
        """Get or set the Death time of the imposed motion. If it is equal to 0.0, the motion is imposed for the entire analysis.
        """ # nopep8
        return self._cards[1].get_value("deathm")

    @deathm.setter
    def deathm(self, value: float) -> None:
        """Set the deathm property."""
        self._cards[1].set_value("deathm", value)

    @property
    def partb(self) -> typing.Optional[int]:
        """Get or set the Part ID for a discrete beam connected between the insert and die.
        """ # nopep8
        return self._cards[2].get_value("partb")

    @partb.setter
    def partb(self, value: int) -> None:
        """Set the partb property."""
        self._cards[2].set_value("partb", value)

    @property
    def deathb(self) -> float:
        """Get or set the Death time for the discrete beam specified by BPART.
        """ # nopep8
        return self._cards[2].get_value("deathb")

    @deathb.setter
    def deathb(self, value: float) -> None:
        """Set the deathb property."""
        self._cards[2].set_value("deathb", value)

    @property
    def mcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for mcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.mcid:
                return kwd
        return None

    @mcid_link.setter
    def mcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for mcid."""
        self.mcid = value.lcid

    @property
    def coordid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for coordid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.coordid:
                return kwd
        return None

    @coordid_link.setter
    def coordid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for coordid."""
        self.coordid = value.cid

    @property
    def pidl_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pidl."""
        return self._get_link_by_attr("PART", "pid", self.pidl, "parts")

    @property
    def pidc_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pidc."""
        return self._get_link_by_attr("PART", "pid", self.pidc, "parts")

    @property
    def partb_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given partb."""
        return self._get_link_by_attr("PART", "pid", self.partb, "parts")

