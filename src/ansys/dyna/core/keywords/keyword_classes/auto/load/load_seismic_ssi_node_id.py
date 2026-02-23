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

"""Module providing the LoadSeismicSsiNodeId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_LOADSEISMICSSINODEID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_LOADSEISMICSSINODEID_CARD1 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("typeid", int, 10, 10, None),
    FieldSchema("gmx", int, 20, 10, None),
    FieldSchema("gmy", int, 30, 10, None),
    FieldSchema("gmz", int, 40, 10, None),
)

_LOADSEISMICSSINODEID_CARD2 = (
    FieldSchema("sf", float, 0, 10, 1.0),
    FieldSchema("cid", int, 10, 10, 0),
    FieldSchema("birth", float, 20, 10, 0.0),
    FieldSchema("death", float, 30, 10, 1e+28),
    FieldSchema("isg", int, 40, 10, 0),
    FieldSchema("igm", int, 50, 10, 0),
)

class LoadSeismicSsiNodeId(KeywordBase):
    """DYNA LOAD_SEISMIC_SSI_NODE_ID keyword"""

    keyword = "LOAD"
    subkeyword = "SEISMIC_SSI_NODE_ID"
    _link_fields = {
        "typeid": LinkType.NODE,
        "gmx": LinkType.DEFINE_CURVE,
        "gmy": LinkType.DEFINE_CURVE,
        "gmz": LinkType.DEFINE_CURVE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadSeismicSsiNodeId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSINODEID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSINODEID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSINODEID_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the Node ID (NID in *NODE).
        """ # nopep8
        return self._cards[1].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        """Set the typeid property."""
        self._cards[1].set_value("typeid", value)

    @property
    def gmx(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) x-direction.
        """ # nopep8
        return self._cards[1].get_value("gmx")

    @gmx.setter
    def gmx(self, value: int) -> None:
        """Set the gmx property."""
        self._cards[1].set_value("gmx", value)

    @property
    def gmy(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) y-direction.
        """ # nopep8
        return self._cards[1].get_value("gmy")

    @gmy.setter
    def gmy(self, value: int) -> None:
        """Set the gmy property."""
        self._cards[1].set_value("gmy", value)

    @property
    def gmz(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) z-direction.
        """ # nopep8
        return self._cards[1].get_value("gmz")

    @gmz.setter
    def gmz(self, value: int) -> None:
        """Set the gmz property."""
        self._cards[1].set_value("gmz", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[2].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[2].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[2].set_value("cid", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[2].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[2].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[2].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[2].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
        """ # nopep8
        return self._cards[2].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        """Set the isg property."""
        if value not in [0, 1, None]:
            raise Exception("""isg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("isg", value)

    @property
    def igm(self) -> int:
        """Get or set the Specification of ground motions GMX, GMY, GMZ:
        EQ.0: ground motions are specified as acceleration load curves. See *DEFINE_CURVE
        EQ.1: Both ground accelerations and velocities specified using *DEFINE_GROUND_MOTION
        .
        """ # nopep8
        return self._cards[2].get_value("igm")

    @igm.setter
    def igm(self, value: int) -> None:
        """Set the igm property."""
        if value not in [0, 1, None]:
            raise Exception("""igm must be `None` or one of {0,1}.""")
        self._cards[2].set_value("igm", value)

    @property
    def typeid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given typeid."""
        return self._get_link_by_attr("NODE", "nid", self.typeid, "parts")

    @property
    def gmx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for gmx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.gmx:
                return kwd
        return None

    @gmx_link.setter
    def gmx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for gmx."""
        self.gmx = value.lcid

    @property
    def gmy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for gmy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.gmy:
                return kwd
        return None

    @gmy_link.setter
    def gmy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for gmy."""
        self.gmy = value.lcid

    @property
    def gmz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for gmz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.gmz:
                return kwd
        return None

    @gmz_link.setter
    def gmz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for gmz."""
        self.gmz = value.lcid

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

