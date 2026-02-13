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

"""Module providing the InitialImpulseMine class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_INITIALIMPULSEMINE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("mtnt", float, 10, 10, 0.0),
    FieldSchema("rhos", float, 20, 10, 0.0),
    FieldSchema("depth", float, 30, 10, 0.0),
    FieldSchema("area", float, 40, 10, 0.0),
    FieldSchema("scale", float, 50, 10, 1.0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unit", int, 70, 10, 1),
)

_INITIALIMPULSEMINE_CARD1 = (
    FieldSchema("x", float, 0, 10, 0.0),
    FieldSchema("y", float, 10, 10, 0.0),
    FieldSchema("z", float, 20, 10, 0.0),
    FieldSchema("nidmc", int, 30, 10, 0),
    FieldSchema("gvid", int, 40, 10, None),
    FieldSchema("tbirth", float, 50, 10, 0.0),
    FieldSchema("psid", int, 60, 10, 0),
    FieldSchema("search", float, 70, 10, 0.0),
)

class InitialImpulseMine(KeywordBase):
    """DYNA INITIAL_IMPULSE_MINE keyword"""

    keyword = "INITIAL"
    subkeyword = "IMPULSE_MINE"
    _link_fields = {
        "nidmc": LinkType.NODE,
        "gvid": LinkType.DEFINE_VECTOR,
        "psid": LinkType.SET_PART,
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialImpulseMine class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALIMPULSEMINE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALIMPULSEMINE_CARD1,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def mtnt(self) -> float:
        """Get or set the Equivalent mass of TNT
        """ # nopep8
        return self._cards[0].get_value("mtnt")

    @mtnt.setter
    def mtnt(self, value: float) -> None:
        """Set the mtnt property."""
        self._cards[0].set_value("mtnt", value)

    @property
    def rhos(self) -> float:
        """Get or set the Density of overburden soil
        """ # nopep8
        return self._cards[0].get_value("rhos")

    @rhos.setter
    def rhos(self, value: float) -> None:
        """Set the rhos property."""
        self._cards[0].set_value("rhos", value)

    @property
    def depth(self) -> float:
        """Get or set the Burial depth from the ground surface to the center of the mine. This value must be a positive
        """ # nopep8
        return self._cards[0].get_value("depth")

    @depth.setter
    def depth(self, value: float) -> None:
        """Set the depth property."""
        self._cards[0].set_value("depth", value)

    @property
    def area(self) -> float:
        """Get or set the Cross sectional area of the mine
        """ # nopep8
        return self._cards[0].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        """Set the area property."""
        self._cards[0].set_value("area", value)

    @property
    def scale(self) -> float:
        """Get or set the Scale factor for the impulse
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        """Set the scale property."""
        self._cards[0].set_value("scale", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system. This must match the units used by finite element model.
        EQ.1: inch, dozen slugs (i.e., lbf-s^2/in), second, psi (default)
        EQ.2: meter, kilogram, second, Pascal
        EQ.3: centimeter, gram, microsecond, megabar
        EQ.4: millimeter, kilogram, millisecond, GPa
        EQ.5: millimeter, metric ton, second, MPa
        EQ.6: millimeter, gram, millisecond, MPa
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [1, 2, 3, 4, 5, 6, None]:
            raise Exception("""unit must be `None` or one of {1,2,3,4,5,6}.""")
        self._cards[0].set_value("unit", value)

    @property
    def x(self) -> float:
        """Get or set the x- coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z- coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def nidmc(self) -> int:
        """Get or set the Optional node ID representing the mine center (see *NODE). If defined then X, Y and Z are ignored
        """ # nopep8
        return self._cards[1].get_value("nidmc")

    @nidmc.setter
    def nidmc(self, value: int) -> None:
        """Set the nidmc property."""
        self._cards[1].set_value("nidmc", value)

    @property
    def gvid(self) -> typing.Optional[int]:
        """Get or set the Vector ID representing the vertically upward direction, i.e., normal to the ground surface
        """ # nopep8
        return self._cards[1].get_value("gvid")

    @gvid.setter
    def gvid(self, value: int) -> None:
        """Set the gvid property."""
        self._cards[1].set_value("gvid", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time. Impulse is activated at this time
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[1].set_value("tbirth", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID identifying the parts affected by the mine.  See *SET_‌PART.  If the segment set defined by SSID includes segments of more than one part, PSID may be used to load only segments of identified parts. Otherwise, if PSID is set to zero, the part affected by the mine defaults to the part comprised by the nodes of the segment set
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def search(self) -> float:
        """Get or set the Limit the search depth into the structure. Initial nodal velocity is distributed from the segment to a depth equal to the SEARCH value. The
        value must be positive. If set to zero the search depth is unlimited and
        extends through the part(s) identified by PSID
        """ # nopep8
        return self._cards[1].get_value("search")

    @search.setter
    def search(self, value: float) -> None:
        """Set the search property."""
        self._cards[1].set_value("search", value)

    @property
    def nidmc_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nidmc."""
        return self._get_link_by_attr("NODE", "nid", self.nidmc, "parts")

    @property
    def gvid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for gvid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.gvid:
                return kwd
        return None

    @gvid_link.setter
    def gvid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for gvid."""
        self.gvid = value.vid

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

    @property
    def ssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

