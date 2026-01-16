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

"""Module providing the SectionPointSourceMixture class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_SECTIONPOINTSOURCEMIXTURE_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("lcidt", int, 10, 10, None),
    FieldSchema("notused", int, 20, 10, None),
    FieldSchema("lcidvel", int, 30, 10, None),
    FieldSchema("nidlc001", int, 40, 10, None),
    FieldSchema("nidlc002", int, 50, 10, None),
    FieldSchema("nidlc003", int, 60, 10, None),
    FieldSchema("idir", int, 70, 10, None),
)

_SECTIONPOINTSOURCEMIXTURE_CARD1 = (
    FieldSchema("lcmdot1", int, 0, 10, 0),
    FieldSchema("lcmdot2", int, 10, 10, 0),
    FieldSchema("lcmdot3", int, 20, 10, 0),
    FieldSchema("lcmdot4", int, 30, 10, 0),
    FieldSchema("lcmdot5", int, 40, 10, 0),
    FieldSchema("lcmdot6", int, 50, 10, 0),
    FieldSchema("lcmdot7", int, 60, 10, 0),
    FieldSchema("lcmdot8", int, 70, 10, 0),
)

_SECTIONPOINTSOURCEMIXTURE_CARD2 = (
    FieldSchema("nodeid", int, 0, 10, 0),
    FieldSchema("vecid", int, 10, 10, 0),
    FieldSchema("orifarea", float, 20, 10, 0.0),
)

_SECTIONPOINTSOURCEMIXTURE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionPointSourceMixture(KeywordBase):
    """DYNA SECTION_POINT_SOURCE_MIXTURE keyword"""

    keyword = "SECTION"
    subkeyword = "POINT_SOURCE_MIXTURE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "nidlc001": LinkType.NODE,
        "nidlc002": LinkType.NODE,
        "nidlc003": LinkType.NODE,
        "nodeid": LinkType.NODE,
        "lcidt": LinkType.DEFINE_CURVE,
        "notused": LinkType.DEFINE_CURVE,
        "lcidvel": LinkType.DEFINE_CURVE,
        "lcmdot1": LinkType.DEFINE_CURVE,
        "lcmdot2": LinkType.DEFINE_CURVE,
        "lcmdot3": LinkType.DEFINE_CURVE,
        "lcmdot4": LinkType.DEFINE_CURVE,
        "lcmdot5": LinkType.DEFINE_CURVE,
        "lcmdot6": LinkType.DEFINE_CURVE,
        "lcmdot7": LinkType.DEFINE_CURVE,
        "lcmdot8": LinkType.DEFINE_CURVE,
        "vecid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the SectionPointSourceMixture class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONPOINTSOURCEMIXTURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONPOINTSOURCEMIXTURE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONPOINTSOURCEMIXTURE_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionPointSourceMixture.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONPOINTSOURCEMIXTURE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID Number
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Inflator gas mixture average stagnation temperature load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[0].set_value("lcidt", value)

    @property
    def notused(self) -> typing.Optional[int]:
        """Get or set the Not used.
        """ # nopep8
        return self._cards[0].get_value("notused")

    @notused.setter
    def notused(self, value: int) -> None:
        """Set the notused property."""
        self._cards[0].set_value("notused", value)

    @property
    def lcidvel(self) -> typing.Optional[int]:
        """Get or set the inflator gas mixture average velocity load curve ID..  If LCIDVEL=0 or blank, LSDYNA will estimate the inlet gas velocity.
        """ # nopep8
        return self._cards[0].get_value("lcidvel")

    @lcidvel.setter
    def lcidvel(self, value: int) -> None:
        """Set the lcidvel property."""
        self._cards[0].set_value("lcidvel", value)

    @property
    def nidlc001(self) -> typing.Optional[int]:
        """Get or set the The 1st Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc001")

    @nidlc001.setter
    def nidlc001(self, value: int) -> None:
        """Set the nidlc001 property."""
        self._cards[0].set_value("nidlc001", value)

    @property
    def nidlc002(self) -> typing.Optional[int]:
        """Get or set the The 2nd Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc002")

    @nidlc002.setter
    def nidlc002(self, value: int) -> None:
        """Set the nidlc002 property."""
        self._cards[0].set_value("nidlc002", value)

    @property
    def nidlc003(self) -> typing.Optional[int]:
        """Get or set the the 3rd Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc003")

    @nidlc003.setter
    def nidlc003(self, value: int) -> None:
        """Set the nidlc003 property."""
        self._cards[0].set_value("nidlc003", value)

    @property
    def idir(self) -> typing.Optional[int]:
        """Get or set the A flag for constraining the nodal velocity of the nodes of the ALE element containing a point source.  If IDIR=0 (default), then the ALE nodes behind the point source (relative position of nodes based on the vector direction of flow of point source) will have zero velocity.  If IDIR=1, then all ALE nodes will have velocity distributed based on energy conservation.  The latter option seems to be more robust in airbag modeling.
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        self._cards[0].set_value("idir", value)

    @property
    def lcmdot1(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot1")

    @lcmdot1.setter
    def lcmdot1(self, value: int) -> None:
        """Set the lcmdot1 property."""
        self._cards[1].set_value("lcmdot1", value)

    @property
    def lcmdot2(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot2")

    @lcmdot2.setter
    def lcmdot2(self, value: int) -> None:
        """Set the lcmdot2 property."""
        self._cards[1].set_value("lcmdot2", value)

    @property
    def lcmdot3(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot3")

    @lcmdot3.setter
    def lcmdot3(self, value: int) -> None:
        """Set the lcmdot3 property."""
        self._cards[1].set_value("lcmdot3", value)

    @property
    def lcmdot4(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot4")

    @lcmdot4.setter
    def lcmdot4(self, value: int) -> None:
        """Set the lcmdot4 property."""
        self._cards[1].set_value("lcmdot4", value)

    @property
    def lcmdot5(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot5")

    @lcmdot5.setter
    def lcmdot5(self, value: int) -> None:
        """Set the lcmdot5 property."""
        self._cards[1].set_value("lcmdot5", value)

    @property
    def lcmdot6(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot6")

    @lcmdot6.setter
    def lcmdot6(self, value: int) -> None:
        """Set the lcmdot6 property."""
        self._cards[1].set_value("lcmdot6", value)

    @property
    def lcmdot7(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot7")

    @lcmdot7.setter
    def lcmdot7(self, value: int) -> None:
        """Set the lcmdot7 property."""
        self._cards[1].set_value("lcmdot7", value)

    @property
    def lcmdot8(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot8")

    @lcmdot8.setter
    def lcmdot8(self, value: int) -> None:
        """Set the lcmdot8 property."""
        self._cards[1].set_value("lcmdot8", value)

    @property
    def nodeid(self) -> int:
        """Get or set the The node ID(s) defining the point source(s).
        """ # nopep8
        return self._cards[2].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        """Set the nodeid property."""
        self._cards[2].set_value("nodeid", value)

    @property
    def vecid(self) -> int:
        """Get or set the The vector ID defining the direction of flow at each point source.
        """ # nopep8
        return self._cards[2].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[2].set_value("vecid", value)

    @property
    def orifarea(self) -> float:
        """Get or set the The orifice area at each point source
        """ # nopep8
        return self._cards[2].get_value("orifarea")

    @orifarea.setter
    def orifarea(self, value: float) -> None:
        """Set the orifarea property."""
        self._cards[2].set_value("orifarea", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def nidlc001_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nidlc001."""
        return self._get_link_by_attr("NODE", "nid", self.nidlc001, "parts")

    @property
    def nidlc002_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nidlc002."""
        return self._get_link_by_attr("NODE", "nid", self.nidlc002, "parts")

    @property
    def nidlc003_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nidlc003."""
        return self._get_link_by_attr("NODE", "nid", self.nidlc003, "parts")

    @property
    def nodeid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nodeid."""
        return self._get_link_by_attr("NODE", "nid", self.nodeid, "parts")

    @property
    def lcidt_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidt:
                return kwd
        return None

    @lcidt_link.setter
    def lcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidt."""
        self.lcidt = value.lcid

    @property
    def notused_link(self) -> DefineCurve:
        """Get the DefineCurve object for notused."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.notused:
                return kwd
        return None

    @notused_link.setter
    def notused_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for notused."""
        self.notused = value.lcid

    @property
    def lcidvel_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidvel."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidvel:
                return kwd
        return None

    @lcidvel_link.setter
    def lcidvel_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidvel."""
        self.lcidvel = value.lcid

    @property
    def lcmdot1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot1:
                return kwd
        return None

    @lcmdot1_link.setter
    def lcmdot1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot1."""
        self.lcmdot1 = value.lcid

    @property
    def lcmdot2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot2:
                return kwd
        return None

    @lcmdot2_link.setter
    def lcmdot2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot2."""
        self.lcmdot2 = value.lcid

    @property
    def lcmdot3_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot3:
                return kwd
        return None

    @lcmdot3_link.setter
    def lcmdot3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot3."""
        self.lcmdot3 = value.lcid

    @property
    def lcmdot4_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot4:
                return kwd
        return None

    @lcmdot4_link.setter
    def lcmdot4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot4."""
        self.lcmdot4 = value.lcid

    @property
    def lcmdot5_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot5:
                return kwd
        return None

    @lcmdot5_link.setter
    def lcmdot5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot5."""
        self.lcmdot5 = value.lcid

    @property
    def lcmdot6_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot6:
                return kwd
        return None

    @lcmdot6_link.setter
    def lcmdot6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot6."""
        self.lcmdot6 = value.lcid

    @property
    def lcmdot7_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot7:
                return kwd
        return None

    @lcmdot7_link.setter
    def lcmdot7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot7."""
        self.lcmdot7 = value.lcid

    @property
    def lcmdot8_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmdot8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmdot8:
                return kwd
        return None

    @lcmdot8_link.setter
    def lcmdot8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmdot8."""
        self.lcmdot8 = value.lcid

    @property
    def vecid_link(self) -> DefineVector:
        """Get the DefineVector object for vecid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vecid:
                return kwd
        return None

    @vecid_link.setter
    def vecid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vecid."""
        self.vecid = value.vid

