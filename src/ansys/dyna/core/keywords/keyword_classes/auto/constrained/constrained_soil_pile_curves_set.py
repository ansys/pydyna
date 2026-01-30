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

"""Module providing the ConstrainedSoilPileCurvesSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONSTRAINEDSOILPILECURVESSET_CARD0 = (
    FieldSchema("pbsid", int, 0, 10, None),
    FieldSchema("diam", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("pidns", int, 30, 10, None),
    FieldSchema("pidnb", int, 40, 10, None),
    FieldSchema("error", int, 50, 10, 0),
    FieldSchema("nring", int, 60, 10, 1),
    FieldSchema("nringb", int, 70, 10, None),
)

_CONSTRAINEDSOILPILECURVESSET_CARD1 = (
    FieldSchema("damp", float, 0, 10, 0.0),
    FieldSchema("local", int, 10, 10, 1),
)

_CONSTRAINEDSOILPILECURVESSET_CARD2 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("zref", float, 10, 10, None),
)

_CONSTRAINEDSOILPILECURVESSET_CARD3 = (
    FieldSchema("blcz", int, 0, 10, None),
    FieldSchema("blc", int, 10, 10, None),
    FieldSchema("blcsh", int, 20, 10, None),
    FieldSchema("blcsv", int, 30, 10, None),
)

_CONSTRAINEDSOILPILECURVESSET_CARD4 = (
    FieldSchema("vlcz", int, 0, 10, None),
    FieldSchema("vlc", int, 10, 10, None),
    FieldSchema("vlcsh", int, 20, 10, None),
    FieldSchema("vlcsv", int, 30, 10, None),
)

_CONSTRAINEDSOILPILECURVESSET_CARD5 = (
    FieldSchema("hlcz", int, 0, 10, None),
    FieldSchema("hlc", int, 10, 10, None),
    FieldSchema("hlcsh", int, 20, 10, None),
    FieldSchema("hlcsv", int, 30, 10, None),
)

class ConstrainedSoilPileCurvesSet(KeywordBase):
    """DYNA CONSTRAINED_SOIL_PILE_CURVES_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SOIL_PILE_CURVES_SET"
    _link_fields = {
        "blcz": LinkType.DEFINE_CURVE,
        "blc": LinkType.DEFINE_CURVE,
        "blcsh": LinkType.DEFINE_CURVE,
        "blcsv": LinkType.DEFINE_CURVE,
        "vlcz": LinkType.DEFINE_CURVE,
        "vlc": LinkType.DEFINE_CURVE,
        "vlcsh": LinkType.DEFINE_CURVE,
        "vlcsv": LinkType.DEFINE_CURVE,
        "hlcz": LinkType.DEFINE_CURVE,
        "hlc": LinkType.DEFINE_CURVE,
        "hlcsh": LinkType.DEFINE_CURVE,
        "hlcsv": LinkType.DEFINE_CURVE,
        "pbsid": LinkType.SET_PART,
        "psid": LinkType.SET_PART,
        "pidns": LinkType.PART,
        "pidnb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedSoilPileCurvesSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECURVESSET_CARD5,
                **kwargs,
            ),        ]
    @property
    def pbsid(self) -> typing.Optional[int]:
        """Get or set the Part set ID containing beam elements for coupling (the piles).
        """ # nopep8
        return self._cards[0].get_value("pbsid")

    @pbsid.setter
    def pbsid(self, value: int) -> None:
        """Set the pbsid property."""
        self._cards[0].set_value("pbsid", value)

    @property
    def diam(self) -> typing.Optional[float]:
        """Get or set the Pile diameter (optional). If zero or blank, the pile diameter will be taken automatically from the section properties of the beam element.
        """ # nopep8
        return self._cards[0].get_value("diam")

    @diam.setter
    def diam(self, value: float) -> None:
        """Set the diam property."""
        self._cards[0].set_value("diam", value)

    @property
    def pidns(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for perpendicular and axial coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidns")

    @pidns.setter
    def pidns(self, value: int) -> None:
        """Set the pidns property."""
        self._cards[0].set_value("pidns", value)

    @property
    def pidnb(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for base coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidnb")

    @pidnb.setter
    def pidnb(self, value: int) -> None:
        """Set the pidnb property."""
        self._cards[0].set_value("pidnb", value)

    @property
    def error(self) -> int:
        """Get or set the Action taken if any coupling point is not constrained within a soil element:
        EQ.0:	Stop with an error message.
        EQ.1 : Warn and continue..
        """ # nopep8
        return self._cards[0].get_value("error")

    @error.setter
    def error(self, value: int) -> None:
        """Set the error property."""
        if value not in [0, 1, None]:
            raise Exception("""error must be `None` or one of {0,1}.""")
        self._cards[0].set_value("error", value)

    @property
    def nring(self) -> int:
        """Get or set the Number of coupling points around circumference at each pile node:
        EQ.1:	One coupling point coincident with pile node
        GT.1 : NRING coupling points equally spaced around the circumference of the pile.
        """ # nopep8
        return self._cards[0].get_value("nring")

    @nring.setter
    def nring(self, value: int) -> None:
        """Set the nring property."""
        self._cards[0].set_value("nring", value)

    @property
    def nringb(self) -> typing.Optional[int]:
        """Get or set the Number of extra rings of coupling points on base, in addition to those around the pile circumference. By default, NRINGB is chosen automatically to distribute the base stress as uniformly as possible .
        """ # nopep8
        return self._cards[0].get_value("nringb")

    @nringb.setter
    def nringb(self, value: int) -> None:
        """Set the nringb property."""
        self._cards[0].set_value("nringb", value)

    @property
    def damp(self) -> float:
        """Get or set the Optional damping coefficient for Axial coupling (stress/velocity units). An additional axial coupling shear stress equal to DAMP times the axial velocity of the pile relative to the soil will be generated.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to identify which free end of a pile is treated as the Base:
        EQ.1:	End with the most negative global Z - coordinate
        EQ.2 : End which is Node 1 of the attached beam element topology.
        """ # nopep8
        return self._cards[1].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        """Set the local property."""
        if value not in [1, 2, None]:
            raise Exception("""local must be `None` or one of {1,2}.""")
        self._cards[1].set_value("local", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID (depending on OPTION2) containing solid elements for coupling (the soil).
        """ # nopep8
        return self._cards[2].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[2].set_value("psid", value)

    @property
    def zref(self) -> typing.Optional[float]:
        """Get or set the Reference Z-coordinate, used in calculation of “relative z-coordinate”. For example, ZREF may be located at the soil surface.  .
        """ # nopep8
        return self._cards[2].get_value("zref")

    @zref.setter
    def zref(self, value: float) -> None:
        """Set the zref property."""
        self._cards[2].set_value("zref", value)

    @property
    def blcz(self) -> typing.Optional[int]:
        """Get or set the For base coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[3].get_value("blcz")

    @blcz.setter
    def blcz(self, value: int) -> None:
        """Set the blcz property."""
        self._cards[3].set_value("blcz", value)

    @property
    def blc(self) -> typing.Optional[int]:
        """Get or set the For base coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement.
        """ # nopep8
        return self._cards[3].get_value("blc")

    @blc.setter
    def blc(self, value: int) -> None:
        """Set the blc property."""
        self._cards[3].set_value("blc", value)

    @property
    def blcsh(self) -> typing.Optional[int]:
        """Get or set the For base coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate .
        """ # nopep8
        return self._cards[3].get_value("blcsh")

    @blcsh.setter
    def blcsh(self, value: int) -> None:
        """Set the blcsh property."""
        self._cards[3].set_value("blcsh", value)

    @property
    def blcsv(self) -> typing.Optional[int]:
        """Get or set the For base coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate .
        """ # nopep8
        return self._cards[3].get_value("blcsv")

    @blcsv.setter
    def blcsv(self, value: int) -> None:
        """Set the blcsv property."""
        self._cards[3].set_value("blcsv", value)

    @property
    def vlcz(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[4].get_value("vlcz")

    @vlcz.setter
    def vlcz(self, value: int) -> None:
        """Set the vlcz property."""
        self._cards[4].set_value("vlcz", value)

    @property
    def vlc(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement .
        """ # nopep8
        return self._cards[4].get_value("vlc")

    @vlc.setter
    def vlc(self, value: int) -> None:
        """Set the vlc property."""
        self._cards[4].set_value("vlc", value)

    @property
    def vlcsh(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[4].get_value("vlcsh")

    @vlcsh.setter
    def vlcsh(self, value: int) -> None:
        """Set the vlcsh property."""
        self._cards[4].set_value("vlcsh", value)

    @property
    def vlcsv(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[4].get_value("vlcsv")

    @vlcsv.setter
    def vlcsv(self, value: int) -> None:
        """Set the vlcsv property."""
        self._cards[4].set_value("vlcsv", value)

    @property
    def hlcz(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[5].get_value("hlcz")

    @hlcz.setter
    def hlcz(self, value: int) -> None:
        """Set the hlcz property."""
        self._cards[5].set_value("hlcz", value)

    @property
    def hlc(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement
        """ # nopep8
        return self._cards[5].get_value("hlc")

    @hlc.setter
    def hlc(self, value: int) -> None:
        """Set the hlc property."""
        self._cards[5].set_value("hlc", value)

    @property
    def hlcsh(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate
        """ # nopep8
        return self._cards[5].get_value("hlcsh")

    @hlcsh.setter
    def hlcsh(self, value: int) -> None:
        """Set the hlcsh property."""
        self._cards[5].set_value("hlcsh", value)

    @property
    def hlcsv(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[5].get_value("hlcsv")

    @hlcsv.setter
    def hlcsv(self, value: int) -> None:
        """Set the hlcsv property."""
        self._cards[5].set_value("hlcsv", value)

    @property
    def blcz_link(self) -> DefineCurve:
        """Get the DefineCurve object for blcz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.blcz:
                return kwd
        return None

    @blcz_link.setter
    def blcz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for blcz."""
        self.blcz = value.lcid

    @property
    def blc_link(self) -> DefineCurve:
        """Get the DefineCurve object for blc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.blc:
                return kwd
        return None

    @blc_link.setter
    def blc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for blc."""
        self.blc = value.lcid

    @property
    def blcsh_link(self) -> DefineCurve:
        """Get the DefineCurve object for blcsh."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.blcsh:
                return kwd
        return None

    @blcsh_link.setter
    def blcsh_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for blcsh."""
        self.blcsh = value.lcid

    @property
    def blcsv_link(self) -> DefineCurve:
        """Get the DefineCurve object for blcsv."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.blcsv:
                return kwd
        return None

    @blcsv_link.setter
    def blcsv_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for blcsv."""
        self.blcsv = value.lcid

    @property
    def vlcz_link(self) -> DefineCurve:
        """Get the DefineCurve object for vlcz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.vlcz:
                return kwd
        return None

    @vlcz_link.setter
    def vlcz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for vlcz."""
        self.vlcz = value.lcid

    @property
    def vlc_link(self) -> DefineCurve:
        """Get the DefineCurve object for vlc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.vlc:
                return kwd
        return None

    @vlc_link.setter
    def vlc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for vlc."""
        self.vlc = value.lcid

    @property
    def vlcsh_link(self) -> DefineCurve:
        """Get the DefineCurve object for vlcsh."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.vlcsh:
                return kwd
        return None

    @vlcsh_link.setter
    def vlcsh_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for vlcsh."""
        self.vlcsh = value.lcid

    @property
    def vlcsv_link(self) -> DefineCurve:
        """Get the DefineCurve object for vlcsv."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.vlcsv:
                return kwd
        return None

    @vlcsv_link.setter
    def vlcsv_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for vlcsv."""
        self.vlcsv = value.lcid

    @property
    def hlcz_link(self) -> DefineCurve:
        """Get the DefineCurve object for hlcz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcz:
                return kwd
        return None

    @hlcz_link.setter
    def hlcz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcz."""
        self.hlcz = value.lcid

    @property
    def hlc_link(self) -> DefineCurve:
        """Get the DefineCurve object for hlc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlc:
                return kwd
        return None

    @hlc_link.setter
    def hlc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlc."""
        self.hlc = value.lcid

    @property
    def hlcsh_link(self) -> DefineCurve:
        """Get the DefineCurve object for hlcsh."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcsh:
                return kwd
        return None

    @hlcsh_link.setter
    def hlcsh_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcsh."""
        self.hlcsh = value.lcid

    @property
    def hlcsv_link(self) -> DefineCurve:
        """Get the DefineCurve object for hlcsv."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcsv:
                return kwd
        return None

    @hlcsv_link.setter
    def hlcsv_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcsv."""
        self.hlcsv = value.lcid

    @property
    def pbsid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for pbsid."""
        return self._get_set_link("PART", self.pbsid)

    @pbsid_link.setter
    def pbsid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pbsid."""
        self.pbsid = value.sid

    @property
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

    @property
    def pidns_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidns."""
        return self._get_link_by_attr("PART", "pid", self.pidns, "parts")

    @property
    def pidnb_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidnb."""
        return self._get_link_by_attr("PART", "pid", self.pidnb, "parts")

