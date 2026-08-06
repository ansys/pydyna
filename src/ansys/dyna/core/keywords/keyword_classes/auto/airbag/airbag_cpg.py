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

"""Module providing the AirbagCpg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGCPG_CARD0 = (
    FieldSchema("sid1", int, 0, 10, None),
    FieldSchema("stype1", int, 10, 10, 0),
    FieldSchema("sid2", int, 20, 10, 0),
    FieldSchema("stype2", int, 30, 10, 0),
    FieldSchema("block", int, 40, 10, None),
    FieldSchema("npdata", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("spsid", int, 70, 10, None),
)

_AIRBAGCPG_CARD1 = (
    FieldSchema("hlen", float, 0, 10, 0.0),
    FieldSchema("unit", int, 10, 10, 0),
    FieldSchema("adaid", int, 20, 10, None),
    FieldSchema("tatm", float, 30, 10, 293.0),
    FieldSchema("patm", float, 40, 10, None),
    FieldSchema("nvent", int, 50, 10, 0),
    FieldSchema("birth", float, 60, 10, None),
    FieldSchema("tsw", float, 70, 10, None),
)

_AIRBAGCPG_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ngas", int, 10, 10, None),
    FieldSchema("norif", int, 20, 10, None),
    FieldSchema("nid1", int, 30, 10, 0),
    FieldSchema("nid2", int, 40, 10, 0),
    FieldSchema("nid3", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_AIRBAGCPG_CARD3 = (
    FieldSchema("sidh", int, 0, 10, None),
    FieldSchema("stypeh", int, 10, 10, 0),
    FieldSchema("hconv", float, 20, 10, None),
    FieldSchema("pfric", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_AIRBAGCPG_CARD4 = (
    FieldSchema("sid3", int, 0, 10, None),
    FieldSchema("stype3", int, 10, 10, 0),
    FieldSchema("hlenv", float, 20, 10, None),
    FieldSchema("lct", int, 30, 10, 0),
    FieldSchema("lcp", int, 40, 10, 0),
    FieldSchema("dvid", int, 50, 10, None),
    FieldSchema("ppop", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_AIRBAGCPG_CARD5 = (
    FieldSchema("pair", float, 0, 10, None),
    FieldSchema("tair", float, 10, 10, 0.0),
    FieldSchema("xmair", float, 20, 10, None),
    FieldSchema("aair", float, 30, 10, None),
    FieldSchema("bair", float, 40, 10, 0.0),
    FieldSchema("cair", float, 50, 10, 0.0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_AIRBAGCPG_CARD6 = (
    FieldSchema("lcmi", int, 0, 10, None),
    FieldSchema("lcti", int, 10, 10, None),
    FieldSchema("xmi", float, 20, 10, None),
    FieldSchema("ai", float, 30, 10, None),
    FieldSchema("bi", float, 40, 10, 0.0),
    FieldSchema("ci", float, 50, 10, 0.0),
    FieldSchema("infgi", int, 60, 10, 1),
    FieldSchema("unused", int, 70, 10, None),
)

_AIRBAGCPG_CARD7 = (
    FieldSchema("ssidi", int, 0, 10, None),
    FieldSchema("hleno", float, 10, 10, None),
    FieldSchema("roloc", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("infoi", int, 40, 10, 1),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

class AirbagCpg(KeywordBase):
    """DYNA AIRBAG_CPG keyword"""

    keyword = "AIRBAG"
    subkeyword = "CPG"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "ssidi": LinkType.ELEMENT_SHELL,
        "lct": LinkType.DEFINE_CURVE,
        "lcp": LinkType.DEFINE_CURVE,
        "lcmi": LinkType.DEFINE_CURVE,
        "lcti": LinkType.DEFINE_CURVE,
        "spsid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagCpg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGCPG_CARD7,
                **kwargs,
            ),
        ]
    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the complete airbag.
        """ # nopep8
        return self._cards[0].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[0].set_value("sid1", value)

    @property
    def stype1(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[0].get_value("stype1")

    @stype1.setter
    def stype1(self, value: int) -> None:
        """Set the stype1 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype1 must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Part or part set ID defining the embeded parts of the airbag.
        """ # nopep8
        return self._cards[0].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[0].set_value("sid2", value)

    @property
    def stype2(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.

        """ # nopep8
        return self._cards[0].get_value("stype2")

    @stype2.setter
    def stype2(self, value: int) -> None:
        """Set the stype2 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype2 must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype2", value)

    @property
    def block(self) -> typing.Optional[int]:
        """Get or set the Blocking options. C
        EQ.0:	Off.Blocking is not considered
        EQ.1 : Elements receiving contact forces are blocked : porosity and venting leakage are set to 0.
        EQ.4 : Same as 1 except blockage is not applied at vents.
        """ # nopep8
        return self._cards[0].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        """Set the block property."""
        self._cards[0].set_value("block", value)

    @property
    def npdata(self) -> int:
        """Get or set the Number of parts or part sets with additional data.
        """ # nopep8
        return self._cards[0].get_value("npdata")

    @npdata.setter
    def npdata(self, value: int) -> None:
        """Set the npdata property."""
        self._cards[0].set_value("npdata", value)

    @property
    def spsid(self) -> typing.Optional[int]:
        """Get or set the Optional sampling part set ID for geodesic redistribution of CPG particles on solid surface shell elements (see Remark 10).
        """ # nopep8
        return self._cards[0].get_value("spsid")

    @spsid.setter
    def spsid(self, value: int) -> None:
        """Set the spsid property."""
        self._cards[0].set_value("spsid", value)

    @property
    def hlen(self) -> float:
        """Get or set the Average interparticle distance. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("hlen")

    @hlen.setter
    def hlen(self, value: float) -> None:
        """Set the hlen property."""
        self._cards[1].set_value("hlen", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system
        EQ.0: kg-mm-ms-K
        EQ.1: SI
        EQ.2: tonne-mm-s-K.
        """ # nopep8
        return self._cards[1].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""unit must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("unit", value)

    @property
    def adaid(self) -> typing.Optional[int]:
        """Get or set the Optional ID referencing *DEFINE_CPG_ADAPTIVE.
        """ # nopep8
        return self._cards[1].get_value("adaid")

    @adaid.setter
    def adaid(self, value: int) -> None:
        """Set the adaid property."""
        self._cards[1].set_value("adaid", value)

    @property
    def tatm(self) -> float:
        """Get or set the Atmospheric temperature (Default 293K).
        """ # nopep8
        return self._cards[1].get_value("tatm")

    @tatm.setter
    def tatm(self, value: float) -> None:
        """Set the tatm property."""
        self._cards[1].set_value("tatm", value)

    @property
    def patm(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure (Default 1ATM).
        """ # nopep8
        return self._cards[1].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[1].set_value("patm", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vent hole parts or part sets.
        """ # nopep8
        return self._cards[1].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        """Set the nvent property."""
        self._cards[1].set_value("nvent", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Birth time for CPG solver. Note that there should be no deformation occurring in any part of SID1 prior to birth time.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def tsw(self) -> typing.Optional[float]:
        """Get or set the If greater than 0 time at which the algorithm switches to the control volume method with uniform pressure.
        """ # nopep8
        return self._cards[1].get_value("tsw")

    @tsw.setter
    def tsw(self, value: float) -> None:
        """Set the tsw property."""
        self._cards[1].set_value("tsw", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas components.
        """ # nopep8
        return self._cards[2].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        """Set the ngas property."""
        self._cards[2].set_value("ngas", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices.
        """ # nopep8
        return self._cards[2].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[2].set_value("norif", value)

    @property
    def nid1(self) -> int:
        """Get or set the Three nodes that define a moving coordinate system. By default, the coordinate system is fixed.
        """ # nopep8
        return self._cards[2].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[2].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the Three nodes that define a moving coordinate system default, the coordinate system is fixed.
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[2].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the Three nodes that define a moving coordinate system default, the coordinate system is fixed.
        """ # nopep8
        return self._cards[2].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[2].set_value("nid3", value)

    @property
    def sidh(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining part data.
        """ # nopep8
        return self._cards[3].get_value("sidh")

    @sidh.setter
    def sidh(self, value: int) -> None:
        """Set the sidh property."""
        self._cards[3].set_value("sidh", value)

    @property
    def stypeh(self) -> int:
        """Get or set the Set type EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[3].get_value("stypeh")

    @stypeh.setter
    def stypeh(self, value: int) -> None:
        """Set the stypeh property."""
        if value not in [0, 1, None]:
            raise Exception("""stypeh must be `None` or one of {0,1}.""")
        self._cards[3].set_value("stypeh", value)

    @property
    def hconv(self) -> typing.Optional[float]:
        """Get or set the Convective heat transfer coefficient used to calculate heat loss from the airbag�s external surface to the ambient. See Remark 2.
        """ # nopep8
        return self._cards[3].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        """Set the hconv property."""
        self._cards[3].set_value("hconv", value)

    @property
    def pfric(self) -> float:
        """Get or set the Friction factor.
        """ # nopep8
        return self._cards[3].get_value("pfric")

    @pfric.setter
    def pfric(self, value: float) -> None:
        """Set the pfric property."""
        self._cards[3].set_value("pfric", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining vent holes.
        """ # nopep8
        return self._cards[4].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        """Set the sid3 property."""
        self._cards[4].set_value("sid3", value)

    @property
    def stype3(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set.
        """ # nopep8
        return self._cards[4].get_value("stype3")

    @stype3.setter
    def stype3(self, value: int) -> None:
        """Set the stype3 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype3 must be `None` or one of {0,1}.""")
        self._cards[4].set_value("stype3", value)

    @property
    def hlenv(self) -> typing.Optional[float]:
        """Get or set the Optional local average interparticle distance centered on the vent. See Remark 7.
        """ # nopep8
        return self._cards[4].get_value("hlenv")

    @hlenv.setter
    def hlenv(self, value: float) -> None:
        """Set the hlenv property."""
        self._cards[4].set_value("hlenv", value)

    @property
    def lct(self) -> int:
        """Get or set the Optional load curve to impose temperature as a function of time in cases where backflow is detected.
        Otherwise, TATM is used. See Remark 4.
        """ # nopep8
        return self._cards[4].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        """Set the lct property."""
        self._cards[4].set_value("lct", value)

    @property
    def lcp(self) -> int:
        """Get or set the Optional load curve to impose pressure as a function of time. Otherwise, PATM is used. See Remark 4.
        """ # nopep8
        return self._cards[4].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[4].set_value("lcp", value)

    @property
    def dvid(self) -> typing.Optional[int]:
        """Get or set the Optional vent ID. See *DEFINE_CPG_VENT. When active, LCT, LCP and PPOP on this line are ignored and replaced with *DEFINE_CPG_VENT input.
        """ # nopep8
        return self._cards[4].get_value("dvid")

    @dvid.setter
    def dvid(self, value: int) -> None:
        """Set the dvid property."""
        self._cards[4].set_value("dvid", value)

    @property
    def ppop(self) -> typing.Optional[int]:
        """Get or set the Pressure difference between the interior and ambient pressure (PATM) for opening  the vent holes. Once the vents are open, they stay open.
        """ # nopep8
        return self._cards[4].get_value("ppop")

    @ppop.setter
    def ppop(self, value: int) -> None:
        """Set the ppop property."""
        self._cards[4].set_value("ppop", value)

    @property
    def pair(self) -> typing.Optional[float]:
        """Get or set the Initial pressure inside bag.
        """ # nopep8
        return self._cards[5].get_value("pair")

    @pair.setter
    def pair(self, value: float) -> None:
        """Set the pair property."""
        self._cards[5].set_value("pair", value)

    @property
    def tair(self) -> float:
        """Get or set the Initial temperature inside bag.
        """ # nopep8
        return self._cards[5].get_value("tair")

    @tair.setter
    def tair(self, value: float) -> None:
        """Set the tair property."""
        self._cards[5].set_value("tair", value)

    @property
    def xmair(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas initially inside bag.
        LT.0: -XMAIR references the ID of a *DEFINE_CPG_GAS_PROPERTIES keyword that defines the gas thermodynamic properties.
        Note that AAIR, BAIR, and CAIR are ignored
        """ # nopep8
        return self._cards[5].get_value("xmair")

    @xmair.setter
    def xmair(self, value: float) -> None:
        """Set the xmair property."""
        self._cards[5].set_value("xmair", value)

    @property
    def aair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[5].get_value("aair")

    @aair.setter
    def aair(self, value: float) -> None:
        """Set the aair property."""
        self._cards[5].set_value("aair", value)

    @property
    def bair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[5].get_value("bair")

    @bair.setter
    def bair(self, value: float) -> None:
        """Set the bair property."""
        self._cards[5].set_value("bair", value)

    @property
    def cair(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters.
        """ # nopep8
        return self._cards[5].get_value("cair")

    @cair.setter
    def cair(self, value: float) -> None:
        """Set the cair property."""
        self._cards[5].set_value("cair", value)

    @property
    def lcmi(self) -> typing.Optional[int]:
        """Get or set the Mass flow rate curve for gas component i.
        """ # nopep8
        return self._cards[6].get_value("lcmi")

    @lcmi.setter
    def lcmi(self, value: int) -> None:
        """Set the lcmi property."""
        self._cards[6].set_value("lcmi", value)

    @property
    def lcti(self) -> typing.Optional[int]:
        """Get or set the Temperature curve for gas component i.
        """ # nopep8
        return self._cards[6].get_value("lcti")

    @lcti.setter
    def lcti(self, value: int) -> None:
        """Set the lcti property."""
        self._cards[6].set_value("lcti", value)

    @property
    def xmi(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas component i.
        LT.0: the absolute value of XMi references the ID of a *DEFINE_CPG_GAS_PROPERTIES keyword that defines the gas thermodynamic properties.
        Note that Ai, Bi, and Ci are ignored
        """ # nopep8
        return self._cards[6].get_value("xmi")

    @xmi.setter
    def xmi(self, value: float) -> None:
        """Set the xmi property."""
        self._cards[6].set_value("xmi", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[6].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        """Set the ai property."""
        self._cards[6].set_value("ai", value)

    @property
    def bi(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[6].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[6].set_value("bi", value)

    @property
    def ci(self) -> float:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
        """ # nopep8
        return self._cards[6].get_value("ci")

    @ci.setter
    def ci(self, value: float) -> None:
        """Set the ci property."""
        self._cards[6].set_value("ci", value)

    @property
    def infgi(self) -> int:
        """Get or set the Inflator ID that this gas component belongs to (Default 1).
        """ # nopep8
        return self._cards[6].get_value("infgi")

    @infgi.setter
    def infgi(self, value: int) -> None:
        """Set the infgi property."""
        self._cards[6].set_value("infgi", value)

    @property
    def ssidi(self) -> typing.Optional[int]:
        """Get or set the Shell ID defining the location of nozzle i. If a negative value is entered, the absolute value refers to a Set Shell ID. See Remark 5
        """ # nopep8
        return self._cards[7].get_value("ssidi")

    @ssidi.setter
    def ssidi(self, value: int) -> None:
        """Set the ssidi property."""
        self._cards[7].set_value("ssidi", value)

    @property
    def hleno(self) -> typing.Optional[float]:
        """Get or set the Optional local average interparticle distance centered on the orifice.
        """ # nopep8
        return self._cards[7].get_value("hleno")

    @hleno.setter
    def hleno(self, value: float) -> None:
        """Set the hleno property."""
        self._cards[7].set_value("hleno", value)

    @property
    def roloc(self) -> typing.Optional[float]:
        """Get or set the Optional radius definition that overwrites the automatic refinement size centered around the orifice when HLENO is defined. See Remark 7.
        """ # nopep8
        return self._cards[7].get_value("roloc")

    @roloc.setter
    def roloc(self, value: float) -> None:
        """Set the roloc property."""
        self._cards[7].set_value("roloc", value)

    @property
    def infoi(self) -> int:
        """Get or set the Inflator ID for this orifice. (default = 1).
        """ # nopep8
        return self._cards[7].get_value("infoi")

    @infoi.setter
    def infoi(self, value: int) -> None:
        """Set the infoi property."""
        self._cards[7].set_value("infoi", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def ssidi_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given ssidi."""
        return self._get_link_by_attr("ELEMENT", "eid", self.ssidi, "parts")

    @property
    def lct_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lct."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lct:
                return kwd
        return None

    @lct_link.setter
    def lct_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lct."""
        self.lct = value.lcid

    @property
    def lcp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcp:
                return kwd
        return None

    @lcp_link.setter
    def lcp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcp."""
        self.lcp = value.lcid

    @property
    def lcmi_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcmi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmi:
                return kwd
        return None

    @lcmi_link.setter
    def lcmi_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmi."""
        self.lcmi = value.lcid

    @property
    def lcti_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcti."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcti:
                return kwd
        return None

    @lcti_link.setter
    def lcti_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcti."""
        self.lcti = value.lcid

    @property
    def spsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for spsid."""
        return self._get_set_link("PART", self.spsid)

    @spsid_link.setter
    def spsid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for spsid."""
        self.spsid = value.sid

