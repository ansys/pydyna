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

"""Module providing the AirbagWangNefskeId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGWANGNEFSKEID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_AIRBAGWANGNEFSKEID_CARD1 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("sidtyp", int, 10, 10, 0),
    FieldSchema("rbid", int, 20, 10, 0),
    FieldSchema("vsca", float, 30, 10, 1.0),
    FieldSchema("psca", float, 40, 10, 1.0),
    FieldSchema("vini", float, 50, 10, 0.0),
    FieldSchema("mwd", float, 60, 10, 0.0),
    FieldSchema("spsf", float, 70, 10, 0.0),
)

_AIRBAGWANGNEFSKEID_CARD2 = (
    FieldSchema("cv", float, 0, 10, None),
    FieldSchema("cp", float, 10, 10, None),
    FieldSchema("t", float, 20, 10, 0.0),
    FieldSchema("lct", int, 30, 10, 0),
    FieldSchema("lcmt", int, 40, 10, None),
    FieldSchema("tvol", float, 50, 10, 0.0),
    FieldSchema("lcdt", int, 60, 10, 0),
    FieldSchema("iabt", float, 70, 10, None),
)

_AIRBAGWANGNEFSKEID_CARD3 = (
    FieldSchema("c23", float, 0, 10, None),
    FieldSchema("lcc23", int, 10, 10, 0),
    FieldSchema("a23", float, 20, 10, None),
    FieldSchema("lca23", int, 30, 10, 0),
    FieldSchema("cp23", float, 40, 10, None),
    FieldSchema("lccp23", int, 50, 10, 0),
    FieldSchema("ap23", float, 60, 10, 0.0),
    FieldSchema("lcap23", int, 70, 10, 0),
)

_AIRBAGWANGNEFSKEID_CARD4 = (
    FieldSchema("pe", float, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("gc", float, 20, 10, None),
    FieldSchema("lcefr", int, 30, 10, 0),
    FieldSchema("pover", float, 40, 10, 0.0),
    FieldSchema("ppop", float, 50, 10, 0.0),
    FieldSchema("opt", int, 60, 10, 1),
    FieldSchema("knkdn", int, 70, 10, 0),
)

_AIRBAGWANGNEFSKEID_CARD5 = (
    FieldSchema("ioc", float, 0, 10, None),
    FieldSchema("ioa", float, 10, 10, None),
    FieldSchema("ivol", float, 20, 10, None),
    FieldSchema("iro", float, 30, 10, None),
    FieldSchema("it", float, 40, 10, None),
    FieldSchema("lcbf", int, 50, 10, None),
)

_AIRBAGWANGNEFSKEID_CARD6 = (
    FieldSchema("text", float, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("mw", float, 30, 10, None),
    FieldSchema("gasc", float, 40, 10, None),
    FieldSchema("hconv", float, 50, 10, 0.0),
)

class AirbagWangNefskeId(KeywordBase):
    """DYNA AIRBAG_WANG_NEFSKE_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "WANG_NEFSKE_ID"
    _link_fields = {
        "lct": LinkType.DEFINE_CURVE,
        "lcmt": LinkType.DEFINE_CURVE,
        "lcdt": LinkType.DEFINE_CURVE,
        "lcc23": LinkType.DEFINE_CURVE,
        "lca23": LinkType.DEFINE_CURVE,
        "lccp23": LinkType.DEFINE_CURVE,
        "lcap23": LinkType.DEFINE_CURVE,
        "lcefr": LinkType.DEFINE_CURVE,
        "knkdn": LinkType.DEFINE_CURVE,
        "lcbf": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagWangNefskeId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGWANGNEFSKEID_CARD6,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[1].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[1].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        """Set the sidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sidtyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[1].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        """Set the rbid property."""
        self._cards[1].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        """Set the vsca property."""
        self._cards[1].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        """Set the psca property."""
        self._cards[1].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        """Set the vini property."""
        self._cards[1].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        """Set the mwd property."""
        self._cards[1].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        """Set the spsf property."""
        self._cards[1].set_value("spsf", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[2].set_value("cv", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant pressure.
        """ # nopep8
        return self._cards[2].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[2].set_value("cp", value)

    @property
    def t(self) -> float:
        """Get or set the Temperature of input gas (default =0.0).
        For temperature variations a load curve, LCT, may be defined.
        """ # nopep8
        return self._cards[2].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[2].set_value("t", value)

    @property
    def lct(self) -> int:
        """Get or set the Optional load curve number defining temperature of input gas versus time.  This overides columns T.
        """ # nopep8
        return self._cards[2].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        """Set the lct property."""
        self._cards[2].set_value("lct", value)

    @property
    def lcmt(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying input mass flow rate or tank pressure versus time. If the tank volume, TVOL, is nonzero the curve ID is assumed to be tank pressure versus time. If LCMT=0, then the inflator has to be modeled, see Card 4. During the dynamic relaxation phase the airbag is ignored unless the curve is flagged to act during dynamic relaxation.
        """ # nopep8
        return self._cards[2].get_value("lcmt")

    @lcmt.setter
    def lcmt(self, value: int) -> None:
        """Set the lcmt property."""
        self._cards[2].set_value("lcmt", value)

    @property
    def tvol(self) -> float:
        """Get or set the Tank volume which is required only for the tank pressure versus time curve, LCMT.
        """ # nopep8
        return self._cards[2].get_value("tvol")

    @tvol.setter
    def tvol(self, value: float) -> None:
        """Set the tvol property."""
        self._cards[2].set_value("tvol", value)

    @property
    def lcdt(self) -> int:
        """Get or set the Load curve for time rate of change of temperature (dT/dt) versus time.
        """ # nopep8
        return self._cards[2].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        """Set the lcdt property."""
        self._cards[2].set_value("lcdt", value)

    @property
    def iabt(self) -> typing.Optional[float]:
        """Get or set the Initial airbag temperature. (Optional, generally not defined).
        """ # nopep8
        return self._cards[2].get_value("iabt")

    @iabt.setter
    def iabt(self, value: float) -> None:
        """Set the iabt property."""
        self._cards[2].set_value("iabt", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the Vent orifice coefficient which applies to exit hole. Set to zero if LCC23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[3].set_value("c23", value)

    @property
    def lcc23(self) -> int:
        """Get or set the Load curve number defining the vent orifice coefficient which applies to exit hole as a function of time. A nonzero value for C23 overrides LCC23.
        """ # nopep8
        return self._cards[3].get_value("lcc23")

    @lcc23.setter
    def lcc23(self, value: int) -> None:
        """Set the lcc23 property."""
        self._cards[3].set_value("lcc23", value)

    @property
    def a23(self) -> typing.Optional[float]:
        """Get or set the Vent orifice area which applies to exit hole. Set to zero if LCA23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("a23")

    @a23.setter
    def a23(self, value: float) -> None:
        """Set the a23 property."""
        self._cards[3].set_value("a23", value)

    @property
    def lca23(self) -> int:
        """Get or set the Load curve number defining the vent orifice area which applies to exit hole as a function of absolute pressure. A nonzero value for A23 overrides LCA23.
        """ # nopep8
        return self._cards[3].get_value("lca23")

    @lca23.setter
    def lca23(self, value: int) -> None:
        """Set the lca23 property."""
        self._cards[3].set_value("lca23", value)

    @property
    def cp23(self) -> typing.Optional[float]:
        """Get or set the Orifice coefficient for leakage (fabric porosity). Set to zero if LCCP23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("cp23")

    @cp23.setter
    def cp23(self, value: float) -> None:
        """Set the cp23 property."""
        self._cards[3].set_value("cp23", value)

    @property
    def lccp23(self) -> int:
        """Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
        """ # nopep8
        return self._cards[3].get_value("lccp23")

    @lccp23.setter
    def lccp23(self, value: int) -> None:
        """Set the lccp23 property."""
        self._cards[3].set_value("lccp23", value)

    @property
    def ap23(self) -> float:
        """Get or set the Area for leakage (fabric porosity).
        """ # nopep8
        return self._cards[3].get_value("ap23")

    @ap23.setter
    def ap23(self, value: float) -> None:
        """Set the ap23 property."""
        self._cards[3].set_value("ap23", value)

    @property
    def lcap23(self) -> int:
        """Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
        """ # nopep8
        return self._cards[3].get_value("lcap23")

    @lcap23.setter
    def lcap23(self, value: int) -> None:
        """Set the lcap23 property."""
        self._cards[3].set_value("lcap23", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure.
        """ # nopep8
        return self._cards[4].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        """Set the pe property."""
        self._cards[4].set_value("pe", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Ambient density.
        """ # nopep8
        return self._cards[4].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[4].set_value("ro", value)

    @property
    def gc(self) -> typing.Optional[float]:
        """Get or set the Gravitational conversion constant (mandatory - no default). If consistent units are being used for all parameters in the airbag definition then unity should be input.
        """ # nopep8
        return self._cards[4].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        """Set the gc property."""
        self._cards[4].set_value("gc", value)

    @property
    def lcefr(self) -> int:
        """Get or set the Optional curve for exit flow rate versus (gauge) pressure.
        """ # nopep8
        return self._cards[4].get_value("lcefr")

    @lcefr.setter
    def lcefr(self, value: int) -> None:
        """Set the lcefr property."""
        self._cards[4].set_value("lcefr", value)

    @property
    def pover(self) -> float:
        """Get or set the Initial relative overpressure (gauge), P-over in control volume.
        """ # nopep8
        return self._cards[4].get_value("pover")

    @pover.setter
    def pover(self, value: float) -> None:
        """Set the pover property."""
        self._cards[4].set_value("pover", value)

    @property
    def ppop(self) -> float:
        """Get or set the Pop pressure: relative pressure (gauge) for initiating exit flow, P-pop.
        """ # nopep8
        return self._cards[4].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        """Set the ppop property."""
        self._cards[4].set_value("ppop", value)

    @property
    def opt(self) -> int:
        """Get or set the Fabric venting option, if nonzero CP23, LCCP23, AP23, and LCAP23 are set to zero.
        EQ.1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered (default).
        EQ.2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
        EQ.3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
        EQ.4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
        EQ.5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
        EQ.6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
        EQ.7: Simple porosity model. Blockage is not considered.
        EQ.8: Simple porosity model. Blockage of venting area due to contact is considered.
        """ # nopep8
        return self._cards[4].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""opt must be `None` or one of {1,2,3,4,5,6,7,8}.""")
        self._cards[4].set_value("opt", value)

    @property
    def knkdn(self) -> int:
        """Get or set the Optional load curve ID defining the knock down pressure scale factor versus time. This option only applies to jetting. The scale factor defined by this load curve scales the pressure applied to airbag segments which do not have a clear line-of-sight to the jet. Typically, at very early times this scale factor will be less than unity and equal to unity at later times. The full pressure is always applied to segments which can see the jets.
        """ # nopep8
        return self._cards[4].get_value("knkdn")

    @knkdn.setter
    def knkdn(self, value: int) -> None:
        """Set the knkdn property."""
        self._cards[4].set_value("knkdn", value)

    @property
    def ioc(self) -> typing.Optional[float]:
        """Get or set the Inflator orifice coefficient.
        """ # nopep8
        return self._cards[5].get_value("ioc")

    @ioc.setter
    def ioc(self, value: float) -> None:
        """Set the ioc property."""
        self._cards[5].set_value("ioc", value)

    @property
    def ioa(self) -> typing.Optional[float]:
        """Get or set the Inflator orifice area.
        """ # nopep8
        return self._cards[5].get_value("ioa")

    @ioa.setter
    def ioa(self, value: float) -> None:
        """Set the ioa property."""
        self._cards[5].set_value("ioa", value)

    @property
    def ivol(self) -> typing.Optional[float]:
        """Get or set the Inflator volume.
        """ # nopep8
        return self._cards[5].get_value("ivol")

    @ivol.setter
    def ivol(self, value: float) -> None:
        """Set the ivol property."""
        self._cards[5].set_value("ivol", value)

    @property
    def iro(self) -> typing.Optional[float]:
        """Get or set the Inflator density.
        """ # nopep8
        return self._cards[5].get_value("iro")

    @iro.setter
    def iro(self, value: float) -> None:
        """Set the iro property."""
        self._cards[5].set_value("iro", value)

    @property
    def it(self) -> typing.Optional[float]:
        """Get or set the Inflator temperature.
        """ # nopep8
        return self._cards[5].get_value("it")

    @it.setter
    def it(self, value: float) -> None:
        """Set the it property."""
        self._cards[5].set_value("it", value)

    @property
    def lcbf(self) -> typing.Optional[int]:
        """Get or set the Load curve defining burn fraction versus time.
        """ # nopep8
        return self._cards[5].get_value("lcbf")

    @lcbf.setter
    def lcbf(self, value: int) -> None:
        """Set the lcbf property."""
        self._cards[5].set_value("lcbf", value)

    @property
    def text(self) -> typing.Optional[float]:
        """Get or set the Ambient temperature.
        """ # nopep8
        return self._cards[6].get_value("text")

    @text.setter
    def text(self, value: float) -> None:
        """Set the text property."""
        self._cards[6].set_value("text", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the First heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK)
        """ # nopep8
        return self._cards[6].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[6].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Second heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK2)
        """ # nopep8
        return self._cards[6].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[6].set_value("b", value)

    @property
    def mw(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of inflator gas. (e.g., Kg/mole)
        """ # nopep8
        return self._cards[6].get_value("mw")

    @mw.setter
    def mw(self, value: float) -> None:
        """Set the mw property."""
        self._cards[6].set_value("mw", value)

    @property
    def gasc(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant of inflator gas. (e.g., 8.314 Joules/mole/oK)
        """ # nopep8
        return self._cards[6].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        """Set the gasc property."""
        self._cards[6].set_value("gasc", value)

    @property
    def hconv(self) -> float:
        """Get or set the Convection heat transfer coefficient
        """ # nopep8
        return self._cards[6].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        """Set the hconv property."""
        self._cards[6].set_value("hconv", value)

    @property
    def lct_link(self) -> DefineCurve:
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
    def lcmt_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmt:
                return kwd
        return None

    @lcmt_link.setter
    def lcmt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmt."""
        self.lcmt = value.lcid

    @property
    def lcdt_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdt:
                return kwd
        return None

    @lcdt_link.setter
    def lcdt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdt."""
        self.lcdt = value.lcid

    @property
    def lcc23_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcc23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcc23:
                return kwd
        return None

    @lcc23_link.setter
    def lcc23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcc23."""
        self.lcc23 = value.lcid

    @property
    def lca23_link(self) -> DefineCurve:
        """Get the DefineCurve object for lca23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lca23:
                return kwd
        return None

    @lca23_link.setter
    def lca23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lca23."""
        self.lca23 = value.lcid

    @property
    def lccp23_link(self) -> DefineCurve:
        """Get the DefineCurve object for lccp23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lccp23:
                return kwd
        return None

    @lccp23_link.setter
    def lccp23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lccp23."""
        self.lccp23 = value.lcid

    @property
    def lcap23_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcap23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcap23:
                return kwd
        return None

    @lcap23_link.setter
    def lcap23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcap23."""
        self.lcap23 = value.lcid

    @property
    def lcefr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcefr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcefr:
                return kwd
        return None

    @lcefr_link.setter
    def lcefr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcefr."""
        self.lcefr = value.lcid

    @property
    def knkdn_link(self) -> DefineCurve:
        """Get the DefineCurve object for knkdn."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.knkdn:
                return kwd
        return None

    @knkdn_link.setter
    def knkdn_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for knkdn."""
        self.knkdn = value.lcid

    @property
    def lcbf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcbf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcbf:
                return kwd
        return None

    @lcbf_link.setter
    def lcbf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcbf."""
        self.lcbf = value.lcid

