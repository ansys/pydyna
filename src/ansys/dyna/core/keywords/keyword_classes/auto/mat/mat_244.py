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

"""Module providing the Mat244 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT244_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("tunit", float, 40, 10, None),
    FieldSchema("crsh", int, 50, 10, 0),
    FieldSchema("phase", int, 60, 10, 0),
    FieldSchema("heat", int, 70, 10, 0),
)

_MAT244_CARD1 = (
    FieldSchema("lcy1", int, 0, 10, None),
    FieldSchema("lcy2", int, 10, 10, None),
    FieldSchema("lcy3", int, 20, 10, None),
    FieldSchema("lcy4", int, 30, 10, None),
    FieldSchema("lcy5", int, 40, 10, None),
    FieldSchema("kfer", float, 50, 10, None),
    FieldSchema("kper", float, 60, 10, None),
    FieldSchema("b", float, 70, 10, None),
)

_MAT244_CARD2 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("co", float, 10, 10, None),
    FieldSchema("mo", float, 20, 10, None),
    FieldSchema("cr", float, 30, 10, None),
    FieldSchema("ni", float, 40, 10, None),
    FieldSchema("mn", float, 50, 10, None),
    FieldSchema("si", float, 60, 10, None),
    FieldSchema("v", float, 70, 10, None),
)

_MAT244_CARD3 = (
    FieldSchema("w", float, 0, 10, None),
    FieldSchema("cu", float, 10, 10, None),
    FieldSchema("p", float, 20, 10, None),
    FieldSchema("al", float, 30, 10, None),
    FieldSchema("as_", float, 40, 10, None, "as"),
    FieldSchema("ti", float, 50, 10, None),
    FieldSchema("cwm", int, 60, 10, 0),
    FieldSchema("lctre", int, 70, 10, None),
)

_MAT244_CARD4 = (
    FieldSchema("thexp1", float, 0, 10, None),
    FieldSchema("thexp5", float, 10, 10, None),
    FieldSchema("lcth1", int, 20, 10, None),
    FieldSchema("lcth5", int, 30, 10, None),
    FieldSchema("tref", float, 40, 10, 273.15),
    FieldSchema("lat1", float, 50, 10, None),
    FieldSchema("lat5", float, 60, 10, None),
    FieldSchema("tabth", int, 70, 10, None),
)

_MAT244_CARD5 = (
    FieldSchema("qr2", float, 0, 10, None),
    FieldSchema("qr3", float, 10, 10, None),
    FieldSchema("qr4", float, 20, 10, None),
    FieldSchema("alpha", float, 30, 10, None),
    FieldSchema("grain", float, 40, 10, None),
    FieldSchema("toffe", float, 50, 10, None),
    FieldSchema("tofpe", float, 60, 10, None),
    FieldSchema("tofba", float, 70, 10, None),
)

_MAT244_CARD6 = (
    FieldSchema("plmem2", float, 0, 10, None),
    FieldSchema("plmem3", float, 10, 10, None),
    FieldSchema("plmem4", float, 20, 10, None),
    FieldSchema("plmem5", float, 30, 10, None),
    FieldSchema("strc", float, 40, 10, None),
    FieldSchema("strp", float, 50, 10, None),
    FieldSchema("react", int, 60, 10, 0),
    FieldSchema("temper", int, 70, 10, 0),
)

_MAT244_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat244(KeywordBase):
    """DYNA MAT_244 keyword"""

    keyword = "MAT"
    subkeyword = "244"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcy2": LinkType.DEFINE_CURVE,
        "lcy3": LinkType.DEFINE_CURVE,
        "lcy4": LinkType.DEFINE_CURVE,
        "lcy5": LinkType.DEFINE_CURVE,
        "lctre": LinkType.DEFINE_CURVE,
        "lcth1": LinkType.DEFINE_CURVE,
        "lcth5": LinkType.DEFINE_CURVE,
        "lcy1": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat244 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT244_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT244_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat244.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT244_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID, a unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus:
        GT.0.0: constant value is used
        LT.0.0: temperature dependent Young's modulus given by load curve ID = -E
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def tunit(self) -> typing.Optional[float]:
        """Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
        """ # nopep8
        return self._cards[0].get_value("tunit")

    @tunit.setter
    def tunit(self, value: float) -> None:
        """Set the tunit property."""
        self._cards[0].set_value("tunit", value)

    @property
    def crsh(self) -> int:
        """Get or set the Switch to use a simple and fast material model but with the actual phases active.
        EQ.0: The original model were phase transitions and trip is used.
        EQ.1: A more simpler and faster version is active. To use this the NIPS and/or NIPH on *DATABASE_EXTENT_BINARY must be set to 12 or greater. Please see remark 5 below for more information.
        """ # nopep8
        return self._cards[0].get_value("crsh")

    @crsh.setter
    def crsh(self, value: int) -> None:
        """Set the crsh property."""
        if value not in [0, 1, None]:
            raise Exception("""crsh must be `None` or one of {0,1}.""")
        self._cards[0].set_value("crsh", value)

    @property
    def phase(self) -> int:
        """Get or set the Switch to exclude middle phases from the simulation.
        EQ.0: All phases ACTIVE default)
        EQ.1: pearlite and bainite ACTIVE
        EQ.2: bainite ACTIVE
        EQ.3: ferrite and pearlite ACTIVE
        EQ.4: ferrite and bainite ACTIVE
        EQ.5: NO ACTIVE middle phases (only austenite -> martensite)
        """ # nopep8
        return self._cards[0].get_value("phase")

    @phase.setter
    def phase(self, value: int) -> None:
        """Set the phase property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""phase must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("phase", value)

    @property
    def heat(self) -> int:
        """Get or set the Switch to activate the heating algorithms
        EQ.0: Heating is not activated. That means that no transformation to Austenite is possible.
        EQ.1: Heating is activated: That means that only transformation to Austenite is possible.
        EQ.2: Automatic switching between cooling and heating. LS-DYNA checks the temperature gradient and calls the appropriate algorithms.
        For example, this can be used to simulate the heat affected zone during welding.
        LT.0: Switch between cooling and heating is defined by a time dependent load curve with id
        ABS(HEAT). The ordinate should be 1.0 when heating is applied and 0.0 if cooling is preferable.
        """ # nopep8
        return self._cards[0].get_value("heat")

    @heat.setter
    def heat(self, value: int) -> None:
        """Set the heat property."""
        self._cards[0].set_value("heat", value)

    @property
    def lcy1(self) -> typing.Optional[int]:
        """Get or set the Load curve or Table ID for austenite hardening.
        IF LCID input yield stress versus effective plastic strain. IF TABID.
        GT.0: 2D table. Input temperatures as table values and hardening curves as targets
        for those temperatures (see *DEFINE_TABLE) IF TABID.
        LT.0: 3D table. Input temperatures as main table values and strain rates as values
        for the sub tables, and hardening curves as targets for those strain rates.
        """ # nopep8
        return self._cards[1].get_value("lcy1")

    @lcy1.setter
    def lcy1(self, value: int) -> None:
        """Set the lcy1 property."""
        self._cards[1].set_value("lcy1", value)

    @property
    def lcy2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for ferrite hardening (stress versus eff. pl. str.)
        """ # nopep8
        return self._cards[1].get_value("lcy2")

    @lcy2.setter
    def lcy2(self, value: int) -> None:
        """Set the lcy2 property."""
        self._cards[1].set_value("lcy2", value)

    @property
    def lcy3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for pearlite hardening (stress versus eff. pl. str.)
        """ # nopep8
        return self._cards[1].get_value("lcy3")

    @lcy3.setter
    def lcy3(self, value: int) -> None:
        """Set the lcy3 property."""
        self._cards[1].set_value("lcy3", value)

    @property
    def lcy4(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for bainite hardening (stress versus eff. pl. str.)
        """ # nopep8
        return self._cards[1].get_value("lcy4")

    @lcy4.setter
    def lcy4(self, value: int) -> None:
        """Set the lcy4 property."""
        self._cards[1].set_value("lcy4", value)

    @property
    def lcy5(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for martensite hardening (stress versus eff. pl. str.)
        """ # nopep8
        return self._cards[1].get_value("lcy5")

    @lcy5.setter
    def lcy5(self, value: int) -> None:
        """Set the lcy5 property."""
        self._cards[1].set_value("lcy5", value)

    @property
    def kfer(self) -> typing.Optional[float]:
        """Get or set the Correction factor for boron in the ferrite reaction.
        """ # nopep8
        return self._cards[1].get_value("kfer")

    @kfer.setter
    def kfer(self, value: float) -> None:
        """Set the kfer property."""
        self._cards[1].set_value("kfer", value)

    @property
    def kper(self) -> typing.Optional[float]:
        """Get or set the Correction factor for boron in the pearlite reaction.
        """ # nopep8
        return self._cards[1].get_value("kper")

    @kper.setter
    def kper(self, value: float) -> None:
        """Set the kper property."""
        self._cards[1].set_value("kper", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Boron [weight %]
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Carbon [weight %]
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def co(self) -> typing.Optional[float]:
        """Get or set the Cobolt [weight %]
        """ # nopep8
        return self._cards[2].get_value("co")

    @co.setter
    def co(self, value: float) -> None:
        """Set the co property."""
        self._cards[2].set_value("co", value)

    @property
    def mo(self) -> typing.Optional[float]:
        """Get or set the Molybdenum [weight %]
        """ # nopep8
        return self._cards[2].get_value("mo")

    @mo.setter
    def mo(self, value: float) -> None:
        """Set the mo property."""
        self._cards[2].set_value("mo", value)

    @property
    def cr(self) -> typing.Optional[float]:
        """Get or set the Chromium [weight %]
        """ # nopep8
        return self._cards[2].get_value("cr")

    @cr.setter
    def cr(self, value: float) -> None:
        """Set the cr property."""
        self._cards[2].set_value("cr", value)

    @property
    def ni(self) -> typing.Optional[float]:
        """Get or set the Nickel [weight %]
        """ # nopep8
        return self._cards[2].get_value("ni")

    @ni.setter
    def ni(self, value: float) -> None:
        """Set the ni property."""
        self._cards[2].set_value("ni", value)

    @property
    def mn(self) -> typing.Optional[float]:
        """Get or set the Manganese [weight %]
        """ # nopep8
        return self._cards[2].get_value("mn")

    @mn.setter
    def mn(self, value: float) -> None:
        """Set the mn property."""
        self._cards[2].set_value("mn", value)

    @property
    def si(self) -> typing.Optional[float]:
        """Get or set the Silicon [weight %]
        """ # nopep8
        return self._cards[2].get_value("si")

    @si.setter
    def si(self, value: float) -> None:
        """Set the si property."""
        self._cards[2].set_value("si", value)

    @property
    def v(self) -> typing.Optional[float]:
        """Get or set the Vanadium [weight %]
        """ # nopep8
        return self._cards[2].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        """Set the v property."""
        self._cards[2].set_value("v", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Tungsten [weight %]
        """ # nopep8
        return self._cards[3].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[3].set_value("w", value)

    @property
    def cu(self) -> typing.Optional[float]:
        """Get or set the copper [weight %]
        """ # nopep8
        return self._cards[3].get_value("cu")

    @cu.setter
    def cu(self, value: float) -> None:
        """Set the cu property."""
        self._cards[3].set_value("cu", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Phosphorous [weight %]
        """ # nopep8
        return self._cards[3].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[3].set_value("p", value)

    @property
    def al(self) -> typing.Optional[float]:
        """Get or set the Aluminium [weight %]
        """ # nopep8
        return self._cards[3].get_value("al")

    @al.setter
    def al(self, value: float) -> None:
        """Set the al property."""
        self._cards[3].set_value("al", value)

    @property
    def as_(self) -> typing.Optional[float]:
        """Get or set the Arsenic [weight %]
        """ # nopep8
        return self._cards[3].get_value("as_")

    @as_.setter
    def as_(self, value: float) -> None:
        """Set the as_ property."""
        self._cards[3].set_value("as_", value)

    @property
    def ti(self) -> typing.Optional[float]:
        """Get or set the Titanium [weight %]
        """ # nopep8
        return self._cards[3].get_value("ti")

    @ti.setter
    def ti(self, value: float) -> None:
        """Set the ti property."""
        self._cards[3].set_value("ti", value)

    @property
    def cwm(self) -> int:
        """Get or set the Flag for computational welding mechanics input. One additional input card is read.
        EQ.1.0: Active
        EQ.0.0: Inactive
        """ # nopep8
        return self._cards[3].get_value("cwm")

    @cwm.setter
    def cwm(self, value: int) -> None:
        """Set the cwm property."""
        if value not in [0, 1, None]:
            raise Exception("""cwm must be `None` or one of {0,1}.""")
        self._cards[3].set_value("cwm", value)

    @property
    def lctre(self) -> typing.Optional[int]:
        """Get or set the Load curve for transformation induced strains. See Remark 13 for more information.
        """ # nopep8
        return self._cards[3].get_value("lctre")

    @lctre.setter
    def lctre(self, value: int) -> None:
        """Set the lctre property."""
        self._cards[3].set_value("lctre", value)

    @property
    def thexp1(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion in austenite
        """ # nopep8
        return self._cards[4].get_value("thexp1")

    @thexp1.setter
    def thexp1(self, value: float) -> None:
        """Set the thexp1 property."""
        self._cards[4].set_value("thexp1", value)

    @property
    def thexp5(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion in martensite
        """ # nopep8
        return self._cards[4].get_value("thexp5")

    @thexp5.setter
    def thexp5(self, value: float) -> None:
        """Set the thexp5 property."""
        self._cards[4].set_value("thexp5", value)

    @property
    def lcth1(self) -> typing.Optional[int]:
        """Get or set the Load curve for the thermal expansion coefficient for austenite:
        LT.0.0: curve ID = -LA and TREF is used as reference temperature
        GT.0.0: curve ID = LA
        """ # nopep8
        return self._cards[4].get_value("lcth1")

    @lcth1.setter
    def lcth1(self, value: int) -> None:
        """Set the lcth1 property."""
        self._cards[4].set_value("lcth1", value)

    @property
    def lcth5(self) -> typing.Optional[int]:
        """Get or set the Load curve for the thermal expansion coefficient for martensite:
        LT.0.0: curve ID = -LA and TREF is used as reference temperature
        GT.0.0: curve ID = LA
        """ # nopep8
        return self._cards[4].get_value("lcth5")

    @lcth5.setter
    def lcth5(self, value: int) -> None:
        """Set the lcth5 property."""
        self._cards[4].set_value("lcth5", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature for thermal expansion. Used if and only if LA.LT.0.0 or/and LM.LT.0.0
        """ # nopep8
        return self._cards[4].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[4].set_value("tref", value)

    @property
    def lat1(self) -> typing.Optional[float]:
        """Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
        """ # nopep8
        return self._cards[4].get_value("lat1")

    @lat1.setter
    def lat1(self, value: float) -> None:
        """Set the lat1 property."""
        self._cards[4].set_value("lat1", value)

    @property
    def lat5(self) -> typing.Optional[float]:
        """Get or set the Latent heat for the decomposition of austenite into martensite
        """ # nopep8
        return self._cards[4].get_value("lat5")

    @lat5.setter
    def lat5(self, value: float) -> None:
        """Set the lat5 property."""
        self._cards[4].set_value("lat5", value)

    @property
    def tabth(self) -> typing.Optional[int]:
        """Get or set the Table definition for thermal expansion coefficient. With this option active THEXP1,
        THEXP2, LCTH1 and LCTH5 are ignored. See remarks for more information how to input this table.
        GT.0: A table for instantaneous thermal expansion (TREF is ignored).
        LT.0: A table with thermal expansion with reference to TREF.
        """ # nopep8
        return self._cards[4].get_value("tabth")

    @tabth.setter
    def tabth(self, value: int) -> None:
        """Set the tabth property."""
        self._cards[4].set_value("tabth", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R = 8.314472 [J/mol K].
        """ # nopep8
        return self._cards[5].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        """Set the qr2 property."""
        self._cards[5].set_value("qr2", value)

    @property
    def qr3(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].
        """ # nopep8
        return self._cards[5].get_value("qr3")

    @qr3.setter
    def qr3(self, value: float) -> None:
        """Set the qr3 property."""
        self._cards[5].set_value("qr3", value)

    @property
    def qr4(self) -> typing.Optional[float]:
        """Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].
        """ # nopep8
        return self._cards[5].get_value("qr4")

    @qr4.setter
    def qr4(self, value: float) -> None:
        """Set the qr4 property."""
        self._cards[5].set_value("qr4", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below TSMART, whereas a value of 0.033 means a 99.9% transformation.
        """ # nopep8
        return self._cards[5].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[5].set_value("alpha", value)

    @property
    def grain(self) -> typing.Optional[float]:
        """Get or set the ASTM grain size number for austenite, usually a number between 7 and 11.
        """ # nopep8
        return self._cards[5].get_value("grain")

    @grain.setter
    def grain(self, value: float) -> None:
        """Set the grain property."""
        self._cards[5].set_value("grain", value)

    @property
    def toffe(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction.
        """ # nopep8
        return self._cards[5].get_value("toffe")

    @toffe.setter
    def toffe(self, value: float) -> None:
        """Set the toffe property."""
        self._cards[5].set_value("toffe", value)

    @property
    def tofpe(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction.
        """ # nopep8
        return self._cards[5].get_value("tofpe")

    @tofpe.setter
    def tofpe(self, value: float) -> None:
        """Set the tofpe property."""
        self._cards[5].set_value("tofpe", value)

    @property
    def tofba(self) -> typing.Optional[float]:
        """Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction.
        """ # nopep8
        return self._cards[5].get_value("tofba")

    @tofba.setter
    def tofba(self, value: float) -> None:
        """Set the tofba property."""
        self._cards[5].set_value("tofba", value)

    @property
    def plmem2(self) -> typing.Optional[float]:
        """Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
        """ # nopep8
        return self._cards[6].get_value("plmem2")

    @plmem2.setter
    def plmem2(self, value: float) -> None:
        """Set the plmem2 property."""
        self._cards[6].set_value("plmem2", value)

    @property
    def plmem3(self) -> typing.Optional[float]:
        """Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the pearlite phase and a value of 0 means that nothing is transferred.
        """ # nopep8
        return self._cards[6].get_value("plmem3")

    @plmem3.setter
    def plmem3(self, value: float) -> None:
        """Set the plmem3 property."""
        self._cards[6].set_value("plmem3", value)

    @property
    def plmem4(self) -> typing.Optional[float]:
        """Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the bainite phase and a value of 0 means that nothing is transferred.
        """ # nopep8
        return self._cards[6].get_value("plmem4")

    @plmem4.setter
    def plmem4(self, value: float) -> None:
        """Set the plmem4 property."""
        self._cards[6].set_value("plmem4", value)

    @property
    def plmem5(self) -> typing.Optional[float]:
        """Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the martensite phase and a value of 0 means that nothing is transferred.
        """ # nopep8
        return self._cards[6].get_value("plmem5")

    @plmem5.setter
    def plmem5(self, value: float) -> None:
        """Set the plmem5 property."""
        self._cards[6].set_value("plmem5", value)

    @property
    def strc(self) -> typing.Optional[float]:
        """Get or set the Effective strain rate parameter C.
        STRC.LT.0.0: load curve id = -STRC
        STRC.GT.0.0: constant value
        STRC.EQ.0.0: strain rate NOT active
        """ # nopep8
        return self._cards[6].get_value("strc")

    @strc.setter
    def strc(self, value: float) -> None:
        """Set the strc property."""
        self._cards[6].set_value("strc", value)

    @property
    def strp(self) -> typing.Optional[float]:
        """Get or set the Effective strain rate parameter P.
        STRP.LT.0.0: load curve id = -STRP
        STRP.GT.0.0: constant value
        STRP.EQ.0.0: strain rate NOT active
        """ # nopep8
        return self._cards[6].get_value("strp")

    @strp.setter
    def strp(self, value: float) -> None:
        """Set the strp property."""
        self._cards[6].set_value("strp", value)

    @property
    def react(self) -> int:
        """Get or set the Flag for advanced reaction kinetics input.
        One additional input card is read.
        EQ.1.0: Active
        EQ.0.0: Inactive
        """ # nopep8
        return self._cards[6].get_value("react")

    @react.setter
    def react(self, value: int) -> None:
        """Set the react property."""
        if value not in [0, 1, None]:
            raise Exception("""react must be `None` or one of {0,1}.""")
        self._cards[6].set_value("react", value)

    @property
    def temper(self) -> int:
        """Get or set the Flag for tempering input. One additional input card is read.
        EQ.1.0: Active
        EQ.0.0: Inactive
        """ # nopep8
        return self._cards[6].get_value("temper")

    @temper.setter
    def temper(self, value: int) -> None:
        """Set the temper property."""
        if value not in [0, 1, None]:
            raise Exception("""temper must be `None` or one of {0,1}.""")
        self._cards[6].set_value("temper", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcy2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcy2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcy2:
                return kwd
        return None

    @lcy2_link.setter
    def lcy2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcy2."""
        self.lcy2 = value.lcid

    @property
    def lcy3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcy3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcy3:
                return kwd
        return None

    @lcy3_link.setter
    def lcy3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcy3."""
        self.lcy3 = value.lcid

    @property
    def lcy4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcy4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcy4:
                return kwd
        return None

    @lcy4_link.setter
    def lcy4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcy4."""
        self.lcy4 = value.lcid

    @property
    def lcy5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcy5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcy5:
                return kwd
        return None

    @lcy5_link.setter
    def lcy5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcy5."""
        self.lcy5 = value.lcid

    @property
    def lctre_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lctre."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lctre:
                return kwd
        return None

    @lctre_link.setter
    def lctre_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lctre."""
        self.lctre = value.lcid

    @property
    def lcth1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcth1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcth1:
                return kwd
        return None

    @lcth1_link.setter
    def lcth1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcth1."""
        self.lcth1 = value.lcid

    @property
    def lcth5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcth5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcth5:
                return kwd
        return None

    @lcth5_link.setter
    def lcth5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcth5."""
        self.lcth5 = value.lcid

    @property
    def lcy1_link(self) -> typing.Optional[KeywordBase]:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcy1."""
        if self.deck is None:
            return None
        field_value = self.lcy1
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcy1_link.setter
    def lcy1_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcy1."""
        if hasattr(value, "lcid"):
            self.lcy1 = value.lcid
        elif hasattr(value, "tbid"):
            self.lcy1 = value.tbid

