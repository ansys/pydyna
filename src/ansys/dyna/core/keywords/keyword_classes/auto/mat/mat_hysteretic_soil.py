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

"""Module providing the MatHystereticSoil class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATHYSTERETICSOIL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k0", float, 20, 10, None),
    FieldSchema("p0", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("a0", float, 50, 10, 1.0),
    FieldSchema("a1", float, 60, 10, None),
    FieldSchema("a2", float, 70, 10, None),
)

_MATHYSTERETICSOIL_CARD1 = (
    FieldSchema("df", float, 0, 10, None),
    FieldSchema("rp", float, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sflc", float, 30, 10, 1.0),
    FieldSchema("dil_a", float, 40, 10, None),
    FieldSchema("dil_b", float, 50, 10, None),
    FieldSchema("dil_c", float, 60, 10, None),
    FieldSchema("dil_d", float, 70, 10, None),
)

_MATHYSTERETICSOIL_CARD2 = (
    FieldSchema("gam1", float, 0, 10, None),
    FieldSchema("gam2", float, 10, 10, None),
    FieldSchema("gam3", float, 20, 10, None),
    FieldSchema("gam4", float, 30, 10, None),
    FieldSchema("gam5", float, 40, 10, None),
    FieldSchema("lcd", int, 50, 10, None),
    FieldSchema("lcsr", int, 60, 10, None),
    FieldSchema("pinit", int, 70, 10, 0),
)

_MATHYSTERETICSOIL_CARD3 = (
    FieldSchema("tau1", float, 0, 10, None),
    FieldSchema("tau2", float, 10, 10, None),
    FieldSchema("tau3", float, 20, 10, None),
    FieldSchema("tau4", float, 30, 10, None),
    FieldSchema("tau5", float, 40, 10, None),
    FieldSchema("flag5_", int, 50, 10, None, "flag5 "),
)

_MATHYSTERETICSOIL_CARD4 = (
    FieldSchema("sigth", float, 0, 10, None),
    FieldSchema("sigr", float, 10, 10, None),
    FieldSchema("chi", float, 20, 10, None),
)

_MATHYSTERETICSOIL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatHystereticSoil(KeywordBase):
    """DYNA MAT_HYSTERETIC_SOIL keyword"""

    keyword = "MAT"
    subkeyword = "HYSTERETIC_SOIL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatHystereticSoil class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICSOIL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICSOIL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICSOIL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICSOIL_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICSOIL_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatHystereticSoil.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATHYSTERETICSOIL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def k0(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus at the reference pressure.
        """ # nopep8
        return self._cards[0].get_value("k0")

    @k0.setter
    def k0(self, value: float) -> None:
        """Set the k0 property."""
        self._cards[0].set_value("k0", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Cut-off/datum pressure (must be 0 <= i.e tensile). Below thie pressure, stiffness and strength disappears; this is also the zero pressure for pressure-varying properties.
        """ # nopep8
        return self._cards[0].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[0].set_value("p0", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Exponent for pressure-sensitive modulis b. b must lie in the range 0 <= b < 1. Values close to 1 are not recommended because the pressure becomes indeterminate.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def a0(self) -> float:
        """Get or set the Yield function constant ao (default = 1.0), see Mateiral Type 5.
        """ # nopep8
        return self._cards[0].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        """Set the a0 property."""
        self._cards[0].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Yield function constant a1 (default = 0.0), see Material Type 5.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Yield function constant a2 (default = 0.0), see Material Type 5.
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def df(self) -> typing.Optional[float]:
        """Get or set the Damping factor. Must be in the range 0 <= df <=1:
        EQ.0: no damping,
        EQ.1: maximum damping.
        """ # nopep8
        return self._cards[1].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        """Set the df property."""
        self._cards[1].set_value("df", value)

    @property
    def rp(self) -> typing.Optional[float]:
        """Get or set the Reference pressure for following input data.
        """ # nopep8
        return self._cards[1].get_value("rp")

    @rp.setter
    def rp(self, value: float) -> None:
        """Set the rp property."""
        self._cards[1].set_value("rp", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining shear strain verses shear stress. Upto 20 points may be defined in the load curve. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def sflc(self) -> float:
        """Get or set the Scale factor to apply to shear stress in LCID.
        """ # nopep8
        return self._cards[1].get_value("sflc")

    @sflc.setter
    def sflc(self, value: float) -> None:
        """Set the sflc property."""
        self._cards[1].set_value("sflc", value)

    @property
    def dil_a(self) -> typing.Optional[float]:
        """Get or set the Dilation parameter A.
        """ # nopep8
        return self._cards[1].get_value("dil_a")

    @dil_a.setter
    def dil_a(self, value: float) -> None:
        """Set the dil_a property."""
        self._cards[1].set_value("dil_a", value)

    @property
    def dil_b(self) -> typing.Optional[float]:
        """Get or set the Dilation parameter B.
        """ # nopep8
        return self._cards[1].get_value("dil_b")

    @dil_b.setter
    def dil_b(self, value: float) -> None:
        """Set the dil_b property."""
        self._cards[1].set_value("dil_b", value)

    @property
    def dil_c(self) -> typing.Optional[float]:
        """Get or set the Dilation parameter C.
        """ # nopep8
        return self._cards[1].get_value("dil_c")

    @dil_c.setter
    def dil_c(self, value: float) -> None:
        """Set the dil_c property."""
        self._cards[1].set_value("dil_c", value)

    @property
    def dil_d(self) -> typing.Optional[float]:
        """Get or set the Dilation parameter D.
        """ # nopep8
        return self._cards[1].get_value("dil_d")

    @dil_d.setter
    def dil_d(self, value: float) -> None:
        """Set the dil_d property."""
        self._cards[1].set_value("dil_d", value)

    @property
    def gam1(self) -> typing.Optional[float]:
        """Get or set the Shear strain gamma-1 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[2].get_value("gam1")

    @gam1.setter
    def gam1(self, value: float) -> None:
        """Set the gam1 property."""
        self._cards[2].set_value("gam1", value)

    @property
    def gam2(self) -> typing.Optional[float]:
        """Get or set the Shear strain gamma-2 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[2].get_value("gam2")

    @gam2.setter
    def gam2(self, value: float) -> None:
        """Set the gam2 property."""
        self._cards[2].set_value("gam2", value)

    @property
    def gam3(self) -> typing.Optional[float]:
        """Get or set the Shear strain gamma-3 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[2].get_value("gam3")

    @gam3.setter
    def gam3(self, value: float) -> None:
        """Set the gam3 property."""
        self._cards[2].set_value("gam3", value)

    @property
    def gam4(self) -> typing.Optional[float]:
        """Get or set the Shear strain gamma-4 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[2].get_value("gam4")

    @gam4.setter
    def gam4(self, value: float) -> None:
        """Set the gam4 property."""
        self._cards[2].set_value("gam4", value)

    @property
    def gam5(self) -> typing.Optional[float]:
        """Get or set the Shear strain gamma-5 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[2].get_value("gam5")

    @gam5.setter
    def gam5(self, value: float) -> None:
        """Set the gam5 property."""
        self._cards[2].set_value("gam5", value)

    @property
    def lcd(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID defining damping ratio of hysteresis at different strain amplitudes (overrides Masing rules for unload/reload).  The x-axis is shear strain; the y-axis is the damping ratio (such as 0.05 for 5% damping). The strains (x-axis values) of curve LCD must be identical to those of curve LCID.
        """ # nopep8
        return self._cards[2].get_value("lcd")

    @lcd.setter
    def lcd(self, value: int) -> None:
        """Set the lcd property."""
        self._cards[2].set_value("lcd", value)

    @property
    def lcsr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining plastic strain rate scaling effect on yield stress. See *DEFINE_CURVE.  The x-axis is plastic strain rate; the y-axis is the yield enhancement factor.
        """ # nopep8
        return self._cards[2].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        """Set the lcsr property."""
        self._cards[2].set_value("lcsr", value)

    @property
    def pinit(self) -> int:
        """Get or set the Flag for pressure sensitivity (B and A0, A1, A2 equations):
        EQ.0: Use current pressure (will vary during the analysis)
        EQ.1: Use pressure from initial stress state
        EQ.2: Use initial "plane stress" pressure
        EQ.3: Use (compressive) initial vertical stres.
        """ # nopep8
        return self._cards[2].get_value("pinit")

    @pinit.setter
    def pinit(self, value: int) -> None:
        """Set the pinit property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""pinit must be `None` or one of {0,1,2,3}.""")
        self._cards[2].set_value("pinit", value)

    @property
    def tau1(self) -> typing.Optional[float]:
        """Get or set the Shear stress at gamma-1 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[3].get_value("tau1")

    @tau1.setter
    def tau1(self, value: float) -> None:
        """Set the tau1 property."""
        self._cards[3].set_value("tau1", value)

    @property
    def tau2(self) -> typing.Optional[float]:
        """Get or set the Shear stress at gamma-2 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[3].get_value("tau2")

    @tau2.setter
    def tau2(self, value: float) -> None:
        """Set the tau2 property."""
        self._cards[3].set_value("tau2", value)

    @property
    def tau3(self) -> typing.Optional[float]:
        """Get or set the Shear stress at gamma-3 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[3].get_value("tau3")

    @tau3.setter
    def tau3(self, value: float) -> None:
        """Set the tau3 property."""
        self._cards[3].set_value("tau3", value)

    @property
    def tau4(self) -> typing.Optional[float]:
        """Get or set the Shear stress at gamma-4 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[3].get_value("tau4")

    @tau4.setter
    def tau4(self, value: float) -> None:
        """Set the tau4 property."""
        self._cards[3].set_value("tau4", value)

    @property
    def tau5(self) -> typing.Optional[float]:
        """Get or set the Shear stress at gamma-5 (ignored if LCID is non zero).
        """ # nopep8
        return self._cards[3].get_value("tau5")

    @tau5.setter
    def tau5(self, value: float) -> None:
        """Set the tau5 property."""
        self._cards[3].set_value("tau5", value)

    @property
    def flag5_(self) -> typing.Optional[int]:
        """Get or set the If FLAG5 = 1, optional Card 5 will be read. .
        """ # nopep8
        return self._cards[3].get_value("flag5_")

    @flag5_.setter
    def flag5_(self, value: int) -> None:
        """Set the flag5_ property."""
        self._cards[3].set_value("flag5_", value)

    @property
    def sigth(self) -> typing.Optional[float]:
        """Get or set the Threshold shear stress ratio for cyclic degradation
        """ # nopep8
        return self._cards[4].get_value("sigth")

    @sigth.setter
    def sigth(self, value: float) -> None:
        """Set the sigth property."""
        self._cards[4].set_value("sigth", value)

    @property
    def sigr(self) -> typing.Optional[float]:
        """Get or set the Residual shear stress ratio for cyclic degradation
        """ # nopep8
        return self._cards[4].get_value("sigr")

    @sigr.setter
    def sigr(self, value: float) -> None:
        """Set the sigr property."""
        self._cards[4].set_value("sigr", value)

    @property
    def chi(self) -> typing.Optional[float]:
        """Get or set the Cyclic degradation parameter
        """ # nopep8
        return self._cards[4].get_value("chi")

    @chi.setter
    def chi(self, value: float) -> None:
        """Set the chi property."""
        self._cards[4].set_value("chi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

