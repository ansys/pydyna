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

"""Module providing the MatCohesiveMixedModeElastoplasticRateFunctions3Modes class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("roflg", int, 20, 10, 0),
    FieldSchema("intfail", float, 30, 10, None),
    FieldSchema("emod", float, 40, 10, None),
    FieldSchema("gmod", float, 50, 10, None),
    FieldSchema("thick", float, 60, 10, None),
    FieldSchema("inicrt", float, 70, 10, 0.0),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD1 = (
    FieldSchema("g1c_0", int, 0, 10, None),
    FieldSchema("g1c_inf", float, 10, 10, None),
    FieldSchema("edot_g1", float, 20, 10, None),
    FieldSchema("t0", int, 30, 10, None),
    FieldSchema("t1", float, 40, 10, None),
    FieldSchema("edot_t", float, 50, 10, None),
    FieldSchema("fg1", int, 60, 10, None),
    FieldSchema("lcg1c", int, 70, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD2 = (
    FieldSchema("g2c_0", int, 0, 10, None),
    FieldSchema("g2c_inf", float, 10, 10, None),
    FieldSchema("edot_g2", float, 20, 10, None),
    FieldSchema("s0", int, 30, 10, None),
    FieldSchema("s1", float, 40, 10, None),
    FieldSchema("edot_s", float, 50, 10, None),
    FieldSchema("fg2", float, 60, 10, None),
    FieldSchema("lcg2c", int, 70, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD3 = (
    FieldSchema("g3c_0", int, 0, 10, None),
    FieldSchema("g3c_inf", float, 10, 10, None),
    FieldSchema("edot_g3", float, 20, 10, None),
    FieldSchema("r0", int, 30, 10, None),
    FieldSchema("r1", float, 40, 10, None),
    FieldSchema("edot_r", float, 50, 10, None),
    FieldSchema("fg3", int, 60, 10, None),
    FieldSchema("lcg3c", int, 70, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD4 = (
    FieldSchema("gmod3", int, 0, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD5 = (
    FieldSchema("rfiltf", float, 0, 10, None),
    FieldSchema("compy", float, 10, 10, None),
    FieldSchema("smolim", float, 20, 10, None),
    FieldSchema("xmu", float, 30, 10, 1.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("usrfail", int, 50, 10, 0),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD6 = (
    FieldSchema("usrv1", float, 0, 10, None),
    FieldSchema("usrv2", float, 10, 10, None),
    FieldSchema("usrv3", float, 20, 10, None),
    FieldSchema("usrv4", float, 30, 10, None),
    FieldSchema("usrv5", float, 40, 10, None),
    FieldSchema("usrv6", float, 50, 10, None),
    FieldSchema("usrv7", float, 60, 10, None),
    FieldSchema("usrv8", float, 70, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD7 = (
    FieldSchema("usrv9", float, 0, 10, None),
    FieldSchema("usrv10", float, 10, 10, None),
    FieldSchema("usrv11", float, 20, 10, None),
    FieldSchema("usrv12", float, 30, 10, None),
    FieldSchema("usrv13", float, 40, 10, None),
    FieldSchema("usrv14", float, 50, 10, None),
    FieldSchema("usrv15", float, 60, 10, None),
    FieldSchema("usrv16", float, 70, 10, None),
)

_MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCohesiveMixedModeElastoplasticRateFunctions3Modes(KeywordBase):
    """DYNA MAT_COHESIVE_MIXED_MODE_ELASTOPLASTIC_RATE_FUNCTIONS_3MODES keyword"""

    keyword = "MAT"
    subkeyword = "COHESIVE_MIXED_MODE_ELASTOPLASTIC_RATE_FUNCTIONS_3MODES"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcg1c": LinkType.DEFINE_CURVE,
        "lcg2c": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatCohesiveMixedModeElastoplasticRateFunctions3Modes class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_CARD7,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatCohesiveMixedModeElastoplasticRateFunctions3Modes._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCOHESIVEMIXEDMODEELASTOPLASTICRATEFUNCTIONS3MODES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
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
    def roflg(self) -> int:
        """Get or set the Flag for whether density is specified per unit area or volume. ROFLG=0 specified density per unit volume (default), and ROFLG=1 specifies the density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        """Set the roflg property."""
        if value not in [0, 1, None]:
            raise Exception("""roflg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[float]:
        """Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
        LT.0.0: Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
        EQ.0.0: Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
        GT.0.0: Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: float) -> None:
        """Set the intfail property."""
        self._cards[0].set_value("intfail", value)

    @property
    def emod(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of the material (Mode I).
        GT.0.0: Constant value.Curve ID for THERMAL keyword option.Function ID for the FUNCTIONS keyword option.
        LT.0.0 : Load curve ID = | MOD|,  which defines Young's modulus as a function of strain rate.
        """ # nopep8
        return self._cards[0].get_value("emod")

    @emod.setter
    def emod(self, value: float) -> None:
        """Set the emod property."""
        self._cards[0].set_value("emod", value)

    @property
    def gmod(self) -> typing.Optional[float]:
        """Get or set the The shear modulus of the material (Mode II).
        GT.0.0: Constant value.Curve ID for THERMAL keyword option.Function ID for the FUNCTIONS keyword option.
        LT.0.0 : Load curve ID = |GMOD|,  which defines shear modulus as a function of strain rate.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        """Set the gmod property."""
        self._cards[0].set_value("gmod", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the GT.0.0: Cohesive thickness. LE.0.0: Initial thickness is calculated from nodal coordinates.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[0].set_value("thick", value)

    @property
    def inicrt(self) -> float:
        """Get or set the Yield and damage initiation criterion:
        EQ.0.0: quadratic nominal stress(default)
        EQ.1.0: maximum nominal stress.
        LT.0.0: mixed mode with flexible exponent |INICRT
        """ # nopep8
        return self._cards[0].get_value("inicrt")

    @inicrt.setter
    def inicrt(self, value: float) -> None:
        """Set the inicrt property."""
        self._cards[0].set_value("inicrt", value)

    @property
    def g1c_0(self) -> typing.Optional[int]:
        """Get or set the GT 0.0: G1C_0 is a function ID . Energy release rate GIC in Mode I. LE. 0.0: Lower bound value of rate-dependent GIC.
        """ # nopep8
        return self._cards[1].get_value("g1c_0")

    @g1c_0.setter
    def g1c_0(self, value: int) -> None:
        """Set the g1c_0 property."""
        self._cards[1].set_value("g1c_0", value)

    @property
    def g1c_inf(self) -> typing.Optional[float]:
        """Get or set the Upper bound value of rate-dependent GIC (only considered if G1C_0<0).
        """ # nopep8
        return self._cards[1].get_value("g1c_inf")

    @g1c_inf.setter
    def g1c_inf(self, value: float) -> None:
        """Set the g1c_inf property."""
        self._cards[1].set_value("g1c_inf", value)

    @property
    def edot_g1(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIC (only considered if G1C_0<0).
        """ # nopep8
        return self._cards[1].get_value("edot_g1")

    @edot_g1.setter
    def edot_g1(self, value: float) -> None:
        """Set the edot_g1 property."""
        self._cards[1].set_value("edot_g1", value)

    @property
    def t0(self) -> typing.Optional[int]:
        """Get or set the T0 is a function ID
        """ # nopep8
        return self._cards[1].get_value("t0")

    @t0.setter
    def t0(self, value: int) -> None:
        """Set the t0 property."""
        self._cards[1].set_value("t0", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Parameter T1, only considered if T0 < 0:
        GT.0.0: Quadratic logarithmic model
        LT.0.0: Linear logarithmic model.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[1].set_value("t1", value)

    @property
    def edot_t(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode I (only considered if T0<0).
        """ # nopep8
        return self._cards[1].get_value("edot_t")

    @edot_t.setter
    def edot_t(self, value: float) -> None:
        """Set the edot_t property."""
        self._cards[1].set_value("edot_t", value)

    @property
    def fg1(self) -> typing.Optional[int]:
        """Get or set the An function ID
        """ # nopep8
        return self._cards[1].get_value("fg1")

    @fg1.setter
    def fg1(self, value: int) -> None:
        """Set the fg1 property."""
        self._cards[1].set_value("fg1", value)

    @property
    def lcg1c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines fracture energy GIC as a function of cohesive element thickness. G1C_0 and G1C_INF are ignored in this case.
        """ # nopep8
        return self._cards[1].get_value("lcg1c")

    @lcg1c.setter
    def lcg1c(self, value: int) -> None:
        """Set the lcg1c property."""
        self._cards[1].set_value("lcg1c", value)

    @property
    def g2c_0(self) -> typing.Optional[int]:
        """Get or set the An function ID .
        """ # nopep8
        return self._cards[2].get_value("g2c_0")

    @g2c_0.setter
    def g2c_0(self, value: int) -> None:
        """Set the g2c_0 property."""
        self._cards[2].set_value("g2c_0", value)

    @property
    def g2c_inf(self) -> typing.Optional[float]:
        """Get or set the Upper bound value of GIIC (only considered if G2C_0<0).
        """ # nopep8
        return self._cards[2].get_value("g2c_inf")

    @g2c_inf.setter
    def g2c_inf(self, value: float) -> None:
        """Set the g2c_inf property."""
        self._cards[2].set_value("g2c_inf", value)

    @property
    def edot_g2(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIIC (only considered if G2C_0<0).
        """ # nopep8
        return self._cards[2].get_value("edot_g2")

    @edot_g2.setter
    def edot_g2(self, value: float) -> None:
        """Set the edot_g2 property."""
        self._cards[2].set_value("edot_g2", value)

    @property
    def s0(self) -> typing.Optional[int]:
        """Get or set the An function ID
        """ # nopep8
        return self._cards[2].get_value("s0")

    @s0.setter
    def s0(self, value: int) -> None:
        """Set the s0 property."""
        self._cards[2].set_value("s0", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Parameter S1, only considered if S0<0:
        GT.0.0: Quadratic logarithmic model is applied
        LT.0.0: Linear logarithmic model is applied.
        """ # nopep8
        return self._cards[2].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[2].set_value("s1", value)

    @property
    def edot_s(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode II (only considered if S0<0).
        """ # nopep8
        return self._cards[2].get_value("edot_s")

    @edot_s.setter
    def edot_s(self, value: float) -> None:
        """Set the edot_s property."""
        self._cards[2].set_value("edot_s", value)

    @property
    def fg2(self) -> typing.Optional[float]:
        """Get or set the Describes the trilinear shape of the traction-separation law in Mode II (see remarks). It is a load curve ID for the THERMAL keyword option. It is a function ID for the FUNCTIONS keyword option.
        GT.0.0: FG2 is the ratio of fracture energies, G_(II,P) / G_IIC.
        LT.0.0 : | FG2 | is the ratio of displacements, (δ_t2 - δ_t1) / (δ_tf - δ_t1).
        """ # nopep8
        return self._cards[2].get_value("fg2")

    @fg2.setter
    def fg2(self, value: float) -> None:
        """Set the fg2 property."""
        self._cards[2].set_value("fg2", value)

    @property
    def lcg2c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the load curve that defines fracture energy GIIC as a function of cohesive element thickness. G2C_0 and G2C_INF are ignored in that case.
        """ # nopep8
        return self._cards[2].get_value("lcg2c")

    @lcg2c.setter
    def lcg2c(self, value: int) -> None:
        """Set the lcg2c property."""
        self._cards[2].set_value("lcg2c", value)

    @property
    def g3c_0(self) -> typing.Optional[int]:
        """Get or set the GT.0.0:a function ID
        LE.0.0: Lower bound value of rate-dependent G_IIIC
        """ # nopep8
        return self._cards[3].get_value("g3c_0")

    @g3c_0.setter
    def g3c_0(self, value: int) -> None:
        """Set the g3c_0 property."""
        self._cards[3].set_value("g3c_0", value)

    @property
    def g3c_inf(self) -> typing.Optional[float]:
        """Get or set the Upper bound value of rate-dependent G_IIIC (only considered if G3C_0 < 0).
        """ # nopep8
        return self._cards[3].get_value("g3c_inf")

    @g3c_inf.setter
    def g3c_inf(self, value: float) -> None:
        """Set the g3c_inf property."""
        self._cards[3].set_value("g3c_inf", value)

    @property
    def edot_g3(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of G_IIIC (only considered if G1C_0 < 0)
        """ # nopep8
        return self._cards[3].get_value("edot_g3")

    @edot_g3.setter
    def edot_g3(self, value: float) -> None:
        """Set the edot_g3 property."""
        self._cards[3].set_value("edot_g3", value)

    @property
    def r0(self) -> typing.Optional[int]:
        """Get or set the GT.0.0:a function ID
        LT.0.0: Rate - dependency is considered.
        """ # nopep8
        return self._cards[3].get_value("r0")

    @r0.setter
    def r0(self, value: int) -> None:
        """Set the r0 property."""
        self._cards[3].set_value("r0", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Parameter R1, only considered if R0 < 0:
        GT.0.0: quadratic logarithmic model
        LT.0.0: linear logarithmic model
        """ # nopep8
        return self._cards[3].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[3].set_value("r1", value)

    @property
    def edot_r(self) -> typing.Optional[float]:
        """Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode III (only considered if R0 < 0).
        """ # nopep8
        return self._cards[3].get_value("edot_r")

    @edot_r.setter
    def edot_r(self, value: float) -> None:
        """Set the edot_r property."""
        self._cards[3].set_value("edot_r", value)

    @property
    def fg3(self) -> typing.Optional[int]:
        """Get or set the a function.
        GT.0.0: FG3 is ratio of fracture energies, G_(III,P) / G_IIIC.
        LT.0.0: |FG3 | is ratio of displacements, (delta_s2 - delta_s1) / (delta_sf - delta_s1)
        """ # nopep8
        return self._cards[3].get_value("fg3")

    @fg3.setter
    def fg3(self, value: int) -> None:
        """Set the fg3 property."""
        self._cards[3].set_value("fg3", value)

    @property
    def lcg3c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines fracture energy GIIIC as a function of cohesive element thickness. G3C_0 and G3C_INF are ignored in that case.
        """ # nopep8
        return self._cards[3].get_value("lcg3c")

    @lcg3c.setter
    def lcg3c(self, value: int) -> None:
        """Set the lcg3c property."""
        self._cards[3].set_value("lcg3c", value)

    @property
    def gmod3(self) -> typing.Optional[int]:
        """Get or set the a function
        """ # nopep8
        return self._cards[4].get_value("gmod3")

    @gmod3.setter
    def gmod3(self, value: int) -> None:
        """Set the gmod3 property."""
        self._cards[4].set_value("gmod3", value)

    @property
    def rfiltf(self) -> typing.Optional[float]:
        """Get or set the Smoothing factor on the equivalent strain rate using an exponential moving average method:
        This option invokes a modified handling of strain rates, see Remarks.
        GT.0.0: RFILTF applied on the equivalent plastic strain rate
        LT.0.0: | RFILTF | applied on the equivalent total strain rate
        """ # nopep8
        return self._cards[5].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        """Set the rfiltf property."""
        self._cards[5].set_value("rfiltf", value)

    @property
    def compy(self) -> typing.Optional[float]:
        """Get or set the Yield under compression flag.
        EQ.0: Off (default)
        EQ.1: On.
        """ # nopep8
        return self._cards[5].get_value("compy")

    @compy.setter
    def compy(self, value: float) -> None:
        """Set the compy property."""
        self._cards[5].set_value("compy", value)

    @property
    def smolim(self) -> typing.Optional[float]:
        """Get or set the Smooth treatment of asymptotic limits (e.g., pure shear).
        EQ.0: Off (default)
        EQ.1: On.
        """ # nopep8
        return self._cards[5].get_value("smolim")

    @smolim.setter
    def smolim(self, value: float) -> None:
        """Set the smolim property."""
        self._cards[5].set_value("smolim", value)

    @property
    def xmu(self) -> float:
        """Get or set the Exponent of the mixed mode failure criterion. Default is 1.0.
        """ # nopep8
        return self._cards[5].get_value("xmu")

    @xmu.setter
    def xmu(self, value: float) -> None:
        """Set the xmu property."""
        self._cards[5].set_value("xmu", value)

    @property
    def usrfail(self) -> int:
        """Get or set the User defined failure flag:
        EQ.0 : No user subroutine is called (default).
        EQ.1 : User subroutine ucohfail in dyn21umatc.f is called.
        """ # nopep8
        return self._cards[5].get_value("usrfail")

    @usrfail.setter
    def usrfail(self, value: int) -> None:
        """Set the usrfail property."""
        if value not in [0, 1, None]:
            raise Exception("""usrfail must be `None` or one of {0,1}.""")
        self._cards[5].set_value("usrfail", value)

    @property
    def usrv1(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv1")

    @usrv1.setter
    def usrv1(self, value: float) -> None:
        """Set the usrv1 property."""
        self._cards[6].set_value("usrv1", value)

    @property
    def usrv2(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv2")

    @usrv2.setter
    def usrv2(self, value: float) -> None:
        """Set the usrv2 property."""
        self._cards[6].set_value("usrv2", value)

    @property
    def usrv3(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv3")

    @usrv3.setter
    def usrv3(self, value: float) -> None:
        """Set the usrv3 property."""
        self._cards[6].set_value("usrv3", value)

    @property
    def usrv4(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv4")

    @usrv4.setter
    def usrv4(self, value: float) -> None:
        """Set the usrv4 property."""
        self._cards[6].set_value("usrv4", value)

    @property
    def usrv5(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv5")

    @usrv5.setter
    def usrv5(self, value: float) -> None:
        """Set the usrv5 property."""
        self._cards[6].set_value("usrv5", value)

    @property
    def usrv6(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv6")

    @usrv6.setter
    def usrv6(self, value: float) -> None:
        """Set the usrv6 property."""
        self._cards[6].set_value("usrv6", value)

    @property
    def usrv7(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv7")

    @usrv7.setter
    def usrv7(self, value: float) -> None:
        """Set the usrv7 property."""
        self._cards[6].set_value("usrv7", value)

    @property
    def usrv8(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[6].get_value("usrv8")

    @usrv8.setter
    def usrv8(self, value: float) -> None:
        """Set the usrv8 property."""
        self._cards[6].set_value("usrv8", value)

    @property
    def usrv9(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv9")

    @usrv9.setter
    def usrv9(self, value: float) -> None:
        """Set the usrv9 property."""
        self._cards[7].set_value("usrv9", value)

    @property
    def usrv10(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv10")

    @usrv10.setter
    def usrv10(self, value: float) -> None:
        """Set the usrv10 property."""
        self._cards[7].set_value("usrv10", value)

    @property
    def usrv11(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv11")

    @usrv11.setter
    def usrv11(self, value: float) -> None:
        """Set the usrv11 property."""
        self._cards[7].set_value("usrv11", value)

    @property
    def usrv12(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv12")

    @usrv12.setter
    def usrv12(self, value: float) -> None:
        """Set the usrv12 property."""
        self._cards[7].set_value("usrv12", value)

    @property
    def usrv13(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv13")

    @usrv13.setter
    def usrv13(self, value: float) -> None:
        """Set the usrv13 property."""
        self._cards[7].set_value("usrv13", value)

    @property
    def usrv14(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv14")

    @usrv14.setter
    def usrv14(self, value: float) -> None:
        """Set the usrv14 property."""
        self._cards[7].set_value("usrv14", value)

    @property
    def usrv15(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv15")

    @usrv15.setter
    def usrv15(self, value: float) -> None:
        """Set the usrv15 property."""
        self._cards[7].set_value("usrv15", value)

    @property
    def usrv16(self) -> typing.Optional[float]:
        """Get or set the Failure constants for the user-defined failure subroutine
        """ # nopep8
        return self._cards[7].get_value("usrv16")

    @usrv16.setter
    def usrv16(self, value: float) -> None:
        """Set the usrv16 property."""
        self._cards[7].set_value("usrv16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcg1c_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcg1c."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcg1c:
                return kwd
        return None

    @lcg1c_link.setter
    def lcg1c_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcg1c."""
        self.lcg1c = value.lcid

    @property
    def lcg2c_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcg2c."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcg2c:
                return kwd
        return None

    @lcg2c_link.setter
    def lcg2c_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcg2c."""
        self.lcg2c = value.lcid

