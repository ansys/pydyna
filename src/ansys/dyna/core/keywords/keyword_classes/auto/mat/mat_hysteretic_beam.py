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

"""Module providing the MatHystereticBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATHYSTERETICBEAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("iax", int, 40, 10, 1),
    FieldSchema("isurf", int, 50, 10, 1),
    FieldSchema("ihard", int, 60, 10, 2),
    FieldSchema("ifema", int, 70, 10, 0),
)

_MATHYSTERETICBEAM_CARD1 = (
    FieldSchema("lcpms", int, 0, 10, None),
    FieldSchema("sfs", float, 10, 10, 1.0),
    FieldSchema("lcpmt", int, 20, 10, None),
    FieldSchema("sft", float, 30, 10, 1.0),
    FieldSchema("lcat", int, 40, 10, None),
    FieldSchema("sfat", float, 50, 10, 1.0),
    FieldSchema("lcac", int, 60, 10, None),
    FieldSchema("sfac", float, 70, 10, 1.0),
)

_MATHYSTERETICBEAM_CARD2 = (
    FieldSchema("alpha", float, 0, 10, 2.0),
    FieldSchema("beta", float, 10, 10, 2.0),
    FieldSchema("gamma", float, 20, 10, 2.0),
    FieldSchema("f0", float, 30, 10, None),
    FieldSchema("pinm", float, 40, 10, 1.0),
    FieldSchema("pins", float, 50, 10, 1.0),
    FieldSchema("hloc1", float, 60, 10, None),
    FieldSchema("hloc2", float, 70, 10, None),
)

_MATHYSTERETICBEAM_CARD3 = (
    FieldSchema("deltas", float, 0, 10, None),
    FieldSchema("kappas", float, 10, 10, None),
    FieldSchema("deltat", float, 20, 10, None),
    FieldSchema("kappat", float, 30, 10, None),
    FieldSchema("lcshs", int, 40, 10, None),
    FieldSchema("sfshs", float, 50, 10, 1.0),
    FieldSchema("lcsht", int, 60, 10, None),
    FieldSchema("sfsht", float, 70, 10, 1.0),
)

_MATHYSTERETICBEAM_CARD4 = (
    FieldSchema("hardms", float, 0, 10, None),
    FieldSchema("gamms", float, 10, 10, None),
    FieldSchema("hardmt", float, 20, 10, None),
    FieldSchema("gammt", float, 30, 10, None),
    FieldSchema("hardat", float, 40, 10, None),
    FieldSchema("gamat", float, 50, 10, None),
    FieldSchema("hardac", float, 60, 10, None),
    FieldSchema("gamac", float, 70, 10, None),
)

_MATHYSTERETICBEAM_CARD5 = (
    FieldSchema("omgms1", float, 0, 10, None),
    FieldSchema("omgms2", float, 10, 10, None),
    FieldSchema("omgmt1", float, 20, 10, 1.0),
    FieldSchema("omgmt2", float, 30, 10, 2.0),
    FieldSchema("omgat1", float, 40, 10, None),
    FieldSchema("omgat2", float, 50, 10, None),
    FieldSchema("omgac1", float, 60, 10, 1.0),
    FieldSchema("omgac2", float, 70, 10, 2.0),
)

_MATHYSTERETICBEAM_CARD6 = (
    FieldSchema("rums", float, 0, 10, 1e+20),
    FieldSchema("rumt", float, 10, 10, 1e+20),
    FieldSchema("duat", float, 20, 10, 1e+20),
    FieldSchema("duac", float, 30, 10, 1e+20),
    FieldSchema("lam1", float, 40, 10, None),
    FieldSchema("lam2", float, 50, 10, None),
    FieldSchema("soft1", float, 60, 10, 3.0),
    FieldSchema("soft2", float, 70, 10, 4.0),
)

_MATHYSTERETICBEAM_CARD7 = (
    FieldSchema("prs1", float, 0, 10, 1e+20),
    FieldSchema("prs2", float, 10, 10, 2e+20),
    FieldSchema("prs3", float, 20, 10, 3e+20),
    FieldSchema("prs4", float, 30, 10, 4e+20),
    FieldSchema("prt1", float, 40, 10, 1e+20),
    FieldSchema("prt2", float, 50, 10, 2e+20),
    FieldSchema("prt3", float, 60, 10, 3e+20),
    FieldSchema("prt4", float, 70, 10, 4e+20),
)

_MATHYSTERETICBEAM_CARD8 = (
    FieldSchema("ts1", float, 0, 10, 1e+20),
    FieldSchema("ts2", float, 10, 10, 2e+20),
    FieldSchema("ts3", float, 20, 10, 3e+20),
    FieldSchema("ts4", float, 30, 10, 4e+20),
    FieldSchema("cs1", float, 40, 10, 1e+20),
    FieldSchema("cs2", float, 50, 10, 2e+20),
    FieldSchema("cs3", float, 60, 10, 3e+20),
    FieldSchema("cs4", float, 70, 10, 4e+20),
)

_MATHYSTERETICBEAM_CARD9 = (
    FieldSchema("ss1", float, 0, 10, 1e+20),
    FieldSchema("ss2", float, 10, 10, 2e+20),
    FieldSchema("ss3", float, 20, 10, 3e+20),
    FieldSchema("ss4", float, 30, 10, 4e+20),
    FieldSchema("st1", float, 40, 10, 1e+20),
    FieldSchema("st2", float, 50, 10, 2e+20),
    FieldSchema("st3", float, 60, 10, 3e+20),
    FieldSchema("st4", float, 70, 10, 4e+20),
)

_MATHYSTERETICBEAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatHystereticBeam(KeywordBase):
    """DYNA MAT_HYSTERETIC_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "HYSTERETIC_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcpms": LinkType.DEFINE_CURVE,
        "lcpmt": LinkType.DEFINE_CURVE,
        "lcat": LinkType.DEFINE_CURVE,
        "lcac": LinkType.DEFINE_CURVE,
        "lcshs": LinkType.DEFINE_CURVE,
        "lcsht": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatHystereticBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICBEAM_CARD9,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatHystereticBeam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATHYSTERETICBEAM_OPTION0_CARD0,
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young’s modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson’s ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def iax(self) -> int:
        """Get or set the Abscissa definition for axial yield force as a function of inelastic deformation/strain curves (LCAT and LCAC on Card 2):
        EQ.1:	plastic deformation(change in length)
        EQ.2 : nominal plastic strain, that is,
        plastic deformation/ initial length.
        """ # nopep8
        return self._cards[0].get_value("iax")

    @iax.setter
    def iax(self, value: int) -> None:
        """Set the iax property."""
        if value not in [1, 2, None]:
            raise Exception("""iax must be `None` or one of {1,2}.""")
        self._cards[0].set_value("iax", value)

    @property
    def isurf(self) -> int:
        """Get or set the Yield surface type for interaction:
        EQ.1:	simple power law(default)
        EQ.2 : power law based on resultant moment
        EQ.3 : skewed yield surface version of ISURF = 2
        """ # nopep8
        return self._cards[0].get_value("isurf")

    @isurf.setter
    def isurf(self, value: int) -> None:
        """Set the isurf property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""isurf must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("isurf", value)

    @property
    def ihard(self) -> int:
        """Get or set the Hardening type during cyclic response
        EQ.1:	cumulative absolute deformation
        EQ.2:	peak deformation
        EQ.3 : peak deformation, yield - oriented
        EQ.4 : peak deformation, peak - oriented
        """ # nopep8
        return self._cards[0].get_value("ihard")

    @ihard.setter
    def ihard(self, value: int) -> None:
        """Set the ihard property."""
        if value not in [2, 1, 3, 4, None]:
            raise Exception("""ihard must be `None` or one of {2,1,3,4}.""")
        self._cards[0].set_value("ihard", value)

    @property
    def ifema(self) -> int:
        """Get or set the Flag for input of FEMA thresholds
        EQ.0:	no input
        EQ.1:	input of rotation thresholds only
        EQ.2 : input of rotation and axial strain thresholds
        EQ.3 : input of rotation, axial strainand shear strain thresholds
        """ # nopep8
        return self._cards[0].get_value("ifema")

    @ifema.setter
    def ifema(self, value: int) -> None:
        """Set the ifema property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ifema must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("ifema", value)

    @property
    def lcpms(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (See *DEFINE_CURVE) giving normalized yield moment as a function of plastic rotation at hinges about the local s-axis. All values are positive
        """ # nopep8
        return self._cards[1].get_value("lcpms")

    @lcpms.setter
    def lcpms(self, value: int) -> None:
        """Set the lcpms property."""
        self._cards[1].set_value("lcpms", value)

    @property
    def sfs(self) -> float:
        """Get or set the Representative yield moment for plastic hinges about local the s-axis (scales the normalized moment from LCPMS) .
        """ # nopep8
        return self._cards[1].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        """Set the sfs property."""
        self._cards[1].set_value("sfs", value)

    @property
    def lcpmt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (See *DEFINE_CURVE) giving normalized yield moment as a function of plastic rotation at hinges about the local t-axis. All values are positive.
        """ # nopep8
        return self._cards[1].get_value("lcpmt")

    @lcpmt.setter
    def lcpmt(self, value: int) -> None:
        """Set the lcpmt property."""
        self._cards[1].set_value("lcpmt", value)

    @property
    def sft(self) -> float:
        """Get or set the Representative yield moment for plastic hinges about local the t-axis (scales the normalized moment from LCPMT)
        """ # nopep8
        return self._cards[1].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[1].set_value("sft", value)

    @property
    def lcat(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (See *DEFINE_‌CURVE) giving normalized axial tensile yield force as a function of inelastic deformation/strain.  See IAX above for definition of deformation/strain.  All values are positive. See *DEFINE_‌CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcat")

    @lcat.setter
    def lcat(self, value: int) -> None:
        """Set the lcat property."""
        self._cards[1].set_value("lcat", value)

    @property
    def sfat(self) -> float:
        """Get or set the Representative tensile strength (scales the normalized force from LCAT
        """ # nopep8
        return self._cards[1].get_value("sfat")

    @sfat.setter
    def sfat(self, value: float) -> None:
        """Set the sfat property."""
        self._cards[1].set_value("sfat", value)

    @property
    def lcac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (See *DEFINE_‌CURVE) giving normalized axial compressive yield force as a function of inelastic deformation/strain.  See IAX above for definition of deformation/strain.  All values are positive. See *DEFINE_‌CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcac")

    @lcac.setter
    def lcac(self, value: int) -> None:
        """Set the lcac property."""
        self._cards[1].set_value("lcac", value)

    @property
    def sfac(self) -> float:
        """Get or set the Representative compressive strength (scales the normalized force from LCAC)
        """ # nopep8
        return self._cards[1].get_value("sfac")

    @sfac.setter
    def sfac(self, value: float) -> None:
        """Set the sfac property."""
        self._cards[1].set_value("sfac", value)

    @property
    def alpha(self) -> float:
        """Get or set the Parameter to define moment-axial yield surface:
        GT.0.0:	yield surface parameter ALPHA(must not be < 1.1); see Remark 2.
        LT.0.0:	user - defined yield surface for the local s - axis. | ALPHA | is the load curve ID giving the yield locus.The abscissa is the moment about the local s - axis; the ordinate is the axial force(tensile positive).
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[2].set_value("alpha", value)

    @property
    def beta(self) -> float:
        """Get or set the Parameter to define moment-axial yield surface:
        GT.0.0:	yield surface parameter BETA(must not be < 1.1); see Remark 2.
        LT.0.0:	user - defined yield surface for the local t - axis. | BETA | is the load curve ID giving the yield locus.Abscissa is moment about the local t - axis; the ordinate is the axial force(tensile positive)..
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

    @property
    def gamma(self) -> float:
        """Get or set the Parameter to define yield surface which must not be < 1.1 .
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[2].set_value("gamma", value)

    @property
    def f0(self) -> typing.Optional[float]:
        """Get or set the Force at which maximum yield moment is achieved (tensile positive; for reinforced concrete, a negative (compressive) value would be entered)
        """ # nopep8
        return self._cards[2].get_value("f0")

    @f0.setter
    def f0(self, value: float) -> None:
        """Set the f0 property."""
        self._cards[2].set_value("f0", value)

    @property
    def pinm(self) -> float:
        """Get or set the Pinching factor for flexural hysteresis (for IHARD = 3 or 4 only).  .
        """ # nopep8
        return self._cards[2].get_value("pinm")

    @pinm.setter
    def pinm(self, value: float) -> None:
        """Set the pinm property."""
        self._cards[2].set_value("pinm", value)

    @property
    def pins(self) -> float:
        """Get or set the Pinching factor for shear hysteresis (for IHARD = 3 or 4 only).
        """ # nopep8
        return self._cards[2].get_value("pins")

    @pins.setter
    def pins(self, value: float) -> None:
        """Set the pins property."""
        self._cards[2].set_value("pins", value)

    @property
    def hloc1(self) -> typing.Optional[float]:
        """Get or set the Location of plastic Hinge 1 from Node 1 (see Remark 1):
        GE.0.0:	HLOC1 is the distance of Hinge 1 to Node 1 divided by element length.
        LT.0.0.AND.GT. - 1.0 : -HLOC1 is the distance of Hinge 1 to Node 1 divided by element length; deactivate shear yielding.
        EQ. - 1.0:	deactivate Hinge 1.
        EQ. - 10.0 : deactivate shear yielding; Hinge 1 is located at Node 1.
        EQ. - 11.0:	deactivate Hinge 1 and shear yielding.
        """ # nopep8
        return self._cards[2].get_value("hloc1")

    @hloc1.setter
    def hloc1(self, value: float) -> None:
        """Set the hloc1 property."""
        self._cards[2].set_value("hloc1", value)

    @property
    def hloc2(self) -> typing.Optional[float]:
        """Get or set the Location of plastic Hinge 2 from Node 2 (see Remark 1):
        GE.0.0:	HLOC2 is the distance of Hinge 2 to Node 2 divided by element length.
        LT.0.0.AND.GT. - 1.0 : HLOC2 is the distance of Hinge 2 to Node 2 divided by element length; deactivate shear yielding.
        EQ. - 1.0:	deactivate Hinge 2.
        EQ. - 10.0 : deactivate shear yielding; Hinge 2 is located at Node 2.
        EQ. - 11.0:	deactivate Hinge 2 and shear yielding.
        """ # nopep8
        return self._cards[2].get_value("hloc2")

    @hloc2.setter
    def hloc2(self, value: float) -> None:
        """Set the hloc2 property."""
        self._cards[2].set_value("hloc2", value)

    @property
    def deltas(self) -> typing.Optional[float]:
        """Get or set the Parameter to define the skew for yield surface (ISURF = 3);
        """ # nopep8
        return self._cards[3].get_value("deltas")

    @deltas.setter
    def deltas(self, value: float) -> None:
        """Set the deltas property."""
        self._cards[3].set_value("deltas", value)

    @property
    def kappas(self) -> typing.Optional[float]:
        """Get or set the Parameter to define the skew for yield surface (ISURF = 3); .
        """ # nopep8
        return self._cards[3].get_value("kappas")

    @kappas.setter
    def kappas(self, value: float) -> None:
        """Set the kappas property."""
        self._cards[3].set_value("kappas", value)

    @property
    def deltat(self) -> typing.Optional[float]:
        """Get or set the Parameter to define the skew for yield surface (ISURF = 3); .
        """ # nopep8
        return self._cards[3].get_value("deltat")

    @deltat.setter
    def deltat(self, value: float) -> None:
        """Set the deltat property."""
        self._cards[3].set_value("deltat", value)

    @property
    def kappat(self) -> typing.Optional[float]:
        """Get or set the Parameter to define the skew for yield surface (ISURF = 3);
        """ # nopep8
        return self._cards[3].get_value("kappat")

    @kappat.setter
    def kappat(self, value: float) -> None:
        """Set the kappat property."""
        self._cards[3].set_value("kappat", value)

    @property
    def lcshs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) giving yield shear force as a function of inelastic shear strain (shear angle) in the local s-direction .
        """ # nopep8
        return self._cards[3].get_value("lcshs")

    @lcshs.setter
    def lcshs(self, value: int) -> None:
        """Set the lcshs property."""
        self._cards[3].set_value("lcshs", value)

    @property
    def sfshs(self) -> float:
        """Get or set the Scale factor on yield shear force in the local s-direction (scales the force from LCSHS):
        GT.0.0:	constant scale factor
        LT.0.0 : user - defined interaction with axial force. | SFSHS | is the load curve ID giving scale factor as a function of normalized axial force(tensile is positive).The normalization uses SFAT for tensile forceand SFAC for compressive force.For example, point(-1.0,0.5) on the curve defines a scale factor of 0.5 for compressive force of - SFAC
        """ # nopep8
        return self._cards[3].get_value("sfshs")

    @sfshs.setter
    def sfshs(self, value: float) -> None:
        """Set the sfshs property."""
        self._cards[3].set_value("sfshs", value)

    @property
    def lcsht(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) giving yield shear force as a function of inelastic shear strain (shear angle) in the local t-direction .
        """ # nopep8
        return self._cards[3].get_value("lcsht")

    @lcsht.setter
    def lcsht(self, value: int) -> None:
        """Set the lcsht property."""
        self._cards[3].set_value("lcsht", value)

    @property
    def sfsht(self) -> float:
        """Get or set the Scale factor on yield shear force in the local t-direction (scales the force from LCSHS).
        GT.0.0:	constant scale factor
        LT.0.0 : user - defined interaction with axial force. | SFSHT|  is the load curve ID giving scale factor as a function of normalized axial force(tensile is positive).The normalization uses SFAT for tensile forceand SFAC for compressive force.For example, point(-1.0,0.5) on the curve defines a scale factor of 0.5 for compressive force of - SFAC
        """ # nopep8
        return self._cards[3].get_value("sfsht")

    @sfsht.setter
    def sfsht(self, value: float) -> None:
        """Set the sfsht property."""
        self._cards[3].set_value("sfsht", value)

    @property
    def hardms(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus for moment about the local s-axis
        """ # nopep8
        return self._cards[4].get_value("hardms")

    @hardms.setter
    def hardms(self, value: float) -> None:
        """Set the hardms property."""
        self._cards[4].set_value("hardms", value)

    @property
    def gamms(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening limit for moment about the local s-axis .
        """ # nopep8
        return self._cards[4].get_value("gamms")

    @gamms.setter
    def gamms(self, value: float) -> None:
        """Set the gamms property."""
        self._cards[4].set_value("gamms", value)

    @property
    def hardmt(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus for moment about the local t-axis.
        """ # nopep8
        return self._cards[4].get_value("hardmt")

    @hardmt.setter
    def hardmt(self, value: float) -> None:
        """Set the hardmt property."""
        self._cards[4].set_value("hardmt", value)

    @property
    def gammt(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening limit for moment about the local t-axis
        """ # nopep8
        return self._cards[4].get_value("gammt")

    @gammt.setter
    def gammt(self, value: float) -> None:
        """Set the gammt property."""
        self._cards[4].set_value("gammt", value)

    @property
    def hardat(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus for tensile axial force.
        """ # nopep8
        return self._cards[4].get_value("hardat")

    @hardat.setter
    def hardat(self, value: float) -> None:
        """Set the hardat property."""
        self._cards[4].set_value("hardat", value)

    @property
    def gamat(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening limit for tensile axial force
        """ # nopep8
        return self._cards[4].get_value("gamat")

    @gamat.setter
    def gamat(self, value: float) -> None:
        """Set the gamat property."""
        self._cards[4].set_value("gamat", value)

    @property
    def hardac(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus for compressive axial force.
        """ # nopep8
        return self._cards[4].get_value("hardac")

    @hardac.setter
    def hardac(self, value: float) -> None:
        """Set the hardac property."""
        self._cards[4].set_value("hardac", value)

    @property
    def gamac(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening limit for compressive axial force
        """ # nopep8
        return self._cards[4].get_value("gamac")

    @gamac.setter
    def gamac(self, value: float) -> None:
        """Set the gamac property."""
        self._cards[4].set_value("gamac", value)

    @property
    def omgms1(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter ω_s1 for moment about the local s-axis
        """ # nopep8
        return self._cards[5].get_value("omgms1")

    @omgms1.setter
    def omgms1(self, value: float) -> None:
        """Set the omgms1 property."""
        self._cards[5].set_value("omgms1", value)

    @property
    def omgms2(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter ω_s2 for moment about the local s-axis.
        """ # nopep8
        return self._cards[5].get_value("omgms2")

    @omgms2.setter
    def omgms2(self, value: float) -> None:
        """Set the omgms2 property."""
        self._cards[5].set_value("omgms2", value)

    @property
    def omgmt1(self) -> float:
        """Get or set the Damage evolution parameter ω_t1 for moment about the local t-axis.
        """ # nopep8
        return self._cards[5].get_value("omgmt1")

    @omgmt1.setter
    def omgmt1(self, value: float) -> None:
        """Set the omgmt1 property."""
        self._cards[5].set_value("omgmt1", value)

    @property
    def omgmt2(self) -> float:
        """Get or set the Damage evolution parameter ω_t2 for moment about the local t-axis
        """ # nopep8
        return self._cards[5].get_value("omgmt2")

    @omgmt2.setter
    def omgmt2(self, value: float) -> None:
        """Set the omgmt2 property."""
        self._cards[5].set_value("omgmt2", value)

    @property
    def omgat1(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter ω_at1 for tensile force.
        """ # nopep8
        return self._cards[5].get_value("omgat1")

    @omgat1.setter
    def omgat1(self, value: float) -> None:
        """Set the omgat1 property."""
        self._cards[5].set_value("omgat1", value)

    @property
    def omgat2(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter ω_at2 for tensile force
        """ # nopep8
        return self._cards[5].get_value("omgat2")

    @omgat2.setter
    def omgat2(self, value: float) -> None:
        """Set the omgat2 property."""
        self._cards[5].set_value("omgat2", value)

    @property
    def omgac1(self) -> float:
        """Get or set the Damage evolution parameter ω_ac1 for compressive force.
        """ # nopep8
        return self._cards[5].get_value("omgac1")

    @omgac1.setter
    def omgac1(self, value: float) -> None:
        """Set the omgac1 property."""
        self._cards[5].set_value("omgac1", value)

    @property
    def omgac2(self) -> float:
        """Get or set the Damage evolution parameter ω_ac2 for compressive force
        """ # nopep8
        return self._cards[5].get_value("omgac2")

    @omgac2.setter
    def omgac2(self, value: float) -> None:
        """Set the omgac2 property."""
        self._cards[5].set_value("omgac2", value)

    @property
    def rums(self) -> float:
        """Get or set the Ultimate plastic rotation about s-axis for damage calculation
        """ # nopep8
        return self._cards[6].get_value("rums")

    @rums.setter
    def rums(self, value: float) -> None:
        """Set the rums property."""
        self._cards[6].set_value("rums", value)

    @property
    def rumt(self) -> float:
        """Get or set the Ultimate plastic rotation about t-axis for damage calculation.
        """ # nopep8
        return self._cards[6].get_value("rumt")

    @rumt.setter
    def rumt(self, value: float) -> None:
        """Set the rumt property."""
        self._cards[6].set_value("rumt", value)

    @property
    def duat(self) -> float:
        """Get or set the Ultimate tensile plastic deformation/strain for damage calculation.
        """ # nopep8
        return self._cards[6].get_value("duat")

    @duat.setter
    def duat(self, value: float) -> None:
        """Set the duat property."""
        self._cards[6].set_value("duat", value)

    @property
    def duac(self) -> float:
        """Get or set the Ultimate compressive plastic deformation/strain for damage calculation
        """ # nopep8
        return self._cards[6].get_value("duac")

    @duac.setter
    def duac(self, value: float) -> None:
        """Set the duac property."""
        self._cards[6].set_value("duac", value)

    @property
    def lam1(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter.
        """ # nopep8
        return self._cards[6].get_value("lam1")

    @lam1.setter
    def lam1(self, value: float) -> None:
        """Set the lam1 property."""
        self._cards[6].set_value("lam1", value)

    @property
    def lam2(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter
        """ # nopep8
        return self._cards[6].get_value("lam2")

    @lam2.setter
    def lam2(self, value: float) -> None:
        """Set the lam2 property."""
        self._cards[6].set_value("lam2", value)

    @property
    def soft1(self) -> float:
        """Get or set the Threshold index at which softening starts .
        """ # nopep8
        return self._cards[6].get_value("soft1")

    @soft1.setter
    def soft1(self, value: float) -> None:
        """Set the soft1 property."""
        self._cards[6].set_value("soft1", value)

    @property
    def soft2(self) -> float:
        """Get or set the Threshold index at which the element is fully softened and to be removed
        """ # nopep8
        return self._cards[6].get_value("soft2")

    @soft2.setter
    def soft2(self, value: float) -> None:
        """Set the soft2 property."""
        self._cards[6].set_value("soft2", value)

    @property
    def prs1(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about s-axis
        """ # nopep8
        return self._cards[7].get_value("prs1")

    @prs1.setter
    def prs1(self, value: float) -> None:
        """Set the prs1 property."""
        self._cards[7].set_value("prs1", value)

    @property
    def prs2(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about s-axis.
        """ # nopep8
        return self._cards[7].get_value("prs2")

    @prs2.setter
    def prs2(self, value: float) -> None:
        """Set the prs2 property."""
        self._cards[7].set_value("prs2", value)

    @property
    def prs3(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about s-axis.
        """ # nopep8
        return self._cards[7].get_value("prs3")

    @prs3.setter
    def prs3(self, value: float) -> None:
        """Set the prs3 property."""
        self._cards[7].set_value("prs3", value)

    @property
    def prs4(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about s-axis
        """ # nopep8
        return self._cards[7].get_value("prs4")

    @prs4.setter
    def prs4(self, value: float) -> None:
        """Set the prs4 property."""
        self._cards[7].set_value("prs4", value)

    @property
    def prt1(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about t-axis.
        """ # nopep8
        return self._cards[7].get_value("prt1")

    @prt1.setter
    def prt1(self, value: float) -> None:
        """Set the prt1 property."""
        self._cards[7].set_value("prt1", value)

    @property
    def prt2(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about t-axis
        """ # nopep8
        return self._cards[7].get_value("prt2")

    @prt2.setter
    def prt2(self, value: float) -> None:
        """Set the prt2 property."""
        self._cards[7].set_value("prt2", value)

    @property
    def prt3(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about t-axis.
        """ # nopep8
        return self._cards[7].get_value("prt3")

    @prt3.setter
    def prt3(self, value: float) -> None:
        """Set the prt3 property."""
        self._cards[7].set_value("prt3", value)

    @property
    def prt4(self) -> float:
        """Get or set the Plastic rotation thresholds 1 to 4 about t-axis
        """ # nopep8
        return self._cards[7].get_value("prt4")

    @prt4.setter
    def prt4(self, value: float) -> None:
        """Set the prt4 property."""
        self._cards[7].set_value("prt4", value)

    @property
    def ts1(self) -> float:
        """Get or set the Tensile plastic axial deformation/strain thresholds 1 to 4
        """ # nopep8
        return self._cards[8].get_value("ts1")

    @ts1.setter
    def ts1(self, value: float) -> None:
        """Set the ts1 property."""
        self._cards[8].set_value("ts1", value)

    @property
    def ts2(self) -> float:
        """Get or set the Tensile plastic axial deformation/strain thresholds 1 to 4.
        """ # nopep8
        return self._cards[8].get_value("ts2")

    @ts2.setter
    def ts2(self, value: float) -> None:
        """Set the ts2 property."""
        self._cards[8].set_value("ts2", value)

    @property
    def ts3(self) -> float:
        """Get or set the Tensile plastic axial deformation/strain thresholds 1 to 4.
        """ # nopep8
        return self._cards[8].get_value("ts3")

    @ts3.setter
    def ts3(self, value: float) -> None:
        """Set the ts3 property."""
        self._cards[8].set_value("ts3", value)

    @property
    def ts4(self) -> float:
        """Get or set the Tensile plastic axial deformation/strain thresholds 1 to 4
        """ # nopep8
        return self._cards[8].get_value("ts4")

    @ts4.setter
    def ts4(self, value: float) -> None:
        """Set the ts4 property."""
        self._cards[8].set_value("ts4", value)

    @property
    def cs1(self) -> float:
        """Get or set the Compressive plastic axial deformation/strain thresholds 1 to 4.
        """ # nopep8
        return self._cards[8].get_value("cs1")

    @cs1.setter
    def cs1(self, value: float) -> None:
        """Set the cs1 property."""
        self._cards[8].set_value("cs1", value)

    @property
    def cs2(self) -> float:
        """Get or set the Compressive plastic axial deformation/strain thresholds 1 to 4
        """ # nopep8
        return self._cards[8].get_value("cs2")

    @cs2.setter
    def cs2(self, value: float) -> None:
        """Set the cs2 property."""
        self._cards[8].set_value("cs2", value)

    @property
    def cs3(self) -> float:
        """Get or set the Compressive plastic axial deformation/strain thresholds 1 to 4.
        """ # nopep8
        return self._cards[8].get_value("cs3")

    @cs3.setter
    def cs3(self, value: float) -> None:
        """Set the cs3 property."""
        self._cards[8].set_value("cs3", value)

    @property
    def cs4(self) -> float:
        """Get or set the Compressive plastic axial deformation/strain thresholds 1 to 4
        """ # nopep8
        return self._cards[8].get_value("cs4")

    @cs4.setter
    def cs4(self, value: float) -> None:
        """Set the cs4 property."""
        self._cards[8].set_value("cs4", value)

    @property
    def ss1(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the s-direction
        """ # nopep8
        return self._cards[9].get_value("ss1")

    @ss1.setter
    def ss1(self, value: float) -> None:
        """Set the ss1 property."""
        self._cards[9].set_value("ss1", value)

    @property
    def ss2(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the s-direction.
        """ # nopep8
        return self._cards[9].get_value("ss2")

    @ss2.setter
    def ss2(self, value: float) -> None:
        """Set the ss2 property."""
        self._cards[9].set_value("ss2", value)

    @property
    def ss3(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the s-direction.
        """ # nopep8
        return self._cards[9].get_value("ss3")

    @ss3.setter
    def ss3(self, value: float) -> None:
        """Set the ss3 property."""
        self._cards[9].set_value("ss3", value)

    @property
    def ss4(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the s-direction
        """ # nopep8
        return self._cards[9].get_value("ss4")

    @ss4.setter
    def ss4(self, value: float) -> None:
        """Set the ss4 property."""
        self._cards[9].set_value("ss4", value)

    @property
    def st1(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the t-direction.
        """ # nopep8
        return self._cards[9].get_value("st1")

    @st1.setter
    def st1(self, value: float) -> None:
        """Set the st1 property."""
        self._cards[9].set_value("st1", value)

    @property
    def st2(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the t-direction
        """ # nopep8
        return self._cards[9].get_value("st2")

    @st2.setter
    def st2(self, value: float) -> None:
        """Set the st2 property."""
        self._cards[9].set_value("st2", value)

    @property
    def st3(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the t-direction.
        """ # nopep8
        return self._cards[9].get_value("st3")

    @st3.setter
    def st3(self, value: float) -> None:
        """Set the st3 property."""
        self._cards[9].set_value("st3", value)

    @property
    def st4(self) -> float:
        """Get or set the Plastic shear strain thresholds 1 to 4 in the t-direction
        """ # nopep8
        return self._cards[9].get_value("st4")

    @st4.setter
    def st4(self, value: float) -> None:
        """Set the st4 property."""
        self._cards[9].set_value("st4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[10].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcpms_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcpms."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpms:
                return kwd
        return None

    @lcpms_link.setter
    def lcpms_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpms."""
        self.lcpms = value.lcid

    @property
    def lcpmt_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcpmt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpmt:
                return kwd
        return None

    @lcpmt_link.setter
    def lcpmt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpmt."""
        self.lcpmt = value.lcid

    @property
    def lcat_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcat."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcat:
                return kwd
        return None

    @lcat_link.setter
    def lcat_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcat."""
        self.lcat = value.lcid

    @property
    def lcac_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcac."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcac:
                return kwd
        return None

    @lcac_link.setter
    def lcac_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcac."""
        self.lcac = value.lcid

    @property
    def lcshs_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcshs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcshs:
                return kwd
        return None

    @lcshs_link.setter
    def lcshs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcshs."""
        self.lcshs = value.lcid

    @property
    def lcsht_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcsht."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsht:
                return kwd
        return None

    @lcsht_link.setter
    def lcsht_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsht."""
        self.lcsht = value.lcid

