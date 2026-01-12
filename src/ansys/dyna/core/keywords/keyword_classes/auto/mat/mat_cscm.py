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

"""Module providing the MatCscm class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATCSCM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("nplot", int, 20, 10, 1),
    FieldSchema("incre", float, 30, 10, None),
    FieldSchema("irate", int, 40, 10, 0),
    FieldSchema("erode", float, 50, 10, None),
    FieldSchema("recov", float, 60, 10, 0.0),
    FieldSchema("itretrc", int, 70, 10, 0),
)

_MATCSCM_CARD1 = (
    FieldSchema("pred", float, 0, 10, None),
)

_MATCSCM_CARD2 = (
    FieldSchema("g", float, 0, 10, None),
    FieldSchema("k", float, 10, 10, None),
    FieldSchema("alpha", float, 20, 10, None),
    FieldSchema("theta", float, 30, 10, None),
    FieldSchema("lamda", float, 40, 10, None),
    FieldSchema("beta", float, 50, 10, None),
    FieldSchema("nh", float, 60, 10, None),
    FieldSchema("ch", float, 70, 10, None),
)

_MATCSCM_CARD3 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("theta1", float, 10, 10, None),
    FieldSchema("lamda1", float, 20, 10, None),
    FieldSchema("beta1", float, 30, 10, None),
    FieldSchema("alpha2", float, 40, 10, None),
    FieldSchema("theta2", float, 50, 10, None),
    FieldSchema("lamda2", float, 60, 10, None),
    FieldSchema("beta2", float, 70, 10, None),
)

_MATCSCM_CARD4 = (
    FieldSchema("r", float, 0, 10, None),
    FieldSchema("xd", float, 10, 10, None),
    FieldSchema("w", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
)

_MATCSCM_CARD5 = (
    FieldSchema("b", float, 0, 10, None),
    FieldSchema("gfc", float, 10, 10, None),
    FieldSchema("d", float, 20, 10, None),
    FieldSchema("gft", float, 30, 10, None),
    FieldSchema("gfs", float, 40, 10, None),
    FieldSchema("pwrc", float, 50, 10, None),
    FieldSchema("pwrt", float, 60, 10, None),
    FieldSchema("pmod", float, 70, 10, None),
)

_MATCSCM_CARD6 = (
    FieldSchema("eta0c", float, 0, 10, None),
    FieldSchema("nc", float, 10, 10, None),
    FieldSchema("etaot", float, 20, 10, None),
    FieldSchema("nt", float, 30, 10, None),
    FieldSchema("overc", float, 40, 10, None),
    FieldSchema("overt", float, 50, 10, None),
    FieldSchema("srate", float, 60, 10, None),
    FieldSchema("rep0w", float, 70, 10, None),
)

_MATCSCM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCscm(KeywordBase):
    """DYNA MAT_CSCM keyword"""

    keyword = "MAT"
    subkeyword = "CSCM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatCscm class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCSCM_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCscm.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCSCM_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification, a unique number has to be chosen.
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
    def nplot(self) -> int:
        """Get or set the Plotting options:
        EQ. 1: Maximum of brittle and ductile damage (default).
        EQ. 2: Maximum of brittle and ductile damage, with recovery of  brittle damage.
        EQ. 3:  Brittle damage.
        EQ. 4:  Ductile damage.
        EQ. 5:    (intersection of cap with shear surface).
        EQ. 6: X0 (intersection of cap with pressure axis).
        EQ. 7:   (plastic volume strain).
        """ # nopep8
        return self._cards[0].get_value("nplot")

    @nplot.setter
    def nplot(self, value: int) -> None:
        """Set the nplot property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""nplot must be `None` or one of {1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("nplot", value)

    @property
    def incre(self) -> typing.Optional[float]:
        """Get or set the Maximum strain increment for subincrementation.  If left blank, a default value is set during initialization based upon the shear strength and stiffness
        """ # nopep8
        return self._cards[0].get_value("incre")

    @incre.setter
    def incre(self, value: float) -> None:
        """Set the incre property."""
        self._cards[0].set_value("incre", value)

    @property
    def irate(self) -> int:
        """Get or set the Rate effects options:
        EQ.   0: Rate effects model turned off (default).
        EQ.   1: Rate effects model turned on.
        """ # nopep8
        return self._cards[0].get_value("irate")

    @irate.setter
    def irate(self, value: int) -> None:
        """Set the irate property."""
        if value not in [0, 1, None]:
            raise Exception("""irate must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irate", value)

    @property
    def erode(self) -> typing.Optional[float]:
        """Get or set the Elements erode when damage exceeds 0.99 and the maximum principal strain exceeds 1.-ERODE.   For erosion that is independent of strain, set ERODE equal to 1.0.   Erosion does not occur if ERODE is less than 1.0.
        """ # nopep8
        return self._cards[0].get_value("erode")

    @erode.setter
    def erode(self, value: float) -> None:
        """Set the erode property."""
        self._cards[0].set_value("erode", value)

    @property
    def recov(self) -> float:
        """Get or set the The modulus is recovered in compression when RECOV is equal to 0 (default).  The modulus remains at the brittle damage level when RECOV is equal to 1.  Partial recovery is modeled for values of RECOV between 0 and 1.  Two options are available:
        Option 1:  Input a value between 0 and 1.  Recovery is based upon the sign of the pressure invariant only.
        Option 2:  Input a value between 10 and 11.  Recovery is based upon the sign of both the pressure and volumetric strain.    In this case, RECOV=RECOV-10, and a flag is set to request the volumetric strain check.
        """ # nopep8
        return self._cards[0].get_value("recov")

    @recov.setter
    def recov(self, value: float) -> None:
        """Set the recov property."""
        self._cards[0].set_value("recov", value)

    @property
    def itretrc(self) -> int:
        """Get or set the Cap retraction option:
        EQ.0: Cap does not retract (default).
        EQ.1: Cap retracts.
        """ # nopep8
        return self._cards[0].get_value("itretrc")

    @itretrc.setter
    def itretrc(self, value: int) -> None:
        """Set the itretrc property."""
        if value not in [0, 1, None]:
            raise Exception("""itretrc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("itretrc", value)

    @property
    def pred(self) -> typing.Optional[float]:
        """Get or set the Pre-existing damage (0   PreD < 1).  If left blank, the default is zero (no pre-existing damage).
        """ # nopep8
        return self._cards[1].get_value("pred")

    @pred.setter
    def pred(self, value: float) -> None:
        """Set the pred property."""
        self._cards[1].set_value("pred", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus. .
        """ # nopep8
        return self._cards[2].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[2].set_value("g", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[2].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[2].set_value("k", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Tri-axial compression surface constant term,  .
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[2].set_value("alpha", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Tri-axial compression surface linear term,  .
        """ # nopep8
        return self._cards[2].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[2].set_value("theta", value)

    @property
    def lamda(self) -> typing.Optional[float]:
        """Get or set the Tri-axial compression surface nonlinear term,  .
        """ # nopep8
        return self._cards[2].get_value("lamda")

    @lamda.setter
    def lamda(self, value: float) -> None:
        """Set the lamda property."""
        self._cards[2].set_value("lamda", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Tri-axial compression surface exponent.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

    @property
    def nh(self) -> typing.Optional[float]:
        """Get or set the Hardening initiation
        """ # nopep8
        return self._cards[2].get_value("nh")

    @nh.setter
    def nh(self, value: float) -> None:
        """Set the nh property."""
        self._cards[2].set_value("nh", value)

    @property
    def ch(self) -> typing.Optional[float]:
        """Get or set the Hardening rate
        """ # nopep8
        return self._cards[2].get_value("ch")

    @ch.setter
    def ch(self, value: float) -> None:
        """Set the ch property."""
        self._cards[2].set_value("ch", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Torsion surface constant term.
        """ # nopep8
        return self._cards[3].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[3].set_value("alpha1", value)

    @property
    def theta1(self) -> typing.Optional[float]:
        """Get or set the Torsion surface linear term.
        """ # nopep8
        return self._cards[3].get_value("theta1")

    @theta1.setter
    def theta1(self, value: float) -> None:
        """Set the theta1 property."""
        self._cards[3].set_value("theta1", value)

    @property
    def lamda1(self) -> typing.Optional[float]:
        """Get or set the Torsion surface nonlinear term.
        """ # nopep8
        return self._cards[3].get_value("lamda1")

    @lamda1.setter
    def lamda1(self, value: float) -> None:
        """Set the lamda1 property."""
        self._cards[3].set_value("lamda1", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Torsion surface exponent
        """ # nopep8
        return self._cards[3].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[3].set_value("beta1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension surface constant term
        """ # nopep8
        return self._cards[3].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[3].set_value("alpha2", value)

    @property
    def theta2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension surface linear term.
        """ # nopep8
        return self._cards[3].get_value("theta2")

    @theta2.setter
    def theta2(self, value: float) -> None:
        """Set the theta2 property."""
        self._cards[3].set_value("theta2", value)

    @property
    def lamda2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension surface nonlinear term
        """ # nopep8
        return self._cards[3].get_value("lamda2")

    @lamda2.setter
    def lamda2(self, value: float) -> None:
        """Set the lamda2 property."""
        self._cards[3].set_value("lamda2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension surface exponent
        """ # nopep8
        return self._cards[3].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[3].set_value("beta2", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Cap aspect ratio.
        """ # nopep8
        return self._cards[4].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[4].set_value("r", value)

    @property
    def xd(self) -> typing.Optional[float]:
        """Get or set the Cap initial location.
        """ # nopep8
        return self._cards[4].get_value("xd")

    @xd.setter
    def xd(self, value: float) -> None:
        """Set the xd property."""
        self._cards[4].set_value("xd", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Maximum plastic volume compaction, W.
        """ # nopep8
        return self._cards[4].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[4].set_value("w", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Linear shape parameter, D1
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Quadratic shape parameter, D2
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Ductile shape softening parameter, B.
        """ # nopep8
        return self._cards[5].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[5].set_value("b", value)

    @property
    def gfc(self) -> typing.Optional[float]:
        """Get or set the Fracture energy in uniaxial stress Gfc.
        """ # nopep8
        return self._cards[5].get_value("gfc")

    @gfc.setter
    def gfc(self, value: float) -> None:
        """Set the gfc property."""
        self._cards[5].set_value("gfc", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Brittle shape softening parameter, D.
        """ # nopep8
        return self._cards[5].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[5].set_value("d", value)

    @property
    def gft(self) -> typing.Optional[float]:
        """Get or set the Fracture energy in uniaxial tension, Gft
        """ # nopep8
        return self._cards[5].get_value("gft")

    @gft.setter
    def gft(self, value: float) -> None:
        """Set the gft property."""
        self._cards[5].set_value("gft", value)

    @property
    def gfs(self) -> typing.Optional[float]:
        """Get or set the Fracture energy in pure shear stress, Gfs
        """ # nopep8
        return self._cards[5].get_value("gfs")

    @gfs.setter
    def gfs(self, value: float) -> None:
        """Set the gfs property."""
        self._cards[5].set_value("gfs", value)

    @property
    def pwrc(self) -> typing.Optional[float]:
        """Get or set the Shear-to-compression transition parameter.
        """ # nopep8
        return self._cards[5].get_value("pwrc")

    @pwrc.setter
    def pwrc(self, value: float) -> None:
        """Set the pwrc property."""
        self._cards[5].set_value("pwrc", value)

    @property
    def pwrt(self) -> typing.Optional[float]:
        """Get or set the Shear-to-tension transition parameter.
        """ # nopep8
        return self._cards[5].get_value("pwrt")

    @pwrt.setter
    def pwrt(self, value: float) -> None:
        """Set the pwrt property."""
        self._cards[5].set_value("pwrt", value)

    @property
    def pmod(self) -> typing.Optional[float]:
        """Get or set the Modify moderate pressure softening parameter
        """ # nopep8
        return self._cards[5].get_value("pmod")

    @pmod.setter
    def pmod(self, value: float) -> None:
        """Set the pmod property."""
        self._cards[5].set_value("pmod", value)

    @property
    def eta0c(self) -> typing.Optional[float]:
        """Get or set the Rate effects parameter for uniaxial compressive stress,.
        """ # nopep8
        return self._cards[6].get_value("eta0c")

    @eta0c.setter
    def eta0c(self, value: float) -> None:
        """Set the eta0c property."""
        self._cards[6].set_value("eta0c", value)

    @property
    def nc(self) -> typing.Optional[float]:
        """Get or set the Rate effects power for uniaxial compressive stress.
        """ # nopep8
        return self._cards[6].get_value("nc")

    @nc.setter
    def nc(self, value: float) -> None:
        """Set the nc property."""
        self._cards[6].set_value("nc", value)

    @property
    def etaot(self) -> typing.Optional[float]:
        """Get or set the Rate effects parameter for uniaxial tensile stress,  0t.
        """ # nopep8
        return self._cards[6].get_value("etaot")

    @etaot.setter
    def etaot(self, value: float) -> None:
        """Set the etaot property."""
        self._cards[6].set_value("etaot", value)

    @property
    def nt(self) -> typing.Optional[float]:
        """Get or set the Rate effects power for uniaxial tensile stress,  Nt.
        """ # nopep8
        return self._cards[6].get_value("nt")

    @nt.setter
    def nt(self, value: float) -> None:
        """Set the nt property."""
        self._cards[6].set_value("nt", value)

    @property
    def overc(self) -> typing.Optional[float]:
        """Get or set the Maximum overstress allowed in compression
        """ # nopep8
        return self._cards[6].get_value("overc")

    @overc.setter
    def overc(self, value: float) -> None:
        """Set the overc property."""
        self._cards[6].set_value("overc", value)

    @property
    def overt(self) -> typing.Optional[float]:
        """Get or set the .Maximum overstress allowed in tension
        """ # nopep8
        return self._cards[6].get_value("overt")

    @overt.setter
    def overt(self, value: float) -> None:
        """Set the overt property."""
        self._cards[6].set_value("overt", value)

    @property
    def srate(self) -> typing.Optional[float]:
        """Get or set the Ratio of effective shear stress to tensile stress fluidity parameters.
        """ # nopep8
        return self._cards[6].get_value("srate")

    @srate.setter
    def srate(self, value: float) -> None:
        """Set the srate property."""
        self._cards[6].set_value("srate", value)

    @property
    def rep0w(self) -> typing.Optional[float]:
        """Get or set the Power which increases fracture energy with rate effects.
        """ # nopep8
        return self._cards[6].get_value("rep0w")

    @rep0w.setter
    def rep0w(self, value: float) -> None:
        """Set the rep0w property."""
        self._cards[6].set_value("rep0w", value)

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

