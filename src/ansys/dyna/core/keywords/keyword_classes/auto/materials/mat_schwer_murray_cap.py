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

"""Module providing the MatSchwerMurrayCap class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatSchwerMurrayCap(KeywordBase):
    """DYNA MAT_SCHWER_MURRAY_CAP keyword"""

    keyword = "MAT"
    subkeyword = "SCHWER_MURRAY_CAP"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatSchwerMurrayCap class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "shear",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "bulk",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "grun",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "shock",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pore",
                        float,
                        60,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "theta",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "efit",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ffit",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alphan",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "calpha",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ro",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "xo",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "irock",
                        float,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "secp",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "afit",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "bfit",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rdamo",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "w",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d1",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nplot",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "epsmax",
                        float,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cfit",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dfit",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tfail",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "failfg",
                        float,
                        0,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "dbeta",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ddelta",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vptau",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "theta1",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma1",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "theta2",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma2",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta2",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSchwerMurrayCap.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
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
    def shear(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, G.
        """ # nopep8
        return self._cards[0].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        """Set the shear property."""
        self._cards[0].set_value("shear", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus, K.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def grun(self) -> typing.Optional[float]:
        """Get or set the Gruneisen ratio (typically=0).
        """ # nopep8
        return self._cards[0].get_value("grun")

    @grun.setter
    def grun(self, value: float) -> None:
        """Set the grun property."""
        self._cards[0].set_value("grun", value)

    @property
    def shock(self) -> typing.Optional[float]:
        """Get or set the Shock velocity parameter (typically 0), S1.
        """ # nopep8
        return self._cards[0].get_value("shock")

    @shock.setter
    def shock(self, value: float) -> None:
        """Set the shock property."""
        self._cards[0].set_value("shock", value)

    @property
    def pore(self) -> float:
        """Get or set the Flag for pore collapse
        EQ.0.0 for Pore collapse.
        EQ.1.0 for Constant bulk modulus (typical)
        """ # nopep8
        return self._cards[0].get_value("pore")

    @pore.setter
    def pore(self, value: float) -> None:
        """Set the pore property."""
        if value not in [1.0, 0.0, None]:
            raise Exception("""pore must be `None` or one of {1.0,0.0}.""")
        self._cards[0].set_value("pore", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Shear failure parameter.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Shear failure parameter.
        """ # nopep8
        return self._cards[1].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[1].set_value("theta", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Shear failure parameter.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[1].set_value("gamma", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Shear failure parameter.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def efit(self) -> typing.Optional[float]:
        """Get or set the Dilitation damage mechanics parameter (no damage = 1).
        """ # nopep8
        return self._cards[1].get_value("efit")

    @efit.setter
    def efit(self, value: float) -> None:
        """Set the efit property."""
        self._cards[1].set_value("efit", value)

    @property
    def ffit(self) -> typing.Optional[float]:
        """Get or set the Dilitation damage mechanics parameter (no damage = 0).
        """ # nopep8
        return self._cards[1].get_value("ffit")

    @ffit.setter
    def ffit(self, value: float) -> None:
        """Set the ffit property."""
        self._cards[1].set_value("ffit", value)

    @property
    def alphan(self) -> typing.Optional[float]:
        """Get or set the Kinematic strain hardening parameter, Na.
        """ # nopep8
        return self._cards[1].get_value("alphan")

    @alphan.setter
    def alphan(self, value: float) -> None:
        """Set the alphan property."""
        self._cards[1].set_value("alphan", value)

    @property
    def calpha(self) -> typing.Optional[float]:
        """Get or set the Kinematic straing hardening parameter, Ca.
        """ # nopep8
        return self._cards[1].get_value("calpha")

    @calpha.setter
    def calpha(self, value: float) -> None:
        """Set the calpha property."""
        self._cards[1].set_value("calpha", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Initial cap surface ellipticity, R.
        """ # nopep8
        return self._cards[2].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[2].set_value("ro", value)

    @property
    def xo(self) -> typing.Optional[float]:
        """Get or set the Initial cap surface J1 (mean stress) axis intercept, X.
        """ # nopep8
        return self._cards[2].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        """Set the xo property."""
        self._cards[2].set_value("xo", value)

    @property
    def irock(self) -> float:
        """Get or set the EQ.0: soils (cap can contract)
        EQ.1: rock/concrete.
        """ # nopep8
        return self._cards[2].get_value("irock")

    @irock.setter
    def irock(self, value: float) -> None:
        """Set the irock property."""
        if value not in [0, 1, None]:
            raise Exception("""irock must be `None` or one of {0,1}.""")
        self._cards[2].set_value("irock", value)

    @property
    def secp(self) -> typing.Optional[float]:
        """Get or set the Shear enhanced compaction.
        """ # nopep8
        return self._cards[2].get_value("secp")

    @secp.setter
    def secp(self, value: float) -> None:
        """Set the secp property."""
        self._cards[2].set_value("secp", value)

    @property
    def afit(self) -> typing.Optional[float]:
        """Get or set the Ductile damage mechanics parameter (1=no damage).
        """ # nopep8
        return self._cards[2].get_value("afit")

    @afit.setter
    def afit(self, value: float) -> None:
        """Set the afit property."""
        self._cards[2].set_value("afit", value)

    @property
    def bfit(self) -> typing.Optional[float]:
        """Get or set the Ductile damage mechanics parameter (0=no damage).
        """ # nopep8
        return self._cards[2].get_value("bfit")

    @bfit.setter
    def bfit(self, value: float) -> None:
        """Set the bfit property."""
        self._cards[2].set_value("bfit", value)

    @property
    def rdamo(self) -> typing.Optional[float]:
        """Get or set the Ductile damage mechanics parameter.
        """ # nopep8
        return self._cards[2].get_value("rdamo")

    @rdamo.setter
    def rdamo(self, value: float) -> None:
        """Set the rdamo property."""
        self._cards[2].set_value("rdamo", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Plastic Volume Strain parameter, W.
        """ # nopep8
        return self._cards[3].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[3].set_value("w", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Plastic Volume Strain patameter, D1.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Plastic Volume Strain parameter, D2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def nplot(self) -> typing.Optional[float]:
        """Get or set the History variable post-processed as effective plastic strain (See Table 1 for history variables available for plotting).
        """ # nopep8
        return self._cards[3].get_value("nplot")

    @nplot.setter
    def nplot(self, value: float) -> None:
        """Set the nplot property."""
        self._cards[3].set_value("nplot", value)

    @property
    def epsmax(self) -> float:
        """Get or set the Maximum permitted strain increment (default=0)
        """ # nopep8
        return self._cards[3].get_value("epsmax")

    @epsmax.setter
    def epsmax(self, value: float) -> None:
        """Set the epsmax property."""
        self._cards[3].set_value("epsmax", value)

    @property
    def cfit(self) -> typing.Optional[float]:
        """Get or set the Brittle damage mechanics parameter (1=no damage).
        """ # nopep8
        return self._cards[3].get_value("cfit")

    @cfit.setter
    def cfit(self, value: float) -> None:
        """Set the cfit property."""
        self._cards[3].set_value("cfit", value)

    @property
    def dfit(self) -> typing.Optional[float]:
        """Get or set the Brittle damage mechanics parameter (0=no damage).
        """ # nopep8
        return self._cards[3].get_value("dfit")

    @dfit.setter
    def dfit(self, value: float) -> None:
        """Set the dfit property."""
        self._cards[3].set_value("dfit", value)

    @property
    def tfail(self) -> typing.Optional[float]:
        """Get or set the Tensile failure stress.
        """ # nopep8
        return self._cards[3].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        """Set the tfail property."""
        self._cards[3].set_value("tfail", value)

    @property
    def failfg(self) -> float:
        """Get or set the Failure Flag, failed element:
        EQ.0: stresses zeroed (use for ALE and EFG).
        EQ.1: removed from database (preferred).
        """ # nopep8
        return self._cards[4].get_value("failfg")

    @failfg.setter
    def failfg(self, value: float) -> None:
        """Set the failfg property."""
        if value not in [1, 0, None]:
            raise Exception("""failfg must be `None` or one of {1,0}.""")
        self._cards[4].set_value("failfg", value)

    @property
    def dbeta(self) -> typing.Optional[float]:
        """Get or set the Rounded vertices parameter.
        """ # nopep8
        return self._cards[4].get_value("dbeta")

    @dbeta.setter
    def dbeta(self, value: float) -> None:
        """Set the dbeta property."""
        self._cards[4].set_value("dbeta", value)

    @property
    def ddelta(self) -> typing.Optional[float]:
        """Get or set the Rounded vertices parameter.
        """ # nopep8
        return self._cards[4].get_value("ddelta")

    @ddelta.setter
    def ddelta(self, value: float) -> None:
        """Set the ddelta property."""
        self._cards[4].set_value("ddelta", value)

    @property
    def vptau(self) -> typing.Optional[float]:
        """Get or set the Viscoplasticity relaxation time parameter.
        """ # nopep8
        return self._cards[4].get_value("vptau")

    @vptau.setter
    def vptau(self, value: float) -> None:
        """Set the vptau property."""
        self._cards[4].set_value("vptau", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Torsion scaling parameter, a1.
        """ # nopep8
        return self._cards[5].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[5].set_value("alpha1", value)

    @property
    def theta1(self) -> typing.Optional[float]:
        """Get or set the Torsion scaling parameter, theta1.
        """ # nopep8
        return self._cards[5].get_value("theta1")

    @theta1.setter
    def theta1(self, value: float) -> None:
        """Set the theta1 property."""
        self._cards[5].set_value("theta1", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the Torsion scaling parameter, gamma1.
        """ # nopep8
        return self._cards[5].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        """Set the gamma1 property."""
        self._cards[5].set_value("gamma1", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Torsion scaling parameter, beta1.
        """ # nopep8
        return self._cards[5].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[5].set_value("beta1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension scaling parameter, a2.
        """ # nopep8
        return self._cards[5].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[5].set_value("alpha2", value)

    @property
    def theta2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension scaling parameter, thetha2.
        """ # nopep8
        return self._cards[5].get_value("theta2")

    @theta2.setter
    def theta2(self, value: float) -> None:
        """Set the theta2 property."""
        self._cards[5].set_value("theta2", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension scaling parameter, gamma2.
        """ # nopep8
        return self._cards[5].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        """Set the gamma2 property."""
        self._cards[5].set_value("gamma2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Tri-axial extension scaling parameter, beta2.
        """ # nopep8
        return self._cards[5].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[5].set_value("beta2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

