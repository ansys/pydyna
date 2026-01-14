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

"""Module providing the EmEpCellmodelTentusscher class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMEPCELLMODELTENTUSSCHER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD1 = (
    FieldSchema("r", float, 0, 10, None),
    FieldSchema("t", float, 10, 10, None),
    FieldSchema("f", float, 20, 10, None),
    FieldSchema("cm", float, 30, 10, None),
    FieldSchema("vc", float, 40, 10, None),
    FieldSchema("vsr", float, 50, 10, None),
    FieldSchema("vss", float, 60, 10, None),
    FieldSchema("pkna", float, 70, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD2 = (
    FieldSchema("ko", float, 0, 10, None),
    FieldSchema("nao", float, 10, 10, None),
    FieldSchema("cao", float, 20, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD3 = (
    FieldSchema("gk1", float, 0, 10, None),
    FieldSchema("gkr", float, 10, 10, None),
    FieldSchema("gks", float, 20, 10, None),
    FieldSchema("gna", float, 30, 10, None),
    FieldSchema("gbna", float, 40, 10, None),
    FieldSchema("gcal", float, 50, 10, None),
    FieldSchema("gbca", float, 60, 10, None),
    FieldSchema("gto", float, 70, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD4 = (
    FieldSchema("gpca", float, 0, 10, None),
    FieldSchema("gpk", float, 10, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD5 = (
    FieldSchema("pnak", float, 0, 10, None),
    FieldSchema("km", float, 10, 10, None),
    FieldSchema("kmna", float, 20, 10, None),
    FieldSchema("knaca", float, 30, 10, None),
    FieldSchema("ksat", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("gamma", float, 60, 10, None),
    FieldSchema("kmca", float, 70, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD6 = (
    FieldSchema("kmnai", float, 0, 10, None),
    FieldSchema("kpca", float, 10, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD7 = (
    FieldSchema("k1", float, 0, 10, None),
    FieldSchema("k2", float, 10, 10, None),
    FieldSchema("k3", float, 20, 10, None),
    FieldSchema("k4", float, 30, 10, None),
    FieldSchema("ec", float, 40, 10, None),
    FieldSchema("maxsr", float, 50, 10, None),
    FieldSchema("minsr", float, 60, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD8 = (
    FieldSchema("vrel", float, 0, 10, None),
    FieldSchema("vleak", float, 10, 10, None),
    FieldSchema("vxfer", float, 20, 10, None),
    FieldSchema("vmaxup", float, 30, 10, None),
    FieldSchema("kup", float, 40, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD9 = (
    FieldSchema("bufc", float, 0, 10, None),
    FieldSchema("kbufc", float, 10, 10, None),
    FieldSchema("bufsr", float, 20, 10, None),
    FieldSchema("kbufsf", float, 30, 10, None),
    FieldSchema("bufss", float, 40, 10, None),
    FieldSchema("kbufss", float, 50, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD10 = (
    FieldSchema("v", float, 0, 10, None),
    FieldSchema("ki", float, 10, 10, None),
    FieldSchema("nai", float, 20, 10, None),
    FieldSchema("cai", float, 30, 10, None),
    FieldSchema("cass", float, 40, 10, None),
    FieldSchema("casr", float, 50, 10, None),
    FieldSchema("rpri", float, 60, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD11 = (
    FieldSchema("xr1", float, 0, 10, None),
    FieldSchema("xr2", float, 10, 10, None),
    FieldSchema("xs", float, 20, 10, None),
    FieldSchema("m", float, 30, 10, None),
    FieldSchema("h", float, 40, 10, None),
    FieldSchema("j", float, 50, 10, None),
    FieldSchema("d", float, 60, 10, None),
    FieldSchema("f", float, 70, 10, None),
)

_EMEPCELLMODELTENTUSSCHER_CARD12 = (
    FieldSchema("f2", float, 0, 10, None),
    FieldSchema("fcass", float, 10, 10, None),
    FieldSchema("s", float, 20, 10, None),
    FieldSchema("r", float, 30, 10, None),
)

class EmEpCellmodelTentusscher(KeywordBase):
    """DYNA EM_EP_CELLMODEL_TENTUSSCHER keyword

    Fields with renamed properties:
        R (Card 2) -> gas_constant
        F (Card 2) -> faraday_constant
    """

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_TENTUSSCHER"

    def __init__(self, **kwargs):
        """Initialize the EmEpCellmodelTentusscher class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD10,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD11,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELTENTUSSCHER_CARD12,
                **kwargs,
            ),        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT_.

        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def gas_constant(self) -> typing.Optional[float]:
        """Get or set the Gas constant.

        Note: Location in manual: card:2, field: R
        """ # nopep8
        return self._cards[1].get_value("r")

    @gas_constant.setter
    def gas_constant(self, value: float) -> None:
        """Set the gas_constant property."""
        self._cards[1].set_value("r", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Temperature.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def faraday_constant(self) -> typing.Optional[float]:
        """Get or set the Faraday constant .

        Note: Location in manual: card:2, field: F
        """ # nopep8
        return self._cards[1].get_value("f")

    @faraday_constant.setter
    def faraday_constant(self, value: float) -> None:
        """Set the faraday_constant property."""
        self._cards[1].set_value("f", value)

    @property
    def cm(self) -> typing.Optional[float]:
        """Get or set the Cell capacitance for unit surface area.
        """ # nopep8
        return self._cards[1].get_value("cm")

    @cm.setter
    def cm(self, value: float) -> None:
        """Set the cm property."""
        self._cards[1].set_value("cm", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Cytoplasmic volume.
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        """Set the vc property."""
        self._cards[1].set_value("vc", value)

    @property
    def vsr(self) -> typing.Optional[float]:
        """Get or set the Sarcoplasmic reticulum volume.
        """ # nopep8
        return self._cards[1].get_value("vsr")

    @vsr.setter
    def vsr(self, value: float) -> None:
        """Set the vsr property."""
        self._cards[1].set_value("vsr", value)

    @property
    def vss(self) -> typing.Optional[float]:
        """Get or set the Subspace volume.
        """ # nopep8
        return self._cards[1].get_value("vss")

    @vss.setter
    def vss(self, value: float) -> None:
        """Set the vss property."""
        self._cards[1].set_value("vss", value)

    @property
    def pkna(self) -> typing.Optional[float]:
        """Get or set the Relative Iks permeability to Na+
        """ # nopep8
        return self._cards[1].get_value("pkna")

    @pkna.setter
    def pkna(self, value: float) -> None:
        """Set the pkna property."""
        self._cards[1].set_value("pkna", value)

    @property
    def ko(self) -> typing.Optional[float]:
        """Get or set the Extracellular K+ concentration.
        """ # nopep8
        return self._cards[2].get_value("ko")

    @ko.setter
    def ko(self, value: float) -> None:
        """Set the ko property."""
        self._cards[2].set_value("ko", value)

    @property
    def nao(self) -> typing.Optional[float]:
        """Get or set the Extracellular Na+ concentration.
        """ # nopep8
        return self._cards[2].get_value("nao")

    @nao.setter
    def nao(self, value: float) -> None:
        """Set the nao property."""
        self._cards[2].set_value("nao", value)

    @property
    def cao(self) -> typing.Optional[float]:
        """Get or set the Extracellular Ca2+ concentration.
        """ # nopep8
        return self._cards[2].get_value("cao")

    @cao.setter
    def cao(self, value: float) -> None:
        """Set the cao property."""
        self._cards[2].set_value("cao", value)

    @property
    def gk1(self) -> typing.Optional[float]:
        """Get or set the Ik1 conductance.
        """ # nopep8
        return self._cards[3].get_value("gk1")

    @gk1.setter
    def gk1(self, value: float) -> None:
        """Set the gk1 property."""
        self._cards[3].set_value("gk1", value)

    @property
    def gkr(self) -> typing.Optional[float]:
        """Get or set the Ikr conductance.
        """ # nopep8
        return self._cards[3].get_value("gkr")

    @gkr.setter
    def gkr(self, value: float) -> None:
        """Set the gkr property."""
        self._cards[3].set_value("gkr", value)

    @property
    def gks(self) -> typing.Optional[float]:
        """Get or set the Iks conductance.
        """ # nopep8
        return self._cards[3].get_value("gks")

    @gks.setter
    def gks(self, value: float) -> None:
        """Set the gks property."""
        self._cards[3].set_value("gks", value)

    @property
    def gna(self) -> typing.Optional[float]:
        """Get or set the Ina conductance.
        """ # nopep8
        return self._cards[3].get_value("gna")

    @gna.setter
    def gna(self, value: float) -> None:
        """Set the gna property."""
        self._cards[3].set_value("gna", value)

    @property
    def gbna(self) -> typing.Optional[float]:
        """Get or set the Ibna conductance.
        """ # nopep8
        return self._cards[3].get_value("gbna")

    @gbna.setter
    def gbna(self, value: float) -> None:
        """Set the gbna property."""
        self._cards[3].set_value("gbna", value)

    @property
    def gcal(self) -> typing.Optional[float]:
        """Get or set the Ical conductance.
        """ # nopep8
        return self._cards[3].get_value("gcal")

    @gcal.setter
    def gcal(self, value: float) -> None:
        """Set the gcal property."""
        self._cards[3].set_value("gcal", value)

    @property
    def gbca(self) -> typing.Optional[float]:
        """Get or set the Ibca conductance.
        """ # nopep8
        return self._cards[3].get_value("gbca")

    @gbca.setter
    def gbca(self, value: float) -> None:
        """Set the gbca property."""
        self._cards[3].set_value("gbca", value)

    @property
    def gto(self) -> typing.Optional[float]:
        """Get or set the Ito conductance.
        """ # nopep8
        return self._cards[3].get_value("gto")

    @gto.setter
    def gto(self, value: float) -> None:
        """Set the gto property."""
        self._cards[3].set_value("gto", value)

    @property
    def gpca(self) -> typing.Optional[float]:
        """Get or set the Ipca conductance.
        """ # nopep8
        return self._cards[4].get_value("gpca")

    @gpca.setter
    def gpca(self, value: float) -> None:
        """Set the gpca property."""
        self._cards[4].set_value("gpca", value)

    @property
    def gpk(self) -> typing.Optional[float]:
        """Get or set the Ikp conductance.
        """ # nopep8
        return self._cards[4].get_value("gpk")

    @gpk.setter
    def gpk(self, value: float) -> None:
        """Set the gpk property."""
        self._cards[4].set_value("gpk", value)

    @property
    def pnak(self) -> typing.Optional[float]:
        """Get or set the P_NaK: sodium potassium pump current (picoA_per_picoF).
        """ # nopep8
        return self._cards[5].get_value("pnak")

    @pnak.setter
    def pnak(self, value: float) -> None:
        """Set the pnak property."""
        self._cards[5].set_value("pnak", value)

    @property
    def km(self) -> typing.Optional[float]:
        """Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("km")

    @km.setter
    def km(self, value: float) -> None:
        """Set the km property."""
        self._cards[5].set_value("km", value)

    @property
    def kmna(self) -> typing.Optional[float]:
        """Get or set the K_mK and K_MNa in component sodium_potassium_pump_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("kmna")

    @kmna.setter
    def kmna(self, value: float) -> None:
        """Set the kmna property."""
        self._cards[5].set_value("kmna", value)

    @property
    def knaca(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("knaca")

    @knaca.setter
    def knaca(self, value: float) -> None:
        """Set the knaca property."""
        self._cards[5].set_value("knaca", value)

    @property
    def ksat(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("ksat")

    @ksat.setter
    def ksat(self, value: float) -> None:
        """Set the ksat property."""
        self._cards[5].set_value("ksat", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[5].set_value("alpha", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[5].set_value("gamma", value)

    @property
    def kmca(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[5].get_value("kmca")

    @kmca.setter
    def kmca(self, value: float) -> None:
        """Set the kmca property."""
        self._cards[5].set_value("kmca", value)

    @property
    def kmnai(self) -> typing.Optional[float]:
        """Get or set the Components in component sodium_calcium_exchanger_current (millimolar)
        """ # nopep8
        return self._cards[6].get_value("kmnai")

    @kmnai.setter
    def kmnai(self, value: float) -> None:
        """Set the kmnai property."""
        self._cards[6].set_value("kmnai", value)

    @property
    def kpca(self) -> typing.Optional[float]:
        """Get or set the K_pCa: component in calcium_pump_current (millimolar).
        """ # nopep8
        return self._cards[6].get_value("kpca")

    @kpca.setter
    def kpca(self, value: float) -> None:
        """Set the kpca property."""
        self._cards[6].set_value("kpca", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the R to O and RI to I Irel transition rate.
        """ # nopep8
        return self._cards[7].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[7].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the O to I and R to RI Irel transition rate.
        """ # nopep8
        return self._cards[7].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[7].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the O to R and I to RI Irel transition rate.
        """ # nopep8
        return self._cards[7].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        """Set the k3 property."""
        self._cards[7].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[float]:
        """Get or set the I to O and RI to I Irel transition rate
        """ # nopep8
        return self._cards[7].get_value("k4")

    @k4.setter
    def k4(self, value: float) -> None:
        """Set the k4 property."""
        self._cards[7].set_value("k4", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the CaSR half-saturation constant of Kcasr.
        """ # nopep8
        return self._cards[7].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[7].set_value("ec", value)

    @property
    def maxsr(self) -> typing.Optional[float]:
        """Get or set the Maximum value of Kcasr
        """ # nopep8
        return self._cards[7].get_value("maxsr")

    @maxsr.setter
    def maxsr(self, value: float) -> None:
        """Set the maxsr property."""
        self._cards[7].set_value("maxsr", value)

    @property
    def minsr(self) -> typing.Optional[float]:
        """Get or set the Minimum value of Kcasr
        """ # nopep8
        return self._cards[7].get_value("minsr")

    @minsr.setter
    def minsr(self, value: float) -> None:
        """Set the minsr property."""
        self._cards[7].set_value("minsr", value)

    @property
    def vrel(self) -> typing.Optional[float]:
        """Get or set the Maximal Irel conductance.
        """ # nopep8
        return self._cards[8].get_value("vrel")

    @vrel.setter
    def vrel(self, value: float) -> None:
        """Set the vrel property."""
        self._cards[8].set_value("vrel", value)

    @property
    def vleak(self) -> typing.Optional[float]:
        """Get or set the Maximal Ileak conductance.
        """ # nopep8
        return self._cards[8].get_value("vleak")

    @vleak.setter
    def vleak(self, value: float) -> None:
        """Set the vleak property."""
        self._cards[8].set_value("vleak", value)

    @property
    def vxfer(self) -> typing.Optional[float]:
        """Get or set the Maximal Ixfer conductance.
        """ # nopep8
        return self._cards[8].get_value("vxfer")

    @vxfer.setter
    def vxfer(self, value: float) -> None:
        """Set the vxfer property."""
        self._cards[8].set_value("vxfer", value)

    @property
    def vmaxup(self) -> typing.Optional[float]:
        """Get or set the Maximal Iup conductance.
        """ # nopep8
        return self._cards[8].get_value("vmaxup")

    @vmaxup.setter
    def vmaxup(self, value: float) -> None:
        """Set the vmaxup property."""
        self._cards[8].set_value("vmaxup", value)

    @property
    def kup(self) -> typing.Optional[float]:
        """Get or set the Half-saturation constant of Iup.
        """ # nopep8
        return self._cards[8].get_value("kup")

    @kup.setter
    def kup(self, value: float) -> None:
        """Set the kup property."""
        self._cards[8].set_value("kup", value)

    @property
    def bufc(self) -> typing.Optional[float]:
        """Get or set the Total cytoplasmic buffer concentration.
        """ # nopep8
        return self._cards[9].get_value("bufc")

    @bufc.setter
    def bufc(self, value: float) -> None:
        """Set the bufc property."""
        self._cards[9].set_value("bufc", value)

    @property
    def kbufc(self) -> typing.Optional[float]:
        """Get or set the Cai half-saturation constant for cytoplasmic buffer.
        """ # nopep8
        return self._cards[9].get_value("kbufc")

    @kbufc.setter
    def kbufc(self, value: float) -> None:
        """Set the kbufc property."""
        self._cards[9].set_value("kbufc", value)

    @property
    def bufsr(self) -> typing.Optional[float]:
        """Get or set the Total sarcoplasmic buffer concentration (mM)
        """ # nopep8
        return self._cards[9].get_value("bufsr")

    @bufsr.setter
    def bufsr(self, value: float) -> None:
        """Set the bufsr property."""
        self._cards[9].set_value("bufsr", value)

    @property
    def kbufsf(self) -> typing.Optional[float]:
        """Get or set the CaSR half-saturation constant for sarcoplasmic buffer.
        """ # nopep8
        return self._cards[9].get_value("kbufsf")

    @kbufsf.setter
    def kbufsf(self, value: float) -> None:
        """Set the kbufsf property."""
        self._cards[9].set_value("kbufsf", value)

    @property
    def bufss(self) -> typing.Optional[float]:
        """Get or set the Total subspace buffer concentration.
        """ # nopep8
        return self._cards[9].get_value("bufss")

    @bufss.setter
    def bufss(self, value: float) -> None:
        """Set the bufss property."""
        self._cards[9].set_value("bufss", value)

    @property
    def kbufss(self) -> typing.Optional[float]:
        """Get or set the CaSS half-saturation constant for subspace buffer.
        """ # nopep8
        return self._cards[9].get_value("kbufss")

    @kbufss.setter
    def kbufss(self, value: float) -> None:
        """Set the kbufss property."""
        self._cards[9].set_value("kbufss", value)

    @property
    def v(self) -> typing.Optional[float]:
        """Get or set the Initial value of transmembrane potential.
        """ # nopep8
        return self._cards[10].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        """Set the v property."""
        self._cards[10].set_value("v", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Initial value of K_i in component potassium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        """Set the ki property."""
        self._cards[10].set_value("ki", value)

    @property
    def nai(self) -> typing.Optional[float]:
        """Get or set the Initial value of Na_i in component sodium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("nai")

    @nai.setter
    def nai(self, value: float) -> None:
        """Set the nai property."""
        self._cards[10].set_value("nai", value)

    @property
    def cai(self) -> typing.Optional[float]:
        """Get or set the Initial value of Ca_i in component calcium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("cai")

    @cai.setter
    def cai(self, value: float) -> None:
        """Set the cai property."""
        self._cards[10].set_value("cai", value)

    @property
    def cass(self) -> typing.Optional[float]:
        """Get or set the Initial value of Ca_ss in component calcium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("cass")

    @cass.setter
    def cass(self, value: float) -> None:
        """Set the cass property."""
        self._cards[10].set_value("cass", value)

    @property
    def casr(self) -> typing.Optional[float]:
        """Get or set the Initial value of Ca_SR in component calcium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("casr")

    @casr.setter
    def casr(self, value: float) -> None:
        """Set the casr property."""
        self._cards[10].set_value("casr", value)

    @property
    def rpri(self) -> typing.Optional[float]:
        """Get or set the Initial value of R?in component calcium_dynamics.
        """ # nopep8
        return self._cards[10].get_value("rpri")

    @rpri.setter
    def rpri(self, value: float) -> None:
        """Set the rpri property."""
        self._cards[10].set_value("rpri", value)

    @property
    def xr1(self) -> typing.Optional[float]:
        """Get or set the Initial value of Xr1 in component rapid time dependent potassium current Xr1 gate.
        """ # nopep8
        return self._cards[11].get_value("xr1")

    @xr1.setter
    def xr1(self, value: float) -> None:
        """Set the xr1 property."""
        self._cards[11].set_value("xr1", value)

    @property
    def xr2(self) -> typing.Optional[float]:
        """Get or set the Initial value of Xr2 in component rapid time dependent potassium current Xr2 gate.
        """ # nopep8
        return self._cards[11].get_value("xr2")

    @xr2.setter
    def xr2(self, value: float) -> None:
        """Set the xr2 property."""
        self._cards[11].set_value("xr2", value)

    @property
    def xs(self) -> typing.Optional[float]:
        """Get or set the Initial value of Xs in component slow time dependent potassium current Xs gate.
        """ # nopep8
        return self._cards[11].get_value("xs")

    @xs.setter
    def xs(self, value: float) -> None:
        """Set the xs property."""
        self._cards[11].set_value("xs", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Initial value of m in component fast_sodium_current_m_gate.
        """ # nopep8
        return self._cards[11].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[11].set_value("m", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Initial value of h in component fast_sodium_current_h_gate.
        """ # nopep8
        return self._cards[11].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[11].set_value("h", value)

    @property
    def j(self) -> typing.Optional[float]:
        """Get or set the Initial value of j in component fast_sodium_current_j_gate.
        """ # nopep8
        return self._cards[11].get_value("j")

    @j.setter
    def j(self, value: float) -> None:
        """Set the j property."""
        self._cards[11].set_value("j", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Initial value of d in component L_type_Ca_current_d_gate.
        """ # nopep8
        return self._cards[11].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[11].set_value("d", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Initial value of f in component L_type_Ca_current_f_gate.

        Note: For the Faraday constant F (Card 2), use ``faraday_constant``.
        """ # nopep8
        return self._cards[11].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[11].set_value("f", value)

    @property
    def f2(self) -> typing.Optional[float]:
        """Get or set the Initial value of f2 in component L_type_Ca_current_f2_gate.
        """ # nopep8
        return self._cards[12].get_value("f2")

    @f2.setter
    def f2(self, value: float) -> None:
        """Set the f2 property."""
        self._cards[12].set_value("f2", value)

    @property
    def fcass(self) -> typing.Optional[float]:
        """Get or set the Initial value of fCass in component L_type_Ca_current_fCass_gate.
        """ # nopep8
        return self._cards[12].get_value("fcass")

    @fcass.setter
    def fcass(self, value: float) -> None:
        """Set the fcass property."""
        self._cards[12].set_value("fcass", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Initial value of s in component transient_outward_current_s_gate.
        """ # nopep8
        return self._cards[12].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        """Set the s property."""
        self._cards[12].set_value("s", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Initial value of r in component transient_outward_current_r_gate.

        Note: For the gas constant R (Card 2), use ``gas_constant``.
        """ # nopep8
        return self._cards[12].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[12].set_value("r", value)

