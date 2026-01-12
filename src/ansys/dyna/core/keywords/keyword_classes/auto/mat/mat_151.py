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

"""Module providing the Mat151 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT151_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("rho", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
)

_MAT151_CARD1 = (
    FieldSchema("rgas", float, 0, 10, None),
    FieldSchema("bvect", float, 10, 10, None),
    FieldSchema("d0", float, 20, 10, None),
    FieldSchema("qd", float, 30, 10, None),
    FieldSchema("cv", float, 40, 10, None),
    FieldSchema("adrag", float, 50, 10, None),
    FieldSchema("bdrag", float, 60, 10, None),
    FieldSchema("dmtheta", float, 70, 10, None),
)

_MAT151_CARD2 = (
    FieldSchema("dmphi", float, 0, 10, None),
    FieldSchema("dntheta", float, 10, 10, None),
    FieldSchema("dnphi", float, 20, 10, None),
    FieldSchema("theta0", float, 30, 10, None),
    FieldSchema("thetam", float, 40, 10, None),
    FieldSchema("beta0", float, 50, 10, None),
    FieldSchema("btheta", float, 60, 10, None),
    FieldSchema("dmr", float, 70, 10, None),
)

_MAT151_CARD3 = (
    FieldSchema("dnuc1", float, 0, 10, None),
    FieldSchema("dnuc2", float, 10, 10, None),
    FieldSchema("dnuc3", float, 20, 10, None),
    FieldSchema("dnuc4", float, 30, 10, None),
    FieldSchema("dm1", float, 40, 10, None),
    FieldSchema("dm2", float, 50, 10, None),
    FieldSchema("dm3", float, 60, 10, None),
    FieldSchema("dm4", float, 70, 10, None),
)

_MAT151_CARD4 = (
    FieldSchema("dm5", float, 0, 10, None),
    FieldSchema("qind", float, 10, 10, None),
    FieldSchema("q2nd", float, 20, 10, None),
    FieldSchema("q3nd", float, 30, 10, None),
    FieldSchema("q4nd", float, 40, 10, None),
    FieldSchema("calpha", float, 50, 10, None),
    FieldSchema("ckappa", float, 60, 10, None),
    FieldSchema("c1", float, 70, 10, None),
)

_MAT151_CARD5 = (
    FieldSchema("c2nd", float, 0, 10, None),
    FieldSchema("c3", float, 10, 10, None),
    FieldSchema("c4", float, 20, 10, None),
    FieldSchema("c5", float, 30, 10, None),
    FieldSchema("c6", float, 40, 10, None),
    FieldSchema("c7nd", float, 50, 10, None),
    FieldSchema("c8nd", float, 60, 10, None),
    FieldSchema("c9nd", float, 70, 10, None),
)

_MAT151_CARD6 = (
    FieldSchema("c10", float, 0, 10, None),
    FieldSchema("a1", float, 10, 10, None),
    FieldSchema("a2", float, 20, 10, None),
    FieldSchema("a3", float, 30, 10, None),
    FieldSchema("a4", float, 40, 10, None),
    FieldSchema("a_xx", float, 50, 10, None),
    FieldSchema("a_yy", float, 60, 10, None),
    FieldSchema("a_zz", float, 70, 10, None),
)

_MAT151_CARD7 = (
    FieldSchema("a_xy", float, 0, 10, None),
    FieldSchema("a_yz", float, 10, 10, None),
    FieldSchema("a_xz", float, 20, 10, None),
    FieldSchema("alphxx", float, 30, 10, None),
    FieldSchema("alphyy", float, 40, 10, None),
    FieldSchema("alphzz", float, 50, 10, None),
    FieldSchema("alphxy", float, 60, 10, None),
    FieldSchema("alphyz", float, 70, 10, None),
)

_MAT151_CARD8 = (
    FieldSchema("alphxz", float, 0, 10, None),
    FieldSchema("dkappa", float, 10, 10, None),
    FieldSchema("phi0", float, 20, 10, None),
    FieldSchema("phicr", float, 30, 10, None),
    FieldSchema("dlbdag", float, 40, 10, None),
    FieldSchema("factor", float, 50, 10, None),
    FieldSchema("rswtch", float, 60, 10, None),
    FieldSchema("dmgopt", float, 70, 10, None),
)

_MAT151_CARD9 = (
    FieldSchema("delaso", float, 0, 10, None),
    FieldSchema("dimplo", float, 10, 10, None),
    FieldSchema("atol", float, 20, 10, None),
    FieldSchema("rtol", float, 30, 10, None),
    FieldSchema("dniter", float, 40, 10, None),
)

_MAT151_CARD10 = (
    FieldSchema("unused", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("unused", float, 70, 10, None),
)

class Mat151(KeywordBase):
    """DYNA MAT_151 keyword"""

    keyword = "MAT"
    subkeyword = "151"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat151 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT151_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT151_CARD10,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat151.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Material density
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def rgas(self) -> typing.Optional[float]:
        """Get or set the universal gas constant.
        """ # nopep8
        return self._cards[1].get_value("rgas")

    @rgas.setter
    def rgas(self, value: float) -> None:
        """Set the rgas property."""
        self._cards[1].set_value("rgas", value)

    @property
    def bvect(self) -> typing.Optional[float]:
        """Get or set the Burger's vector
        """ # nopep8
        return self._cards[1].get_value("bvect")

    @bvect.setter
    def bvect(self, value: float) -> None:
        """Set the bvect property."""
        self._cards[1].set_value("bvect", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the pre-exponential diffusivity coefficient
        """ # nopep8
        return self._cards[1].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        """Set the d0 property."""
        self._cards[1].set_value("d0", value)

    @property
    def qd(self) -> typing.Optional[float]:
        """Get or set the activation energy.
        """ # nopep8
        return self._cards[1].get_value("qd")

    @qd.setter
    def qd(self, value: float) -> None:
        """Set the qd property."""
        self._cards[1].set_value("qd", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the specific heat at constant volume
        """ # nopep8
        return self._cards[1].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        """Set the cv property."""
        self._cards[1].set_value("cv", value)

    @property
    def adrag(self) -> typing.Optional[float]:
        """Get or set the drag intercept
        """ # nopep8
        return self._cards[1].get_value("adrag")

    @adrag.setter
    def adrag(self, value: float) -> None:
        """Set the adrag property."""
        self._cards[1].set_value("adrag", value)

    @property
    def bdrag(self) -> typing.Optional[float]:
        """Get or set the drag coefficient
        """ # nopep8
        return self._cards[1].get_value("bdrag")

    @bdrag.setter
    def bdrag(self, value: float) -> None:
        """Set the bdrag property."""
        self._cards[1].set_value("bdrag", value)

    @property
    def dmtheta(self) -> typing.Optional[float]:
        """Get or set the shear modulus temperature coefficient
        """ # nopep8
        return self._cards[1].get_value("dmtheta")

    @dmtheta.setter
    def dmtheta(self, value: float) -> None:
        """Set the dmtheta property."""
        self._cards[1].set_value("dmtheta", value)

    @property
    def dmphi(self) -> typing.Optional[float]:
        """Get or set the shear modulus damage coefficient.
        """ # nopep8
        return self._cards[2].get_value("dmphi")

    @dmphi.setter
    def dmphi(self, value: float) -> None:
        """Set the dmphi property."""
        self._cards[2].set_value("dmphi", value)

    @property
    def dntheta(self) -> typing.Optional[float]:
        """Get or set the bulk modulus temperature coefficient
        """ # nopep8
        return self._cards[2].get_value("dntheta")

    @dntheta.setter
    def dntheta(self, value: float) -> None:
        """Set the dntheta property."""
        self._cards[2].set_value("dntheta", value)

    @property
    def dnphi(self) -> typing.Optional[float]:
        """Get or set the bulk modulus damage coefficient
        """ # nopep8
        return self._cards[2].get_value("dnphi")

    @dnphi.setter
    def dnphi(self, value: float) -> None:
        """Set the dnphi property."""
        self._cards[2].set_value("dnphi", value)

    @property
    def theta0(self) -> typing.Optional[float]:
        """Get or set the reference temperature.
        """ # nopep8
        return self._cards[2].get_value("theta0")

    @theta0.setter
    def theta0(self, value: float) -> None:
        """Set the theta0 property."""
        self._cards[2].set_value("theta0", value)

    @property
    def thetam(self) -> typing.Optional[float]:
        """Get or set the melt temperature
        """ # nopep8
        return self._cards[2].get_value("thetam")

    @thetam.setter
    def thetam(self, value: float) -> None:
        """Set the thetam property."""
        self._cards[2].set_value("thetam", value)

    @property
    def beta0(self) -> typing.Optional[float]:
        """Get or set the coefficient of thermal expansion at reference temperature
        """ # nopep8
        return self._cards[2].get_value("beta0")

    @beta0.setter
    def beta0(self, value: float) -> None:
        """Set the beta0 property."""
        self._cards[2].set_value("beta0", value)

    @property
    def btheta(self) -> typing.Optional[float]:
        """Get or set the thermal expansion temperature coefficient
        """ # nopep8
        return self._cards[2].get_value("btheta")

    @btheta.setter
    def btheta(self, value: float) -> None:
        """Set the btheta property."""
        self._cards[2].set_value("btheta", value)

    @property
    def dmr(self) -> typing.Optional[float]:
        """Get or set the damage rate sensitivity parameter
        """ # nopep8
        return self._cards[2].get_value("dmr")

    @dmr.setter
    def dmr(self, value: float) -> None:
        """Set the dmr property."""
        self._cards[2].set_value("dmr", value)

    @property
    def dnuc1(self) -> typing.Optional[float]:
        """Get or set the Nucleation coefficient .
        """ # nopep8
        return self._cards[3].get_value("dnuc1")

    @dnuc1.setter
    def dnuc1(self, value: float) -> None:
        """Set the dnuc1 property."""
        self._cards[3].set_value("dnuc1", value)

    @property
    def dnuc2(self) -> typing.Optional[float]:
        """Get or set the Nucleation coefficient
        """ # nopep8
        return self._cards[3].get_value("dnuc2")

    @dnuc2.setter
    def dnuc2(self, value: float) -> None:
        """Set the dnuc2 property."""
        self._cards[3].set_value("dnuc2", value)

    @property
    def dnuc3(self) -> typing.Optional[float]:
        """Get or set the Nucleation coefficient
        """ # nopep8
        return self._cards[3].get_value("dnuc3")

    @dnuc3.setter
    def dnuc3(self, value: float) -> None:
        """Set the dnuc3 property."""
        self._cards[3].set_value("dnuc3", value)

    @property
    def dnuc4(self) -> typing.Optional[float]:
        """Get or set the Nucleation coefficient
        """ # nopep8
        return self._cards[3].get_value("dnuc4")

    @dnuc4.setter
    def dnuc4(self, value: float) -> None:
        """Set the dnuc4 property."""
        self._cards[3].set_value("dnuc4", value)

    @property
    def dm1(self) -> typing.Optional[float]:
        """Get or set the coefficient of yield temperature dependence.
        """ # nopep8
        return self._cards[3].get_value("dm1")

    @dm1.setter
    def dm1(self, value: float) -> None:
        """Set the dm1 property."""
        self._cards[3].set_value("dm1", value)

    @property
    def dm2(self) -> typing.Optional[float]:
        """Get or set the coefficient of yield temperature dependence
        """ # nopep8
        return self._cards[3].get_value("dm2")

    @dm2.setter
    def dm2(self, value: float) -> None:
        """Set the dm2 property."""
        self._cards[3].set_value("dm2", value)

    @property
    def dm3(self) -> typing.Optional[float]:
        """Get or set the coefficient of yield temperature dependence
        """ # nopep8
        return self._cards[3].get_value("dm3")

    @dm3.setter
    def dm3(self, value: float) -> None:
        """Set the dm3 property."""
        self._cards[3].set_value("dm3", value)

    @property
    def dm4(self) -> typing.Optional[float]:
        """Get or set the coefficient of yield temperature dependence.
        """ # nopep8
        return self._cards[3].get_value("dm4")

    @dm4.setter
    def dm4(self, value: float) -> None:
        """Set the dm4 property."""
        self._cards[3].set_value("dm4", value)

    @property
    def dm5(self) -> typing.Optional[float]:
        """Get or set the coefficient of yield temperature dependence
        """ # nopep8
        return self._cards[4].get_value("dm5")

    @dm5.setter
    def dm5(self, value: float) -> None:
        """Set the dm5 property."""
        self._cards[4].set_value("dm5", value)

    @property
    def qind(self) -> typing.Optional[float]:
        """Get or set the dimensionless activation energy for f
        """ # nopep8
        return self._cards[4].get_value("qind")

    @qind.setter
    def qind(self, value: float) -> None:
        """Set the qind property."""
        self._cards[4].set_value("qind", value)

    @property
    def q2nd(self) -> typing.Optional[float]:
        """Get or set the dimensionless activation energy for rd
        """ # nopep8
        return self._cards[4].get_value("q2nd")

    @q2nd.setter
    def q2nd(self, value: float) -> None:
        """Set the q2nd property."""
        self._cards[4].set_value("q2nd", value)

    @property
    def q3nd(self) -> typing.Optional[float]:
        """Get or set the dimensionless activation energy for Rd
        """ # nopep8
        return self._cards[4].get_value("q3nd")

    @q3nd.setter
    def q3nd(self, value: float) -> None:
        """Set the q3nd property."""
        self._cards[4].set_value("q3nd", value)

    @property
    def q4nd(self) -> typing.Optional[float]:
        """Get or set the dimensionless activation energy Rs.
        """ # nopep8
        return self._cards[4].get_value("q4nd")

    @q4nd.setter
    def q4nd(self, value: float) -> None:
        """Set the q4nd property."""
        self._cards[4].set_value("q4nd", value)

    @property
    def calpha(self) -> typing.Optional[float]:
        """Get or set the coefficient for backstress alpha
        """ # nopep8
        return self._cards[4].get_value("calpha")

    @calpha.setter
    def calpha(self, value: float) -> None:
        """Set the calpha property."""
        self._cards[4].set_value("calpha", value)

    @property
    def ckappa(self) -> typing.Optional[float]:
        """Get or set the coefficient for internal stress kappa
        """ # nopep8
        return self._cards[4].get_value("ckappa")

    @ckappa.setter
    def ckappa(self, value: float) -> None:
        """Set the ckappa property."""
        self._cards[4].set_value("ckappa", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the parameter for flow rule exponent n .
        """ # nopep8
        return self._cards[4].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[4].set_value("c1", value)

    @property
    def c2nd(self) -> typing.Optional[float]:
        """Get or set the parameter for transition rate f
        """ # nopep8
        return self._cards[5].get_value("c2nd")

    @c2nd.setter
    def c2nd(self, value: float) -> None:
        """Set the c2nd property."""
        self._cards[5].set_value("c2nd", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the parameter for alpha dynamic recovery rd
        """ # nopep8
        return self._cards[5].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[5].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the parameter for alpha hardening h
        """ # nopep8
        return self._cards[5].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[5].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the parameter for kappa dynamic recovery Rd
        """ # nopep8
        return self._cards[5].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[5].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the parameter for kappa hardening H
        """ # nopep8
        return self._cards[5].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        """Set the c6 property."""
        self._cards[5].set_value("c6", value)

    @property
    def c7nd(self) -> typing.Optional[float]:
        """Get or set the parameter kappa static recovery Rs
        """ # nopep8
        return self._cards[5].get_value("c7nd")

    @c7nd.setter
    def c7nd(self, value: float) -> None:
        """Set the c7nd property."""
        self._cards[5].set_value("c7nd", value)

    @property
    def c8nd(self) -> typing.Optional[float]:
        """Get or set the parameter for yield
        """ # nopep8
        return self._cards[5].get_value("c8nd")

    @c8nd.setter
    def c8nd(self, value: float) -> None:
        """Set the c8nd property."""
        self._cards[5].set_value("c8nd", value)

    @property
    def c9nd(self) -> typing.Optional[float]:
        """Get or set the parameter for temperature dependence of flow rule exponent n .
        """ # nopep8
        return self._cards[5].get_value("c9nd")

    @c9nd.setter
    def c9nd(self, value: float) -> None:
        """Set the c9nd property."""
        self._cards[5].set_value("c9nd", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the parameter for static recovery (set=1)
        """ # nopep8
        return self._cards[6].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        """Set the c10 property."""
        self._cards[6].set_value("c10", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the plastic anisotropy parameter
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the plastic anisotropy parameter
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the plastic anisotropy parameter
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[6].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the plastic anisotropy parameter.
        """ # nopep8
        return self._cards[6].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        """Set the a4 property."""
        self._cards[6].set_value("a4", value)

    @property
    def a_xx(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component
        """ # nopep8
        return self._cards[6].get_value("a_xx")

    @a_xx.setter
    def a_xx(self, value: float) -> None:
        """Set the a_xx property."""
        self._cards[6].set_value("a_xx", value)

    @property
    def a_yy(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component
        """ # nopep8
        return self._cards[6].get_value("a_yy")

    @a_yy.setter
    def a_yy(self, value: float) -> None:
        """Set the a_yy property."""
        self._cards[6].set_value("a_yy", value)

    @property
    def a_zz(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component.
        """ # nopep8
        return self._cards[6].get_value("a_zz")

    @a_zz.setter
    def a_zz(self, value: float) -> None:
        """Set the a_zz property."""
        self._cards[6].set_value("a_zz", value)

    @property
    def a_xy(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component
        """ # nopep8
        return self._cards[7].get_value("a_xy")

    @a_xy.setter
    def a_xy(self, value: float) -> None:
        """Set the a_xy property."""
        self._cards[7].set_value("a_xy", value)

    @property
    def a_yz(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component
        """ # nopep8
        return self._cards[7].get_value("a_yz")

    @a_yz.setter
    def a_yz(self, value: float) -> None:
        """Set the a_yz property."""
        self._cards[7].set_value("a_yz", value)

    @property
    def a_xz(self) -> typing.Optional[float]:
        """Get or set the initial structure tensor component
        """ # nopep8
        return self._cards[7].get_value("a_xz")

    @a_xz.setter
    def a_xz(self, value: float) -> None:
        """Set the a_xz property."""
        self._cards[7].set_value("a_xz", value)

    @property
    def alphxx(self) -> typing.Optional[float]:
        """Get or set the initial backstress component
        """ # nopep8
        return self._cards[7].get_value("alphxx")

    @alphxx.setter
    def alphxx(self, value: float) -> None:
        """Set the alphxx property."""
        self._cards[7].set_value("alphxx", value)

    @property
    def alphyy(self) -> typing.Optional[float]:
        """Get or set the initial backstress component.
        """ # nopep8
        return self._cards[7].get_value("alphyy")

    @alphyy.setter
    def alphyy(self, value: float) -> None:
        """Set the alphyy property."""
        self._cards[7].set_value("alphyy", value)

    @property
    def alphzz(self) -> typing.Optional[float]:
        """Get or set the initial backstress component
        """ # nopep8
        return self._cards[7].get_value("alphzz")

    @alphzz.setter
    def alphzz(self, value: float) -> None:
        """Set the alphzz property."""
        self._cards[7].set_value("alphzz", value)

    @property
    def alphxy(self) -> typing.Optional[float]:
        """Get or set the initial backstress component
        """ # nopep8
        return self._cards[7].get_value("alphxy")

    @alphxy.setter
    def alphxy(self, value: float) -> None:
        """Set the alphxy property."""
        self._cards[7].set_value("alphxy", value)

    @property
    def alphyz(self) -> typing.Optional[float]:
        """Get or set the initial backstress component.
        """ # nopep8
        return self._cards[7].get_value("alphyz")

    @alphyz.setter
    def alphyz(self, value: float) -> None:
        """Set the alphyz property."""
        self._cards[7].set_value("alphyz", value)

    @property
    def alphxz(self) -> typing.Optional[float]:
        """Get or set the initial backstress component
        """ # nopep8
        return self._cards[8].get_value("alphxz")

    @alphxz.setter
    def alphxz(self, value: float) -> None:
        """Set the alphxz property."""
        self._cards[8].set_value("alphxz", value)

    @property
    def dkappa(self) -> typing.Optional[float]:
        """Get or set the initial isotropic internal stress
        """ # nopep8
        return self._cards[8].get_value("dkappa")

    @dkappa.setter
    def dkappa(self, value: float) -> None:
        """Set the dkappa property."""
        self._cards[8].set_value("dkappa", value)

    @property
    def phi0(self) -> typing.Optional[float]:
        """Get or set the initial isotropic porosity
        """ # nopep8
        return self._cards[8].get_value("phi0")

    @phi0.setter
    def phi0(self, value: float) -> None:
        """Set the phi0 property."""
        self._cards[8].set_value("phi0", value)

    @property
    def phicr(self) -> typing.Optional[float]:
        """Get or set the Critical cutoff porosity
        """ # nopep8
        return self._cards[8].get_value("phicr")

    @phicr.setter
    def phicr(self, value: float) -> None:
        """Set the phicr property."""
        self._cards[8].set_value("phicr", value)

    @property
    def dlbdag(self) -> typing.Optional[float]:
        """Get or set the slip system geometry parameter
        """ # nopep8
        return self._cards[8].get_value("dlbdag")

    @dlbdag.setter
    def dlbdag(self, value: float) -> None:
        """Set the dlbdag property."""
        self._cards[8].set_value("dlbdag", value)

    @property
    def factor(self) -> typing.Optional[float]:
        """Get or set the fraction of plastic work converted to heat, adiabatic.
        """ # nopep8
        return self._cards[8].get_value("factor")

    @factor.setter
    def factor(self, value: float) -> None:
        """Set the factor property."""
        self._cards[8].set_value("factor", value)

    @property
    def rswtch(self) -> typing.Optional[float]:
        """Get or set the Rate sensitivity switch
        """ # nopep8
        return self._cards[8].get_value("rswtch")

    @rswtch.setter
    def rswtch(self, value: float) -> None:
        """Set the rswtch property."""
        self._cards[8].set_value("rswtch", value)

    @property
    def dmgopt(self) -> typing.Optional[float]:
        """Get or set the damage model option parameter
        1.0	pressure independent Cocks/Ashby 1980
        2.0	pressure dependent Cocks/Ashby 1980
        3.0	pressure dependent Cocks 1989
        """ # nopep8
        return self._cards[8].get_value("dmgopt")

    @dmgopt.setter
    def dmgopt(self, value: float) -> None:
        """Set the dmgopt property."""
        self._cards[8].set_value("dmgopt", value)

    @property
    def delaso(self) -> typing.Optional[float]:
        """Get or set the Temperature option:
        EQ.0.0:	Driven externally
        EQ.1.0 : Adiabatic
        """ # nopep8
        return self._cards[9].get_value("delaso")

    @delaso.setter
    def delaso(self, value: float) -> None:
        """Set the delaso property."""
        self._cards[9].set_value("delaso", value)

    @property
    def dimplo(self) -> typing.Optional[float]:
        """Get or set the implementation option flag
        1.0	combined viscous drag and thermally activated dislocation motion
        2.0	separate viscous drag and thermally activated dislocation motion.
        """ # nopep8
        return self._cards[9].get_value("dimplo")

    @dimplo.setter
    def dimplo(self, value: float) -> None:
        """Set the dimplo property."""
        self._cards[9].set_value("dimplo", value)

    @property
    def atol(self) -> typing.Optional[float]:
        """Get or set the absolute error tolerance for local Newton iteration
        """ # nopep8
        return self._cards[9].get_value("atol")

    @atol.setter
    def atol(self, value: float) -> None:
        """Set the atol property."""
        self._cards[9].set_value("atol", value)

    @property
    def rtol(self) -> typing.Optional[float]:
        """Get or set the relative error tolerance for local Newton iteration
        """ # nopep8
        return self._cards[9].get_value("rtol")

    @rtol.setter
    def rtol(self, value: float) -> None:
        """Set the rtol property."""
        self._cards[9].set_value("rtol", value)

    @property
    def dniter(self) -> typing.Optional[float]:
        """Get or set the maximum number of iterations for local Newton iteration
        """ # nopep8
        return self._cards[9].get_value("dniter")

    @dniter.setter
    def dniter(self, value: float) -> None:
        """Set the dniter property."""
        self._cards[9].set_value("dniter", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[11].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[11].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

