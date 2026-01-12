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

"""Module providing the MatTnmPolymer class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATTNMPOLYMER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("roflg", int, 20, 10, None),
)

_MATTNMPOLYMER_CARD1 = (
    FieldSchema("mua", float, 0, 10, None),
    FieldSchema("thetah", float, 10, 10, None),
    FieldSchema("lambl", float, 20, 10, None),
    FieldSchema("kappa", float, 30, 10, None),
    FieldSchema("tauha", float, 40, 10, None),
    FieldSchema("a", float, 50, 10, None),
    FieldSchema("ma", float, 60, 10, None),
    FieldSchema("n", float, 70, 10, None),
)

_MATTNMPOLYMER_CARD2 = (
    FieldSchema("mubi", float, 0, 10, None),
    FieldSchema("mubf", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("tauhb", float, 30, 10, None),
    FieldSchema("mb", float, 40, 10, None),
    FieldSchema("muc", float, 50, 10, None),
    FieldSchema("q", float, 60, 10, None),
    FieldSchema("alpha", float, 70, 10, None),
)

_MATTNMPOLYMER_CARD3 = (
    FieldSchema("theata0", float, 0, 10, None),
    FieldSchema("ibulk", float, 10, 10, None),
    FieldSchema("ig", float, 20, 10, None),
    FieldSchema("tsstif", float, 30, 10, None),
    FieldSchema("gamma0", float, 40, 10, None),
)

class MatTnmPolymer(KeywordBase):
    """DYNA MAT_TNM_POLYMER keyword"""

    keyword = "MAT"
    subkeyword = "TNM_POLYMER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatTnmPolymer class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATTNMPOLYMER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTNMPOLYMER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTNMPOLYMER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTNMPOLYMER_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatTnmPolymer.option_specs[0],
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
        """Get or set the Material identification. A unique number or label be specified (see *PART).
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
    def roflg(self) -> typing.Optional[int]:
        """Get or set the Flag for whether density is specified per unit area or volume:
        EQ.0:	Density is per unit volume(default).
        EQ.1 : Density is per unit area for controlling the mass of cohesive elements with an initial volume of zero
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        """Set the roflg property."""
        self._cards[0].set_value("roflg", value)

    @property
    def mua(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for network A
        """ # nopep8
        return self._cards[1].get_value("mua")

    @mua.setter
    def mua(self, value: float) -> None:
        """Set the mua property."""
        self._cards[1].set_value("mua", value)

    @property
    def thetah(self) -> typing.Optional[float]:
        """Get or set the Temperature factor
        """ # nopep8
        return self._cards[1].get_value("thetah")

    @thetah.setter
    def thetah(self, value: float) -> None:
        """Set the thetah property."""
        self._cards[1].set_value("thetah", value)

    @property
    def lambl(self) -> typing.Optional[float]:
        """Get or set the Locking stretch
        """ # nopep8
        return self._cards[1].get_value("lambl")

    @lambl.setter
    def lambl(self, value: float) -> None:
        """Set the lambl property."""
        self._cards[1].set_value("lambl", value)

    @property
    def kappa(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus
        """ # nopep8
        return self._cards[1].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        """Set the kappa property."""
        self._cards[1].set_value("kappa", value)

    @property
    def tauha(self) -> typing.Optional[float]:
        """Get or set the Flow resistance of network A
        """ # nopep8
        return self._cards[1].get_value("tauha")

    @tauha.setter
    def tauha(self, value: float) -> None:
        """Set the tauha property."""
        self._cards[1].set_value("tauha", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Pressure dependence of flow
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def ma(self) -> typing.Optional[float]:
        """Get or set the Stress exponential of network A
        """ # nopep8
        return self._cards[1].get_value("ma")

    @ma.setter
    def ma(self, value: float) -> None:
        """Set the ma property."""
        self._cards[1].set_value("ma", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Temperature exponential
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def mubi(self) -> typing.Optional[float]:
        """Get or set the Initial shear modulus for network B
        """ # nopep8
        return self._cards[2].get_value("mubi")

    @mubi.setter
    def mubi(self, value: float) -> None:
        """Set the mubi property."""
        self._cards[2].set_value("mubi", value)

    @property
    def mubf(self) -> typing.Optional[float]:
        """Get or set the Final shear modulus for network B
        """ # nopep8
        return self._cards[2].get_value("mubf")

    @mubf.setter
    def mubf(self, value: float) -> None:
        """Set the mubf property."""
        self._cards[2].set_value("mubf", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Evolution rate of shear modulus for network B
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

    @property
    def tauhb(self) -> typing.Optional[float]:
        """Get or set the Flow resistance of network B
        """ # nopep8
        return self._cards[2].get_value("tauhb")

    @tauhb.setter
    def tauhb(self, value: float) -> None:
        """Set the tauhb property."""
        self._cards[2].set_value("tauhb", value)

    @property
    def mb(self) -> typing.Optional[float]:
        """Get or set the Stress exponential of network B
        """ # nopep8
        return self._cards[2].get_value("mb")

    @mb.setter
    def mb(self, value: float) -> None:
        """Set the mb property."""
        self._cards[2].set_value("mb", value)

    @property
    def muc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for network C
        """ # nopep8
        return self._cards[2].get_value("muc")

    @muc.setter
    def muc(self, value: float) -> None:
        """Set the muc property."""
        self._cards[2].set_value("muc", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Relative contribution of I2 on network C
        """ # nopep8
        return self._cards[2].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[2].set_value("q", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[2].set_value("alpha", value)

    @property
    def theata0(self) -> typing.Optional[float]:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[3].get_value("theata0")

    @theata0.setter
    def theata0(self, value: float) -> None:
        """Set the theata0 property."""
        self._cards[3].set_value("theata0", value)

    @property
    def ibulk(self) -> typing.Optional[float]:
        """Get or set the Internal bulk modulus
        """ # nopep8
        return self._cards[3].get_value("ibulk")

    @ibulk.setter
    def ibulk(self, value: float) -> None:
        """Set the ibulk property."""
        self._cards[3].set_value("ibulk", value)

    @property
    def ig(self) -> typing.Optional[float]:
        """Get or set the Internal shear modulus
        """ # nopep8
        return self._cards[3].get_value("ig")

    @ig.setter
    def ig(self, value: float) -> None:
        """Set the ig property."""
        self._cards[3].set_value("ig", value)

    @property
    def tsstif(self) -> typing.Optional[float]:
        """Get or set the Transversal stiffness for shells
        """ # nopep8
        return self._cards[3].get_value("tsstif")

    @tsstif.setter
    def tsstif(self, value: float) -> None:
        """Set the tsstif property."""
        self._cards[3].set_value("tsstif", value)

    @property
    def gamma0(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate
        """ # nopep8
        return self._cards[3].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: float) -> None:
        """Set the gamma0 property."""
        self._cards[3].set_value("gamma0", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

