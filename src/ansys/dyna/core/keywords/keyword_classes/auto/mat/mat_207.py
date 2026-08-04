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

"""Module providing the Mat207 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT207_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("go", float, 20, 10, None),
    FieldSchema("k0nu", float, 30, 10, None),
    FieldSchema("pref", float, 40, 10, None),
    FieldSchema("rhoc", float, 50, 10, 0.37),
    FieldSchema("theta", float, 60, 10, None),
    FieldSchema("x", float, 70, 10, None),
)

_MAT207_CARD1 = (
    FieldSchema("ein", float, 0, 10, None),
    FieldSchema("alphac", float, 10, 10, None),
    FieldSchema("e0", float, 20, 10, None),
    FieldSchema("lambd", float, 30, 10, None),
    FieldSchema("xi", float, 40, 10, 0.7),
    FieldSchema("nb", float, 50, 10, None),
    FieldSchema("h0", float, 60, 10, None),
    FieldSchema("ch", float, 70, 10, None),
)

_MAT207_CARD2 = (
    FieldSchema("p0", float, 0, 10, None),
    FieldSchema("cc", float, 10, 10, 0.778),
    FieldSchema("nd", float, 20, 10, None),
    FieldSchema("a0", float, 30, 10, None),
)

_MAT207_CARD3 = (
    FieldSchema("aniso", float, 0, 10, None),
    FieldSchema("kh", float, 10, 10, None),
    FieldSchema("zmax", float, 20, 10, None),
    FieldSchema("cz", float, 30, 10, None),
)

_MAT207_CARD4 = (
    FieldSchema("patm", float, 0, 10, None),
    FieldSchema("m", float, 10, 10, 0.05),
    FieldSchema("n", float, 20, 10, 20.0),
    FieldSchema("v", float, 30, 10, 1000.0),
)

_MAT207_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat207(KeywordBase):
    """DYNA MAT_207 keyword"""

    keyword = "MAT"
    subkeyword = "207"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat207 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT207_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT207_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT207_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT207_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT207_CARD4,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat207._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT207_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def go(self) -> typing.Optional[float]:
        """Get or set the Shear modulus-related dimensionless term G_0 See Remark 6
        """ # nopep8
        return self._cards[0].get_value("go")

    @go.setter
    def go(self, value: float) -> None:
        """Set the go property."""
        self._cards[0].set_value("go", value)

    @property
    def k0nu(self) -> typing.Optional[float]:
        """Get or set the Elastic constant (see Remark 6):
        GT.0.0: Bulk modulus term K_0
        LT.0.0: Absolute value is Poissons ratio, .
        """ # nopep8
        return self._cards[0].get_value("k0nu")

    @k0nu.setter
    def k0nu(self, value: float) -> None:
        """Set the k0nu property."""
        self._cards[0].set_value("k0nu", value)

    @property
    def pref(self) -> typing.Optional[float]:
        """Get or set the Reference pressure in Limiting Compression Curve, associated with unity void ratio. See Remark 10
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        """Set the pref property."""
        self._cards[0].set_value("pref", value)

    @property
    def rhoc(self) -> float:
        """Get or set the Exponent in Limiting Compression Curve, _c. See Remark 10.
        """ # nopep8
        return self._cards[0].get_value("rhoc")

    @rhoc.setter
    def rhoc(self, value: float) -> None:
        """Set the rhoc property."""
        self._cards[0].set_value("rhoc", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Exponent in transitional compression behavior, . See Remark 10.
        """ # nopep8
        return self._cards[0].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[0].set_value("theta", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Material constant X. See Remark 10.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def ein(self) -> typing.Optional[float]:
        """Get or set the Initial void ratio
        """ # nopep8
        return self._cards[1].get_value("ein")

    @ein.setter
    def ein(self, value: float) -> None:
        """Set the ein property."""
        self._cards[1].set_value("ein", value)

    @property
    def alphac(self) -> typing.Optional[float]:
        """Get or set the Critical surface angle in q-p space, _c**c. See Remark 8.
        """ # nopep8
        return self._cards[1].get_value("alphac")

    @alphac.setter
    def alphac(self, value: float) -> None:
        """Set the alphac property."""
        self._cards[1].set_value("alphac", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Material constant in Critical State Line, e_0. See Remark 8.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def lambd(self) -> typing.Optional[float]:
        """Get or set the Material constant in Critical State Line, . See Remark 8.
        """ # nopep8
        return self._cards[1].get_value("lambd")

    @lambd.setter
    def lambd(self, value: float) -> None:
        """Set the lambd property."""
        self._cards[1].set_value("lambd", value)

    @property
    def xi(self) -> float:
        """Get or set the Material constant in Critical State Line, . See Remark 8.
        """ # nopep8
        return self._cards[1].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        """Set the xi property."""
        self._cards[1].set_value("xi", value)

    @property
    def nb(self) -> typing.Optional[float]:
        """Get or set the Bounding surface parameter, n**b. See Remark 8.
        """ # nopep8
        return self._cards[1].get_value("nb")

    @nb.setter
    def nb(self, value: float) -> None:
        """Set the nb property."""
        self._cards[1].set_value("nb", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter, h_0. See Remark 7
        """ # nopep8
        return self._cards[1].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        """Set the h0 property."""
        self._cards[1].set_value("h0", value)

    @property
    def ch(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter, c_h. See Remark 7
        """ # nopep8
        return self._cards[1].get_value("ch")

    @ch.setter
    def ch(self, value: float) -> None:
        """Set the ch property."""
        self._cards[1].set_value("ch", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Initial value of yield surface parameter, p_0. See Remark 7
        """ # nopep8
        return self._cards[2].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[2].set_value("p0", value)

    @property
    def cc(self) -> float:
        """Get or set the Ratio of critical surface angle in extension to critical surface angle in compression, c. See Remark 8
        """ # nopep8
        return self._cards[2].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        """Set the cc property."""
        self._cards[2].set_value("cc", value)

    @property
    def nd(self) -> typing.Optional[float]:
        """Get or set the Dilatancy surface parameter, n_d. See Remark 8.
        """ # nopep8
        return self._cards[2].get_value("nd")

    @nd.setter
    def nd(self, value: float) -> None:
        """Set the nd property."""
        self._cards[2].set_value("nd", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Dilatancy parameter, A_0
        """ # nopep8
        return self._cards[2].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        """Set the a0 property."""
        self._cards[2].set_value("a0", value)

    @property
    def aniso(self) -> typing.Optional[float]:
        """Get or set the Inherent fabric anisotropy measure. See Remark 12.
        """ # nopep8
        return self._cards[3].get_value("aniso")

    @aniso.setter
    def aniso(self, value: float) -> None:
        """Set the aniso property."""
        self._cards[3].set_value("aniso", value)

    @property
    def kh(self) -> typing.Optional[float]:
        """Get or set the Material constant, k_h, in dependence of hardening parameters on inherent fabric anisotropy. See Remark 12
        """ # nopep8
        return self._cards[3].get_value("kh")

    @kh.setter
    def kh(self, value: float) -> None:
        """Set the kh property."""
        self._cards[3].set_value("kh", value)

    @property
    def zmax(self) -> typing.Optional[float]:
        """Get or set the Material constant for fabric change effect, z_max . See Remark 11
        """ # nopep8
        return self._cards[3].get_value("zmax")

    @zmax.setter
    def zmax(self, value: float) -> None:
        """Set the zmax property."""
        self._cards[3].set_value("zmax", value)

    @property
    def cz(self) -> typing.Optional[float]:
        """Get or set the Material constant for fabric change effect, c_z. See Remark 11.
        """ # nopep8
        return self._cards[3].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        """Set the cz property."""
        self._cards[3].set_value("cz", value)

    @property
    def patm(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure, p_atm
        """ # nopep8
        return self._cards[4].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[4].set_value("patm", value)

    @property
    def m(self) -> float:
        """Get or set the Yield surface constant, m. See Remark 7.
        """ # nopep8
        return self._cards[4].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[4].set_value("m", value)

    @property
    def n(self) -> float:
        """Get or set the Yield surface constant, n. See Remark 7
        """ # nopep8
        return self._cards[4].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[4].set_value("n", value)

    @property
    def v(self) -> float:
        """Get or set the Flow rule constant, V
        """ # nopep8
        return self._cards[4].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        """Set the v property."""
        self._cards[4].set_value("v", value)

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

