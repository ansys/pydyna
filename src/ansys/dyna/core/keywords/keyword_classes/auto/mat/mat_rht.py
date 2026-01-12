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

"""Module providing the MatRht class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATRHT_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("shear", float, 20, 10, None),
    FieldSchema("onempa", float, 30, 10, 0.0),
    FieldSchema("epsf", float, 40, 10, 2.0),
    FieldSchema("b0", float, 50, 10, None),
    FieldSchema("b1", float, 60, 10, None),
    FieldSchema("t1", float, 70, 10, None),
)

_MATRHT_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("n", float, 10, 10, None),
    FieldSchema("fc", float, 20, 10, None),
    FieldSchema("fs*", float, 30, 10, None),
    FieldSchema("ft*", float, 40, 10, None),
    FieldSchema("q0", float, 50, 10, None),
    FieldSchema("b", float, 60, 10, None),
    FieldSchema("t2", float, 70, 10, None),
)

_MATRHT_CARD2 = (
    FieldSchema("e0c", float, 0, 10, None),
    FieldSchema("e0t", float, 10, 10, None),
    FieldSchema("ec", float, 20, 10, None),
    FieldSchema("et", float, 30, 10, None),
    FieldSchema("betac", float, 40, 10, None),
    FieldSchema("betat", float, 50, 10, None),
    FieldSchema("ptf", float, 60, 10, 0.001),
)

_MATRHT_CARD3 = (
    FieldSchema("gc*", float, 0, 10, None),
    FieldSchema("gt*", float, 10, 10, None),
    FieldSchema("xi", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("epm", float, 50, 10, None),
    FieldSchema("af", float, 60, 10, None),
    FieldSchema("nf", float, 70, 10, None),
)

_MATRHT_CARD4 = (
    FieldSchema("gamma", float, 0, 10, None),
    FieldSchema("a1", float, 10, 10, None),
    FieldSchema("a2", float, 20, 10, None),
    FieldSchema("a3", float, 30, 10, None),
    FieldSchema("pel", float, 40, 10, None),
    FieldSchema("pco", float, 50, 10, None),
    FieldSchema("np", float, 60, 10, None),
    FieldSchema("alpha", float, 70, 10, None),
)

_MATRHT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatRht(KeywordBase):
    """DYNA MAT_RHT keyword"""

    keyword = "MAT"
    subkeyword = "RHT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatRht class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATRHT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRHT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRHT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRHT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRHT_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatRht.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATRHT_OPTION0_CARD0,
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
    def shear(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("shear")

    @shear.setter
    def shear(self, value: float) -> None:
        """Set the shear property."""
        self._cards[0].set_value("shear", value)

    @property
    def onempa(self) -> float:
        """Get or set the Unit conversion factor defining 1 Mpa in the pressure units used. It can also be used for automatic generation of material parameters for a given
        compressive strength. (See remarks)
        EQ.0: Defaults to 1.0
        EQ.-1: Parameters generated in m, s and kg (Pa)
        EQ.-2: Parameters generated in mm, s and tonne (MPa)
        EQ.-3: Parameters generated in mm, ms and kg (GPa)
        EQ.-4: Parameters generated in in, s and dozens of slugs (psi).
        """ # nopep8
        return self._cards[0].get_value("onempa")

    @onempa.setter
    def onempa(self, value: float) -> None:
        """Set the onempa property."""
        if value not in [0, -1, -2, -3, -4, None]:
            raise Exception("""onempa must be `None` or one of {0,-1,-2,-3,-4}.""")
        self._cards[0].set_value("onempa", value)

    @property
    def epsf(self) -> float:
        """Get or set the Eroding plastic strain.
        """ # nopep8
        return self._cards[0].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        """Set the epsf property."""
        self._cards[0].set_value("epsf", value)

    @property
    def b0(self) -> typing.Optional[float]:
        """Get or set the Parameter for polynomial EOS.
        """ # nopep8
        return self._cards[0].get_value("b0")

    @b0.setter
    def b0(self, value: float) -> None:
        """Set the b0 property."""
        self._cards[0].set_value("b0", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Parameter for polynomial EOS.
        """ # nopep8
        return self._cards[0].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[0].set_value("b1", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Parameter for polynomial EOS.
        """ # nopep8
        return self._cards[0].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[0].set_value("t1", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Failure surface parameter A.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Failure surface parameter N.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Compressive strength.
        """ # nopep8
        return self._cards[1].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[1].set_value("fc", value)

    @property
    def fs_(self) -> typing.Optional[float]:
        """Get or set the Relative shear strength.
        """ # nopep8
        return self._cards[1].get_value("fs*")

    @fs_.setter
    def fs_(self, value: float) -> None:
        """Set the fs_ property."""
        self._cards[1].set_value("fs*", value)

    @property
    def ft_(self) -> typing.Optional[float]:
        """Get or set the Relative tensile strength.
        """ # nopep8
        return self._cards[1].get_value("ft*")

    @ft_.setter
    def ft_(self, value: float) -> None:
        """Set the ft_ property."""
        self._cards[1].set_value("ft*", value)

    @property
    def q0(self) -> typing.Optional[float]:
        """Get or set the Lode angle dependence factor.
        """ # nopep8
        return self._cards[1].get_value("q0")

    @q0.setter
    def q0(self, value: float) -> None:
        """Set the q0 property."""
        self._cards[1].set_value("q0", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Lode angle dependence factor.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Parameter for polynomial EOS.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[1].set_value("t2", value)

    @property
    def e0c(self) -> typing.Optional[float]:
        """Get or set the Reference compressive strain rate.
        """ # nopep8
        return self._cards[2].get_value("e0c")

    @e0c.setter
    def e0c(self, value: float) -> None:
        """Set the e0c property."""
        self._cards[2].set_value("e0c", value)

    @property
    def e0t(self) -> typing.Optional[float]:
        """Get or set the Reference tensile strain rate.
        """ # nopep8
        return self._cards[2].get_value("e0t")

    @e0t.setter
    def e0t(self, value: float) -> None:
        """Set the e0t property."""
        self._cards[2].set_value("e0t", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Break compressive strain rate.
        """ # nopep8
        return self._cards[2].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[2].set_value("ec", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the Break tensile strain rate.
        """ # nopep8
        return self._cards[2].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        """Set the et property."""
        self._cards[2].set_value("et", value)

    @property
    def betac(self) -> typing.Optional[float]:
        """Get or set the Compressive strain rate dependence exponent (optional).
        """ # nopep8
        return self._cards[2].get_value("betac")

    @betac.setter
    def betac(self, value: float) -> None:
        """Set the betac property."""
        self._cards[2].set_value("betac", value)

    @property
    def betat(self) -> typing.Optional[float]:
        """Get or set the Tensile strain rate dependence exponent (optional).
        """ # nopep8
        return self._cards[2].get_value("betat")

    @betat.setter
    def betat(self, value: float) -> None:
        """Set the betat property."""
        self._cards[2].set_value("betat", value)

    @property
    def ptf(self) -> float:
        """Get or set the Pressure influence on plastic flow in tension.
        """ # nopep8
        return self._cards[2].get_value("ptf")

    @ptf.setter
    def ptf(self, value: float) -> None:
        """Set the ptf property."""
        self._cards[2].set_value("ptf", value)

    @property
    def gc_(self) -> typing.Optional[float]:
        """Get or set the Compressive yield surface parameter.
        """ # nopep8
        return self._cards[3].get_value("gc*")

    @gc_.setter
    def gc_(self, value: float) -> None:
        """Set the gc_ property."""
        self._cards[3].set_value("gc*", value)

    @property
    def gt_(self) -> typing.Optional[float]:
        """Get or set the Tensile yield surface parameter.
        """ # nopep8
        return self._cards[3].get_value("gt*")

    @gt_.setter
    def gt_(self, value: float) -> None:
        """Set the gt_ property."""
        self._cards[3].set_value("gt*", value)

    @property
    def xi(self) -> typing.Optional[float]:
        """Get or set the Shear modulus reduction factor.
        """ # nopep8
        return self._cards[3].get_value("xi")

    @xi.setter
    def xi(self, value: float) -> None:
        """Set the xi property."""
        self._cards[3].set_value("xi", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Damage parameter.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Damage parameter.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def epm(self) -> typing.Optional[float]:
        """Get or set the Minimum damaged residual strain.
        """ # nopep8
        return self._cards[3].get_value("epm")

    @epm.setter
    def epm(self, value: float) -> None:
        """Set the epm property."""
        self._cards[3].set_value("epm", value)

    @property
    def af(self) -> typing.Optional[float]:
        """Get or set the Residual surface parameter.
        """ # nopep8
        return self._cards[3].get_value("af")

    @af.setter
    def af(self, value: float) -> None:
        """Set the af property."""
        self._cards[3].set_value("af", value)

    @property
    def nf(self) -> typing.Optional[float]:
        """Get or set the Residual surface parameter.
        """ # nopep8
        return self._cards[3].get_value("nf")

    @nf.setter
    def nf(self, value: float) -> None:
        """Set the nf property."""
        self._cards[3].set_value("nf", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Gruneisen gamma.
        """ # nopep8
        return self._cards[4].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[4].set_value("gamma", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Hugoniot polynomial coefficient.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

    @property
    def pel(self) -> typing.Optional[float]:
        """Get or set the Crush pressure.
        """ # nopep8
        return self._cards[4].get_value("pel")

    @pel.setter
    def pel(self, value: float) -> None:
        """Set the pel property."""
        self._cards[4].set_value("pel", value)

    @property
    def pco(self) -> typing.Optional[float]:
        """Get or set the Compaction pressure.
        """ # nopep8
        return self._cards[4].get_value("pco")

    @pco.setter
    def pco(self, value: float) -> None:
        """Set the pco property."""
        self._cards[4].set_value("pco", value)

    @property
    def np(self) -> typing.Optional[float]:
        """Get or set the Porosity exponent.
        """ # nopep8
        return self._cards[4].get_value("np")

    @np.setter
    def np(self, value: float) -> None:
        """Set the np property."""
        self._cards[4].set_value("np", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Initial porosity.
        """ # nopep8
        return self._cards[4].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[4].set_value("alpha", value)

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

