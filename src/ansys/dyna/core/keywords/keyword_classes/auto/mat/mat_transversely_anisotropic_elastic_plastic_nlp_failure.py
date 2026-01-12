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

"""Module providing the MatTransverselyAnisotropicElasticPlasticNlpFailure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("etan", float, 50, 10, None),
    FieldSchema("r", float, 60, 10, None),
    FieldSchema("hlcid", int, 70, 10, 0),
)

_MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_CARD1 = (
    FieldSchema("idscale", int, 0, 10, None),
    FieldSchema("ea", float, 10, 10, None),
    FieldSchema("coe", float, 20, 10, None),
    FieldSchema("icfld", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("strainlt", float, 50, 10, None),
)

_MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatTransverselyAnisotropicElasticPlasticNlpFailure(KeywordBase):
    """DYNA MAT_TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC_NLP_FAILURE keyword"""

    keyword = "MAT"
    subkeyword = "TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC_NLP_FAILURE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatTransverselyAnisotropicElasticPlasticNlpFailure class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatTransverselyAnisotropicElasticPlasticNlpFailure.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATTRANSVERSELYANISOTROPICELASTICPLASTICNLPFAILURE_OPTION0_CARD0,
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
        """Get or set the Young's modulus.
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[0].set_value("etan", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Anisotropic hardening parameter.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def hlcid(self) -> int:
        """Get or set the Load curve ID defining effective yield stress versus effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        """Set the hlcid property."""
        self._cards[0].set_value("hlcid", value)

    @property
    def idscale(self) -> typing.Optional[int]:
        """Get or set the load curve ID defining the scale factor for the Young's modulus change with respect to effective strain (if EA and COE are defined), this curve is not necessary).
        """ # nopep8
        return self._cards[1].get_value("idscale")

    @idscale.setter
    def idscale(self, value: int) -> None:
        """Set the idscale property."""
        self._cards[1].set_value("idscale", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
        """ # nopep8
        return self._cards[1].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[1].set_value("ea", value)

    @property
    def coe(self) -> typing.Optional[float]:
        """Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
        """ # nopep8
        return self._cards[1].get_value("coe")

    @coe.setter
    def coe(self, value: float) -> None:
        """Set the coe property."""
        self._cards[1].set_value("coe", value)

    @property
    def icfld(self) -> typing.Optional[int]:
        """Get or set the define curve id.
        """ # nopep8
        return self._cards[1].get_value("icfld")

    @icfld.setter
    def icfld(self, value: int) -> None:
        """Set the icfld property."""
        self._cards[1].set_value("icfld", value)

    @property
    def strainlt(self) -> typing.Optional[float]:
        """Get or set the Critical strain value at which strain averaging is activated.
        """ # nopep8
        return self._cards[1].get_value("strainlt")

    @strainlt.setter
    def strainlt(self, value: float) -> None:
        """Set the strainlt property."""
        self._cards[1].set_value("strainlt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

