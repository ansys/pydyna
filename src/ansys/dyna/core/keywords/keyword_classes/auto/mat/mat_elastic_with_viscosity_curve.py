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

"""Module providing the MatElasticWithViscosityCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATELASTICWITHVISCOSITYCURVE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("v0", float, 20, 10, None),
    FieldSchema("a", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("c", float, 50, 10, None),
    FieldSchema("lcid", float, 60, 10, 0.0),
)

_MATELASTICWITHVISCOSITYCURVE_CARD1 = (
    FieldSchema("pr_lc", int, 0, 10, None),
    FieldSchema("ym_lc", int, 10, 10, None),
    FieldSchema("a_lc", int, 20, 10, None),
    FieldSchema("v_lc", int, 30, 10, None),
    FieldSchema("v_log", float, 40, 10, 0.0),
)

class MatElasticWithViscosityCurve(KeywordBase):
    """DYNA MAT_ELASTIC_WITH_VISCOSITY_CURVE keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_WITH_VISCOSITY_CURVE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatElasticWithViscosityCurve class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATELASTICWITHVISCOSITYCURVE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATELASTICWITHVISCOSITYCURVE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatElasticWithViscosityCurve.option_specs[0],
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
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Constant viscosity coefficient. If V0 is defined, don't define A, B, C or the piecewise curve (card 4).
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Viscosity coefficient a. Only, if V0 is not defined.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Viscosity coefficient b. Only, if V0 is not defined.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Viscosity coefficient c. Only, if V0 is not defined.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def lcid(self) -> float:
        """Get or set the Load curve, see *DEFINE_CURVE, defining viscosity versus temperature (optional).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: float) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def pr_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve (see *DEFINE_CURVE) defining Poisson's ratio as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("pr_lc")

    @pr_lc.setter
    def pr_lc(self, value: int) -> None:
        """Set the pr_lc property."""
        self._cards[1].set_value("pr_lc", value)

    @property
    def ym_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve (see *DEFINE_CURVE) defining Young's modulus as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("ym_lc")

    @ym_lc.setter
    def ym_lc(self, value: int) -> None:
        """Set the ym_lc property."""
        self._cards[1].set_value("ym_lc", value)

    @property
    def a_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve (see *DEFINE_CURVE) defining the coefficient of thermal expansion as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("a_lc")

    @a_lc.setter
    def a_lc(self, value: int) -> None:
        """Set the a_lc property."""
        self._cards[1].set_value("a_lc", value)

    @property
    def v_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve (see *DEFINE_CURVE) defining the viscosity as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("v_lc")

    @v_lc.setter
    def v_lc(self, value: int) -> None:
        """Set the v_lc property."""
        self._cards[1].set_value("v_lc", value)

    @property
    def v_log(self) -> float:
        """Get or set the Flag for the form of V_LC. If V_LOG=1.0, the value specified in V_LC is the natural logarithm of the viscosity, ln(V). The value interpolated from the curve is then exponentiated to obtain the viscosity. If V_LOG=0.0, the value is the viscosity. The logarithmic form is useful if the value of the viscosity changes by orders of magnitude over the temperature range of the data.
        """ # nopep8
        return self._cards[1].get_value("v_log")

    @v_log.setter
    def v_log(self, value: float) -> None:
        """Set the v_log property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""v_log must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("v_log", value)

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

