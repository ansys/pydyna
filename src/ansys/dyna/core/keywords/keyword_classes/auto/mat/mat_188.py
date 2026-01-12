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

"""Module providing the Mat188 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT188_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("lcss", int, 60, 10, None),
    FieldSchema("reftem", float, 70, 10, None),
)

_MAT188_CARD1 = (
    FieldSchema("qr1", float, 0, 10, None),
    FieldSchema("cr1", float, 10, 10, None),
    FieldSchema("qr2", float, 20, 10, None),
    FieldSchema("cr2", float, 30, 10, None),
    FieldSchema("qx1", float, 40, 10, None),
    FieldSchema("cx1", float, 50, 10, None),
    FieldSchema("qx2", float, 60, 10, None),
    FieldSchema("cx2", float, 70, 10, None),
)

_MAT188_CARD2 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lce", float, 20, 10, None),
    FieldSchema("lcpr", float, 30, 10, None),
    FieldSchema("lcsigy", float, 40, 10, None),
    FieldSchema("lcqr", float, 50, 10, None),
    FieldSchema("lcqx", float, 60, 10, None),
    FieldSchema("lcalph", float, 70, 10, None),
)

_MAT188_CARD3 = (
    FieldSchema("lcc", float, 0, 10, None),
    FieldSchema("lcp", float, 10, 10, None),
    FieldSchema("lccr", float, 20, 10, None),
    FieldSchema("lccx", float, 30, 10, None),
    FieldSchema("crpa", float, 40, 10, None),
    FieldSchema("crpb", float, 50, 10, None),
    FieldSchema("crpq", float, 60, 10, None),
    FieldSchema("crpm", float, 70, 10, None),
)

_MAT188_CARD4 = (
    FieldSchema("crplaw", float, 0, 10, 0.0),
)

class Mat188(KeywordBase):
    """DYNA MAT_188 keyword"""

    keyword = "MAT"
    subkeyword = "188"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat188 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT188_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT188_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT188_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT188_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT188_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat188.option_specs[0],
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
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. The load curve defines effective stress versus effective plastic strain. The Table ID defines for each temperature value a load curve ID giving the stress versus effective plastic strain for that rate.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[0].set_value("lcss", value)

    @property
    def reftem(self) -> typing.Optional[float]:
        """Get or set the Reference temperature that defines thermal expansion coefficient
        """ # nopep8
        return self._cards[0].get_value("reftem")

    @reftem.setter
    def reftem(self, value: float) -> None:
        """Set the reftem property."""
        self._cards[0].set_value("reftem", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr1
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        """Set the qr1 property."""
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr1
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        """Set the cr1 property."""
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Qr2
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        """Set the qr2 property."""
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Cr2
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        """Set the cr2 property."""
        self._cards[1].set_value("cr2", value)

    @property
    def qx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Qx1
        """ # nopep8
        return self._cards[1].get_value("qx1")

    @qx1.setter
    def qx1(self, value: float) -> None:
        """Set the qx1 property."""
        self._cards[1].set_value("qx1", value)

    @property
    def cx1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Cx1
        """ # nopep8
        return self._cards[1].get_value("cx1")

    @cx1.setter
    def cx1(self, value: float) -> None:
        """Set the cx1 property."""
        self._cards[1].set_value("cx1", value)

    @property
    def qx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Qx2
        """ # nopep8
        return self._cards[1].get_value("qx2")

    @qx2.setter
    def qx2(self, value: float) -> None:
        """Set the qx2 property."""
        self._cards[1].set_value("qx2", value)

    @property
    def cx2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter Cx2
        """ # nopep8
        return self._cards[1].get_value("cx2")

    @cx2.setter
    def cx2(self, value: float) -> None:
        """Set the cx2 property."""
        self._cards[1].set_value("cx2", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter C
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter P
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[2].set_value("p", value)

    @property
    def lce(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling Young's modulus as a function of temperature
        """ # nopep8
        return self._cards[2].get_value("lce")

    @lce.setter
    def lce(self, value: float) -> None:
        """Set the lce property."""
        self._cards[2].set_value("lce", value)

    @property
    def lcpr(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling Poisson's ratio as a function of temperature
        """ # nopep8
        return self._cards[2].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: float) -> None:
        """Set the lcpr property."""
        self._cards[2].set_value("lcpr", value)

    @property
    def lcsigy(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling initial yield stress as a function of temperature
        """ # nopep8
        return self._cards[2].get_value("lcsigy")

    @lcsigy.setter
    def lcsigy(self, value: float) -> None:
        """Set the lcsigy property."""
        self._cards[2].set_value("lcsigy", value)

    @property
    def lcqr(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters QR1 and QR2 or the stress given by the load curve LCSS as a function of temperature.
        """ # nopep8
        return self._cards[2].get_value("lcqr")

    @lcqr.setter
    def lcqr(self, value: float) -> None:
        """Set the lcqr property."""
        self._cards[2].set_value("lcqr", value)

    @property
    def lcqx(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the kinematic hardening parameters QX1 and QX2 as a function of temperature
        """ # nopep8
        return self._cards[2].get_value("lcqx")

    @lcqx.setter
    def lcqx(self, value: float) -> None:
        """Set the lcqx property."""
        self._cards[2].set_value("lcqx", value)

    @property
    def lcalph(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the thermal expansion coefficient as a function of temperature
        """ # nopep8
        return self._cards[2].get_value("lcalph")

    @lcalph.setter
    def lcalph(self, value: float) -> None:
        """Set the lcalph property."""
        self._cards[2].set_value("lcalph", value)

    @property
    def lcc(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the viscous material parameter C as a function of temperature
        """ # nopep8
        return self._cards[3].get_value("lcc")

    @lcc.setter
    def lcc(self, value: float) -> None:
        """Set the lcc property."""
        self._cards[3].set_value("lcc", value)

    @property
    def lcp(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the viscous material parameter P as a function of temperature
        """ # nopep8
        return self._cards[3].get_value("lcp")

    @lcp.setter
    def lcp(self, value: float) -> None:
        """Set the lcp property."""
        self._cards[3].set_value("lcp", value)

    @property
    def lccr(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the isotropic hardening parameters CR1 and CR2 as a function of temperature
        """ # nopep8
        return self._cards[3].get_value("lccr")

    @lccr.setter
    def lccr(self, value: float) -> None:
        """Set the lccr property."""
        self._cards[3].set_value("lccr", value)

    @property
    def lccx(self) -> typing.Optional[float]:
        """Get or set the Load curve for scaling the kinematic hardening parameters CX1 and CX2 as a function of temperature
        """ # nopep8
        return self._cards[3].get_value("lccx")

    @lccx.setter
    def lccx(self, value: float) -> None:
        """Set the lccx property."""
        self._cards[3].set_value("lccx", value)

    @property
    def crpa(self) -> typing.Optional[float]:
        """Get or set the Constant A of Garafalo's hyperbolic sine creep law.
        """ # nopep8
        return self._cards[3].get_value("crpa")

    @crpa.setter
    def crpa(self, value: float) -> None:
        """Set the crpa property."""
        self._cards[3].set_value("crpa", value)

    @property
    def crpb(self) -> typing.Optional[float]:
        """Get or set the Constant B of Garafalo's hyperbolic sine creep law.
        """ # nopep8
        return self._cards[3].get_value("crpb")

    @crpb.setter
    def crpb(self, value: float) -> None:
        """Set the crpb property."""
        self._cards[3].set_value("crpb", value)

    @property
    def crpq(self) -> typing.Optional[float]:
        """Get or set the Constant Q of Garafalo's hyperbolic sine creep law.
        """ # nopep8
        return self._cards[3].get_value("crpq")

    @crpq.setter
    def crpq(self, value: float) -> None:
        """Set the crpq property."""
        self._cards[3].set_value("crpq", value)

    @property
    def crpm(self) -> typing.Optional[float]:
        """Get or set the Constant m of Garafalo's hyperbolic sine creep law.
        """ # nopep8
        return self._cards[3].get_value("crpm")

    @crpm.setter
    def crpm(self, value: float) -> None:
        """Set the crpm property."""
        self._cards[3].set_value("crpm", value)

    @property
    def crplaw(self) -> float:
        """Get or set the Creep law definition:
        EQ.0.0: Garofalo's hyperbolic sine law (default).
        EQ.1.0: Norton'sower law.
        """ # nopep8
        return self._cards[4].get_value("crplaw")

    @crplaw.setter
    def crplaw(self, value: float) -> None:
        """Set the crplaw property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""crplaw must be `None` or one of {0.0,1.0}.""")
        self._cards[4].set_value("crplaw", value)

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

