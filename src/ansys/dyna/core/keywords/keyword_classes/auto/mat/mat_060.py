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

"""Module providing the Mat060 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT060_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("v0", float, 20, 10, None),
    FieldSchema("a", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("c", float, 50, 10, None),
    FieldSchema("lcid", float, 60, 10, 0.0),
)

_MAT060_CARD1 = (
    FieldSchema("pr1", float, 0, 10, None),
    FieldSchema("pr2", float, 10, 10, None),
    FieldSchema("pr3", float, 20, 10, None),
    FieldSchema("pr4", float, 30, 10, None),
    FieldSchema("pr5", float, 40, 10, None),
    FieldSchema("pr6", float, 50, 10, None),
    FieldSchema("pr7", float, 60, 10, None),
    FieldSchema("pr8", float, 70, 10, None),
)

_MAT060_CARD2 = (
    FieldSchema("t1", float, 0, 10, None),
    FieldSchema("t2", float, 10, 10, None),
    FieldSchema("t3", float, 20, 10, None),
    FieldSchema("t4", float, 30, 10, None),
    FieldSchema("t5", float, 40, 10, None),
    FieldSchema("t6", float, 50, 10, None),
    FieldSchema("t7", float, 60, 10, None),
    FieldSchema("t8", float, 70, 10, None),
)

_MAT060_CARD3 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("v4", float, 30, 10, None),
    FieldSchema("v5", float, 40, 10, None),
    FieldSchema("v6", float, 50, 10, None),
    FieldSchema("v7", float, 60, 10, None),
    FieldSchema("v8", float, 70, 10, None),
)

_MAT060_CARD4 = (
    FieldSchema("e1", float, 0, 10, None),
    FieldSchema("e2", float, 10, 10, None),
    FieldSchema("e3", float, 20, 10, None),
    FieldSchema("e4", float, 30, 10, None),
    FieldSchema("e5", float, 40, 10, None),
    FieldSchema("e6", float, 50, 10, None),
    FieldSchema("e7", float, 60, 10, None),
    FieldSchema("e8", float, 70, 10, None),
)

_MAT060_CARD5 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
    FieldSchema("alpha4", float, 30, 10, None),
    FieldSchema("alpha5", float, 40, 10, None),
    FieldSchema("alpha6", float, 50, 10, None),
    FieldSchema("alpha7", float, 60, 10, None),
    FieldSchema("alpha8", float, 70, 10, None),
)

_MAT060_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat060(KeywordBase):
    """DYNA MAT_060 keyword"""

    keyword = "MAT"
    subkeyword = "060"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat060 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT060_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT060_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT060_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT060_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT060_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT060_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat060.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT060_OPTION0_CARD0,
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
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T1.
        """ # nopep8
        return self._cards[1].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        """Set the pr1 property."""
        self._cards[1].set_value("pr1", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T2.
        """ # nopep8
        return self._cards[1].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        """Set the pr2 property."""
        self._cards[1].set_value("pr2", value)

    @property
    def pr3(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T3.
        """ # nopep8
        return self._cards[1].get_value("pr3")

    @pr3.setter
    def pr3(self, value: float) -> None:
        """Set the pr3 property."""
        self._cards[1].set_value("pr3", value)

    @property
    def pr4(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T4.
        """ # nopep8
        return self._cards[1].get_value("pr4")

    @pr4.setter
    def pr4(self, value: float) -> None:
        """Set the pr4 property."""
        self._cards[1].set_value("pr4", value)

    @property
    def pr5(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T5.
        """ # nopep8
        return self._cards[1].get_value("pr5")

    @pr5.setter
    def pr5(self, value: float) -> None:
        """Set the pr5 property."""
        self._cards[1].set_value("pr5", value)

    @property
    def pr6(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T6.
        """ # nopep8
        return self._cards[1].get_value("pr6")

    @pr6.setter
    def pr6(self, value: float) -> None:
        """Set the pr6 property."""
        self._cards[1].set_value("pr6", value)

    @property
    def pr7(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T7.
        """ # nopep8
        return self._cards[1].get_value("pr7")

    @pr7.setter
    def pr7(self, value: float) -> None:
        """Set the pr7 property."""
        self._cards[1].set_value("pr7", value)

    @property
    def pr8(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for the temperatures T8.
        """ # nopep8
        return self._cards[1].get_value("pr8")

    @pr8.setter
    def pr8(self, value: float) -> None:
        """Set the pr8 property."""
        self._cards[1].set_value("pr8", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the First temperature, define up to 8 values.
        """ # nopep8
        return self._cards[2].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[2].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Second temperature.
        """ # nopep8
        return self._cards[2].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[2].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Third temperature.
        """ # nopep8
        return self._cards[2].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        """Set the t3 property."""
        self._cards[2].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Fourth temperature.
        """ # nopep8
        return self._cards[2].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        """Set the t4 property."""
        self._cards[2].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Fifth temperature.
        """ # nopep8
        return self._cards[2].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        """Set the t5 property."""
        self._cards[2].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Sixth temperature.
        """ # nopep8
        return self._cards[2].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        """Set the t6 property."""
        self._cards[2].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the Seventh temperature.
        """ # nopep8
        return self._cards[2].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        """Set the t7 property."""
        self._cards[2].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the Eighth temperature.
        """ # nopep8
        return self._cards[2].get_value("t8")

    @t8.setter
    def t8(self, value: float) -> None:
        """Set the t8 property."""
        self._cards[2].set_value("t8", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T1 (define V1 to v8 only, if not varying with temperature).
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T2.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[3].set_value("v3", value)

    @property
    def v4(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T4.
        """ # nopep8
        return self._cards[3].get_value("v4")

    @v4.setter
    def v4(self, value: float) -> None:
        """Set the v4 property."""
        self._cards[3].set_value("v4", value)

    @property
    def v5(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T5.
        """ # nopep8
        return self._cards[3].get_value("v5")

    @v5.setter
    def v5(self, value: float) -> None:
        """Set the v5 property."""
        self._cards[3].set_value("v5", value)

    @property
    def v6(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T6.
        """ # nopep8
        return self._cards[3].get_value("v6")

    @v6.setter
    def v6(self, value: float) -> None:
        """Set the v6 property."""
        self._cards[3].set_value("v6", value)

    @property
    def v7(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T7.
        """ # nopep8
        return self._cards[3].get_value("v7")

    @v7.setter
    def v7(self, value: float) -> None:
        """Set the v7 property."""
        self._cards[3].set_value("v7", value)

    @property
    def v8(self) -> typing.Optional[float]:
        """Get or set the Corresponding viscosity coefficient at temperature T8.
        """ # nopep8
        return self._cards[3].get_value("v8")

    @v8.setter
    def v8(self, value: float) -> None:
        """Set the v8 property."""
        self._cards[3].set_value("v8", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T1 (define E1 to E8 only, if not varying with temperature).
        """ # nopep8
        return self._cards[4].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[4].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T2.
        """ # nopep8
        return self._cards[4].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[4].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T3.
        """ # nopep8
        return self._cards[4].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        """Set the e3 property."""
        self._cards[4].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T4.
        """ # nopep8
        return self._cards[4].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        """Set the e4 property."""
        self._cards[4].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T5.
        """ # nopep8
        return self._cards[4].get_value("e5")

    @e5.setter
    def e5(self, value: float) -> None:
        """Set the e5 property."""
        self._cards[4].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T6.
        """ # nopep8
        return self._cards[4].get_value("e6")

    @e6.setter
    def e6(self, value: float) -> None:
        """Set the e6 property."""
        self._cards[4].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T7.
        """ # nopep8
        return self._cards[4].get_value("e7")

    @e7.setter
    def e7(self, value: float) -> None:
        """Set the e7 property."""
        self._cards[4].set_value("e7", value)

    @property
    def e8(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T8.
        """ # nopep8
        return self._cards[4].get_value("e8")

    @e8.setter
    def e8(self, value: float) -> None:
        """Set the e8 property."""
        self._cards[4].set_value("e8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T1.
        """ # nopep8
        return self._cards[5].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[5].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T2.
        """ # nopep8
        return self._cards[5].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[5].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T3.
        """ # nopep8
        return self._cards[5].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[5].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T4.
        """ # nopep8
        return self._cards[5].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[5].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T5.
        """ # nopep8
        return self._cards[5].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[5].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T6.
        """ # nopep8
        return self._cards[5].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[5].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T7.
        """ # nopep8
        return self._cards[5].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[5].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the Corresponding thermal expansion coefficient at temperature T8.
        """ # nopep8
        return self._cards[5].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[5].set_value("alpha8", value)

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

