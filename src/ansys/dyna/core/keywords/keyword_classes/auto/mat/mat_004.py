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

"""Module providing the Mat004 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT004_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
)

_MAT004_CARD1 = (
    FieldSchema("t1", float, 0, 10, None),
    FieldSchema("t2", float, 10, 10, None),
    FieldSchema("t3", float, 20, 10, None),
    FieldSchema("t4", float, 30, 10, None),
    FieldSchema("t5", float, 40, 10, None),
    FieldSchema("t6", float, 50, 10, None),
    FieldSchema("t7", float, 60, 10, None),
    FieldSchema("t8", float, 70, 10, None),
)

_MAT004_CARD2 = (
    FieldSchema("e1", float, 0, 10, None),
    FieldSchema("e2", float, 10, 10, None),
    FieldSchema("e3", float, 20, 10, None),
    FieldSchema("e4", float, 30, 10, None),
    FieldSchema("e5", float, 40, 10, None),
    FieldSchema("e6", float, 50, 10, None),
    FieldSchema("e7", float, 60, 10, None),
    FieldSchema("e8", float, 70, 10, None),
)

_MAT004_CARD3 = (
    FieldSchema("pr1", float, 0, 10, None),
    FieldSchema("pr2", float, 10, 10, None),
    FieldSchema("pr3", float, 20, 10, None),
    FieldSchema("pr4", float, 30, 10, None),
    FieldSchema("pr5", float, 40, 10, None),
    FieldSchema("pr6", float, 50, 10, None),
    FieldSchema("pr7", float, 60, 10, None),
    FieldSchema("pr8", float, 70, 10, None),
)

_MAT004_CARD4 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
    FieldSchema("alpha4", float, 30, 10, None),
    FieldSchema("alpha5", float, 40, 10, None),
    FieldSchema("alpha6", float, 50, 10, None),
    FieldSchema("alpha7", float, 60, 10, None),
    FieldSchema("alpha8", float, 70, 10, None),
)

_MAT004_CARD5 = (
    FieldSchema("sigy1", float, 0, 10, None),
    FieldSchema("sigy2", float, 10, 10, None),
    FieldSchema("sigy3", float, 20, 10, None),
    FieldSchema("sigy4", float, 30, 10, None),
    FieldSchema("sigy5", float, 40, 10, None),
    FieldSchema("sigy6", float, 50, 10, None),
    FieldSchema("sigy7", float, 60, 10, None),
    FieldSchema("sigy8", float, 70, 10, None),
)

_MAT004_CARD6 = (
    FieldSchema("etan1", float, 0, 10, None),
    FieldSchema("etan2", float, 10, 10, None),
    FieldSchema("etan3", float, 20, 10, None),
    FieldSchema("etan4", float, 30, 10, None),
    FieldSchema("etan5", float, 40, 10, None),
    FieldSchema("etan6", float, 50, 10, None),
    FieldSchema("etan7", float, 60, 10, None),
    FieldSchema("etan8", float, 70, 10, None),
)

class Mat004(KeywordBase):
    """DYNA MAT_004 keyword"""

    keyword = "MAT"
    subkeyword = "004"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat004 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT004_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT004_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat004.option_specs[0],
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
    def t1(self) -> typing.Optional[float]:
        """Get or set the First temperature. The minimum is 2, the maximum is 8.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Second temperature.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Third temperature.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        """Set the t3 property."""
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Fourth temperature.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        """Set the t4 property."""
        self._cards[1].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Fifth temperature.
        """ # nopep8
        return self._cards[1].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        """Set the t5 property."""
        self._cards[1].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Sixth temperature.
        """ # nopep8
        return self._cards[1].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        """Set the t6 property."""
        self._cards[1].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the Seventh temperature.
        """ # nopep8
        return self._cards[1].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        """Set the t7 property."""
        self._cards[1].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the Eighth temperature.
        """ # nopep8
        return self._cards[1].get_value("t8")

    @t8.setter
    def t8(self, value: float) -> None:
        """Set the t8 property."""
        self._cards[1].set_value("t8", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T1.
        """ # nopep8
        return self._cards[2].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[2].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T2.
        """ # nopep8
        return self._cards[2].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[2].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T3.
        """ # nopep8
        return self._cards[2].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        """Set the e3 property."""
        self._cards[2].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T4.
        """ # nopep8
        return self._cards[2].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        """Set the e4 property."""
        self._cards[2].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T5.
        """ # nopep8
        return self._cards[2].get_value("e5")

    @e5.setter
    def e5(self, value: float) -> None:
        """Set the e5 property."""
        self._cards[2].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T6.
        """ # nopep8
        return self._cards[2].get_value("e6")

    @e6.setter
    def e6(self, value: float) -> None:
        """Set the e6 property."""
        self._cards[2].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T7.
        """ # nopep8
        return self._cards[2].get_value("e7")

    @e7.setter
    def e7(self, value: float) -> None:
        """Set the e7 property."""
        self._cards[2].set_value("e7", value)

    @property
    def e8(self) -> typing.Optional[float]:
        """Get or set the Corresponding Young's modulus at temperature T8.
        """ # nopep8
        return self._cards[2].get_value("e8")

    @e8.setter
    def e8(self, value: float) -> None:
        """Set the e8 property."""
        self._cards[2].set_value("e8", value)

    @property
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T1.
        """ # nopep8
        return self._cards[3].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        """Set the pr1 property."""
        self._cards[3].set_value("pr1", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T2.
        """ # nopep8
        return self._cards[3].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        """Set the pr2 property."""
        self._cards[3].set_value("pr2", value)

    @property
    def pr3(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T3.
        """ # nopep8
        return self._cards[3].get_value("pr3")

    @pr3.setter
    def pr3(self, value: float) -> None:
        """Set the pr3 property."""
        self._cards[3].set_value("pr3", value)

    @property
    def pr4(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T4.
        """ # nopep8
        return self._cards[3].get_value("pr4")

    @pr4.setter
    def pr4(self, value: float) -> None:
        """Set the pr4 property."""
        self._cards[3].set_value("pr4", value)

    @property
    def pr5(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T5.
        """ # nopep8
        return self._cards[3].get_value("pr5")

    @pr5.setter
    def pr5(self, value: float) -> None:
        """Set the pr5 property."""
        self._cards[3].set_value("pr5", value)

    @property
    def pr6(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T6.
        """ # nopep8
        return self._cards[3].get_value("pr6")

    @pr6.setter
    def pr6(self, value: float) -> None:
        """Set the pr6 property."""
        self._cards[3].set_value("pr6", value)

    @property
    def pr7(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T7.
        """ # nopep8
        return self._cards[3].get_value("pr7")

    @pr7.setter
    def pr7(self, value: float) -> None:
        """Set the pr7 property."""
        self._cards[3].set_value("pr7", value)

    @property
    def pr8(self) -> typing.Optional[float]:
        """Get or set the Corresponding Poisson's ratio at temperature T8.
        """ # nopep8
        return self._cards[3].get_value("pr8")

    @pr8.setter
    def pr8(self, value: float) -> None:
        """Set the pr8 property."""
        self._cards[3].set_value("pr8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T1 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[4].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T2 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[4].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T3 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[4].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T4 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[4].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T5 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[4].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T6 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[4].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T7 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[4].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the Corresponding coefficient of thermal expansion at temperature T8 (no defaults).
        """ # nopep8
        return self._cards[4].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[4].set_value("alpha8", value)

    @property
    def sigy1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T1 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy1")

    @sigy1.setter
    def sigy1(self, value: float) -> None:
        """Set the sigy1 property."""
        self._cards[5].set_value("sigy1", value)

    @property
    def sigy2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T2 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy2")

    @sigy2.setter
    def sigy2(self, value: float) -> None:
        """Set the sigy2 property."""
        self._cards[5].set_value("sigy2", value)

    @property
    def sigy3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T3 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy3")

    @sigy3.setter
    def sigy3(self, value: float) -> None:
        """Set the sigy3 property."""
        self._cards[5].set_value("sigy3", value)

    @property
    def sigy4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T4 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy4")

    @sigy4.setter
    def sigy4(self, value: float) -> None:
        """Set the sigy4 property."""
        self._cards[5].set_value("sigy4", value)

    @property
    def sigy5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T5 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy5")

    @sigy5.setter
    def sigy5(self, value: float) -> None:
        """Set the sigy5 property."""
        self._cards[5].set_value("sigy5", value)

    @property
    def sigy6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T6 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy6")

    @sigy6.setter
    def sigy6(self, value: float) -> None:
        """Set the sigy6 property."""
        self._cards[5].set_value("sigy6", value)

    @property
    def sigy7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T7 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy7")

    @sigy7.setter
    def sigy7(self, value: float) -> None:
        """Set the sigy7 property."""
        self._cards[5].set_value("sigy7", value)

    @property
    def sigy8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stresse at temperature T8 (no defaults).
        """ # nopep8
        return self._cards[5].get_value("sigy8")

    @sigy8.setter
    def sigy8(self, value: float) -> None:
        """Set the sigy8 property."""
        self._cards[5].set_value("sigy8", value)

    @property
    def etan1(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T1 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan1")

    @etan1.setter
    def etan1(self, value: float) -> None:
        """Set the etan1 property."""
        self._cards[6].set_value("etan1", value)

    @property
    def etan2(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T2 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan2")

    @etan2.setter
    def etan2(self, value: float) -> None:
        """Set the etan2 property."""
        self._cards[6].set_value("etan2", value)

    @property
    def etan3(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T3 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan3")

    @etan3.setter
    def etan3(self, value: float) -> None:
        """Set the etan3 property."""
        self._cards[6].set_value("etan3", value)

    @property
    def etan4(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T4 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan4")

    @etan4.setter
    def etan4(self, value: float) -> None:
        """Set the etan4 property."""
        self._cards[6].set_value("etan4", value)

    @property
    def etan5(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T5 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan5")

    @etan5.setter
    def etan5(self, value: float) -> None:
        """Set the etan5 property."""
        self._cards[6].set_value("etan5", value)

    @property
    def etan6(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T6 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan6")

    @etan6.setter
    def etan6(self, value: float) -> None:
        """Set the etan6 property."""
        self._cards[6].set_value("etan6", value)

    @property
    def etan7(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T7 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan7")

    @etan7.setter
    def etan7(self, value: float) -> None:
        """Set the etan7 property."""
        self._cards[6].set_value("etan7", value)

    @property
    def etan8(self) -> typing.Optional[float]:
        """Get or set the Corresponding plastic hardening modulus at temperature T8 (no default).
        """ # nopep8
        return self._cards[6].get_value("etan8")

    @etan8.setter
    def etan8(self, value: float) -> None:
        """Set the etan8 property."""
        self._cards[6].set_value("etan8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

