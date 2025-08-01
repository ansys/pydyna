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

"""Module providing the MatConcreteDamage class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatConcreteDamage(KeywordBase):
    """DYNA MAT_CONCRETE_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "CONCRETE_DAMAGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatConcreteDamage class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigf",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a0",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a0y",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1y",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2y",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1f",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2f",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b1",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b2",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b3",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "per",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "er",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prr",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigy",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "etan",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcp",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcr",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lambda-1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lambda-9",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-10",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-11",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-12",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lambda-13",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nu-1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nu-9",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-10",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-11",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-12",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu-13",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatConcreteDamage.option_specs[0],
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Maximum principal stress for failure.
        """ # nopep8
        return self._cards[1].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[1].set_value("sigf", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Cohesion.
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        """Set the a0 property."""
        self._cards[1].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

    @property
    def a0y(self) -> typing.Optional[float]:
        """Get or set the Cohesion for yield.
        """ # nopep8
        return self._cards[2].get_value("a0y")

    @a0y.setter
    def a0y(self, value: float) -> None:
        """Set the a0y property."""
        self._cards[2].set_value("a0y", value)

    @property
    def a1y(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for yield limit.
        """ # nopep8
        return self._cards[2].get_value("a1y")

    @a1y.setter
    def a1y(self, value: float) -> None:
        """Set the a1y property."""
        self._cards[2].set_value("a1y", value)

    @property
    def a2y(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for yield limit.
        """ # nopep8
        return self._cards[2].get_value("a2y")

    @a2y.setter
    def a2y(self, value: float) -> None:
        """Set the a2y property."""
        self._cards[2].set_value("a2y", value)

    @property
    def a1f(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for failed material.
        """ # nopep8
        return self._cards[2].get_value("a1f")

    @a1f.setter
    def a1f(self, value: float) -> None:
        """Set the a1f property."""
        self._cards[2].set_value("a1f", value)

    @property
    def a2f(self) -> typing.Optional[float]:
        """Get or set the Pressure hardening coefficient for failed material.
        """ # nopep8
        return self._cards[2].get_value("a2f")

    @a2f.setter
    def a2f(self, value: float) -> None:
        """Set the a2f property."""
        self._cards[2].set_value("a2f", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Damage scaling factor.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[2].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Damage scaling factor for uniaxial tensile path.
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[2].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Damage scaling factor for triaxial tensile path.
        """ # nopep8
        return self._cards[2].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        """Set the b3 property."""
        self._cards[2].set_value("b3", value)

    @property
    def per(self) -> typing.Optional[float]:
        """Get or set the Percent reinforcement.
        """ # nopep8
        return self._cards[3].get_value("per")

    @per.setter
    def per(self, value: float) -> None:
        """Set the per property."""
        self._cards[3].set_value("per", value)

    @property
    def er(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus for reinforcement.
        """ # nopep8
        return self._cards[3].get_value("er")

    @er.setter
    def er(self, value: float) -> None:
        """Set the er property."""
        self._cards[3].set_value("er", value)

    @property
    def prr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for reinforcement.
        """ # nopep8
        return self._cards[3].get_value("prr")

    @prr.setter
    def prr(self, value: float) -> None:
        """Set the prr property."""
        self._cards[3].set_value("prr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[3].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[3].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus/plastic hardening modulus.
        """ # nopep8
        return self._cards[3].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[3].set_value("etan", value)

    @property
    def lcp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving rate sensitivity for principal material, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[3].get_value("lcp")

    @lcp.setter
    def lcp(self, value: int) -> None:
        """Set the lcp property."""
        self._cards[3].set_value("lcp", value)

    @property
    def lcr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving rate sensitivity for reinforcement, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[3].get_value("lcr")

    @lcr.setter
    def lcr(self, value: int) -> None:
        """Set the lcr property."""
        self._cards[3].set_value("lcr", value)

    @property
    def lambda_1(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-1")

    @lambda_1.setter
    def lambda_1(self, value: float) -> None:
        """Set the lambda_1 property."""
        self._cards[4].set_value("lambda-1", value)

    @property
    def lambda_2(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-2")

    @lambda_2.setter
    def lambda_2(self, value: float) -> None:
        """Set the lambda_2 property."""
        self._cards[4].set_value("lambda-2", value)

    @property
    def lambda_3(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-3")

    @lambda_3.setter
    def lambda_3(self, value: float) -> None:
        """Set the lambda_3 property."""
        self._cards[4].set_value("lambda-3", value)

    @property
    def lambda_4(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-4")

    @lambda_4.setter
    def lambda_4(self, value: float) -> None:
        """Set the lambda_4 property."""
        self._cards[4].set_value("lambda-4", value)

    @property
    def lambda_5(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-5")

    @lambda_5.setter
    def lambda_5(self, value: float) -> None:
        """Set the lambda_5 property."""
        self._cards[4].set_value("lambda-5", value)

    @property
    def lambda_6(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-6")

    @lambda_6.setter
    def lambda_6(self, value: float) -> None:
        """Set the lambda_6 property."""
        self._cards[4].set_value("lambda-6", value)

    @property
    def lambda_7(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-7")

    @lambda_7.setter
    def lambda_7(self, value: float) -> None:
        """Set the lambda_7 property."""
        self._cards[4].set_value("lambda-7", value)

    @property
    def lambda_8(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[4].get_value("lambda-8")

    @lambda_8.setter
    def lambda_8(self, value: float) -> None:
        """Set the lambda_8 property."""
        self._cards[4].set_value("lambda-8", value)

    @property
    def lambda_9(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[5].get_value("lambda-9")

    @lambda_9.setter
    def lambda_9(self, value: float) -> None:
        """Set the lambda_9 property."""
        self._cards[5].set_value("lambda-9", value)

    @property
    def lambda_10(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[5].get_value("lambda-10")

    @lambda_10.setter
    def lambda_10(self, value: float) -> None:
        """Set the lambda_10 property."""
        self._cards[5].set_value("lambda-10", value)

    @property
    def lambda_11(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[5].get_value("lambda-11")

    @lambda_11.setter
    def lambda_11(self, value: float) -> None:
        """Set the lambda_11 property."""
        self._cards[5].set_value("lambda-11", value)

    @property
    def lambda_12(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[5].get_value("lambda-12")

    @lambda_12.setter
    def lambda_12(self, value: float) -> None:
        """Set the lambda_12 property."""
        self._cards[5].set_value("lambda-12", value)

    @property
    def lambda_13(self) -> typing.Optional[float]:
        """Get or set the Tabulated damage function.
        """ # nopep8
        return self._cards[5].get_value("lambda-13")

    @lambda_13.setter
    def lambda_13(self, value: float) -> None:
        """Set the lambda_13 property."""
        self._cards[5].set_value("lambda-13", value)

    @property
    def nu_1(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-1")

    @nu_1.setter
    def nu_1(self, value: float) -> None:
        """Set the nu_1 property."""
        self._cards[6].set_value("nu-1", value)

    @property
    def nu_2(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-2")

    @nu_2.setter
    def nu_2(self, value: float) -> None:
        """Set the nu_2 property."""
        self._cards[6].set_value("nu-2", value)

    @property
    def nu_3(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-3")

    @nu_3.setter
    def nu_3(self, value: float) -> None:
        """Set the nu_3 property."""
        self._cards[6].set_value("nu-3", value)

    @property
    def nu_4(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-4")

    @nu_4.setter
    def nu_4(self, value: float) -> None:
        """Set the nu_4 property."""
        self._cards[6].set_value("nu-4", value)

    @property
    def nu_5(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-5")

    @nu_5.setter
    def nu_5(self, value: float) -> None:
        """Set the nu_5 property."""
        self._cards[6].set_value("nu-5", value)

    @property
    def nu_6(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-6")

    @nu_6.setter
    def nu_6(self, value: float) -> None:
        """Set the nu_6 property."""
        self._cards[6].set_value("nu-6", value)

    @property
    def nu_7(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-7")

    @nu_7.setter
    def nu_7(self, value: float) -> None:
        """Set the nu_7 property."""
        self._cards[6].set_value("nu-7", value)

    @property
    def nu_8(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[6].get_value("nu-8")

    @nu_8.setter
    def nu_8(self, value: float) -> None:
        """Set the nu_8 property."""
        self._cards[6].set_value("nu-8", value)

    @property
    def nu_9(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[7].get_value("nu-9")

    @nu_9.setter
    def nu_9(self, value: float) -> None:
        """Set the nu_9 property."""
        self._cards[7].set_value("nu-9", value)

    @property
    def nu_10(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[7].get_value("nu-10")

    @nu_10.setter
    def nu_10(self, value: float) -> None:
        """Set the nu_10 property."""
        self._cards[7].set_value("nu-10", value)

    @property
    def nu_11(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[7].get_value("nu-11")

    @nu_11.setter
    def nu_11(self, value: float) -> None:
        """Set the nu_11 property."""
        self._cards[7].set_value("nu-11", value)

    @property
    def nu_12(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[7].get_value("nu-12")

    @nu_12.setter
    def nu_12(self, value: float) -> None:
        """Set the nu_12 property."""
        self._cards[7].set_value("nu-12", value)

    @property
    def nu_13(self) -> typing.Optional[float]:
        """Get or set the Tabulated scale factor.
        """ # nopep8
        return self._cards[7].get_value("nu-13")

    @nu_13.setter
    def nu_13(self, value: float) -> None:
        """Set the nu_13 property."""
        self._cards[7].set_value("nu-13", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

