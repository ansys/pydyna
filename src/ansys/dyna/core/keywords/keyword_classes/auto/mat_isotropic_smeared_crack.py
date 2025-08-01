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

"""Module providing the MatIsotropicSmearedCrack class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatIsotropicSmearedCrack(KeywordBase):
    """DYNA MAT_ISOTROPIC_SMEARED_CRACK keyword"""

    keyword = "MAT"
    subkeyword = "ISOTROPIC_SMEARED_CRACK"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatIsotropicSmearedCrack class."""
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
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ispl",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigy",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gk",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sr",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatIsotropicSmearedCrack.option_specs[0],
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
    def ispl(self) -> typing.Optional[int]:
        """Get or set the Failure option:
        EQ.0: Maximum principal stress criterion
        EQ.5: Smeared crack model
        EQ.6: Damage model based on modified von Mises strain
        """ # nopep8
        return self._cards[0].get_value("ispl")

    @ispl.setter
    def ispl(self, value: int) -> None:
        """Set the ispl property."""
        self._cards[0].set_value("ispl", value)

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
    def gk(self) -> typing.Optional[float]:
        """Get or set the Critical energy release rate
        """ # nopep8
        return self._cards[0].get_value("gk")

    @gk.setter
    def gk(self, value: float) -> None:
        """Set the gk property."""
        self._cards[0].set_value("gk", value)

    @property
    def sr(self) -> typing.Optional[float]:
        """Get or set the Strength ratio.
        """ # nopep8
        return self._cards[0].get_value("sr")

    @sr.setter
    def sr(self, value: float) -> None:
        """Set the sr property."""
        self._cards[0].set_value("sr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

