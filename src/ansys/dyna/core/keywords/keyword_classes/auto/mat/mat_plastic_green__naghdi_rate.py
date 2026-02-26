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

"""Module providing the MatPlasticGreen_NaghdiRate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATPLASTICGREEN_NAGHDIRATE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
)

_MATPLASTICGREEN_NAGHDIRATE_CARD1 = (
    FieldSchema("sigy", float, 0, 10, None),
    FieldSchema("etan", float, 10, 10, None),
    FieldSchema("src", float, 20, 10, None),
    FieldSchema("srp", float, 30, 10, None),
    FieldSchema("beta", float, 40, 10, None),
)

_MATPLASTICGREEN_NAGHDIRATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatPlasticGreen_NaghdiRate(KeywordBase):
    """DYNA MAT_PLASTIC_GREEN-NAGHDI_RATE keyword"""

    keyword = "MAT"
    subkeyword = "PLASTIC_GREEN-NAGHDI_RATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatPlasticGreen_NaghdiRate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATPLASTICGREEN_NAGHDIRATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPLASTICGREEN_NAGHDIRATE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatPlasticGreen_NaghdiRate.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATPLASTICGREEN_NAGHDIRATE_OPTION0_CARD0,
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
        return self._cards[1].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[1].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[1].set_value("etan", value)

    @property
    def src(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C.
        """ # nopep8
        return self._cards[1].get_value("src")

    @src.setter
    def src(self, value: float) -> None:
        """Set the src property."""
        self._cards[1].set_value("src", value)

    @property
    def srp(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P.
        """ # nopep8
        return self._cards[1].get_value("srp")

    @srp.setter
    def srp(self, value: float) -> None:
        """Set the srp property."""
        self._cards[1].set_value("srp", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter, 0 < beta' < 1.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

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

