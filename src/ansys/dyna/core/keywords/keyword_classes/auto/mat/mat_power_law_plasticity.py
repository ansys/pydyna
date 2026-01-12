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

"""Module providing the MatPowerLawPlasticity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATPOWERLAWPLASTICITY_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("k", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("src", float, 60, 10, None),
    FieldSchema("srp", float, 70, 10, None),
)

_MATPOWERLAWPLASTICITY_CARD1 = (
    FieldSchema("sigy", float, 0, 10, None),
    FieldSchema("vp", float, 10, 10, 0.0),
    FieldSchema("epsf", float, 20, 10, None),
)

class MatPowerLawPlasticity(KeywordBase):
    """DYNA MAT_POWER_LAW_PLASTICITY keyword"""

    keyword = "MAT"
    subkeyword = "POWER_LAW_PLASTICITY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatPowerLawPlasticity class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATPOWERLAWPLASTICITY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPOWERLAWPLASTICITY_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatPowerLawPlasticity.option_specs[0],
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Strength coefficient.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening exponent.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def src(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C.
        EQ.0: rate effects are ignored.
        """ # nopep8
        return self._cards[0].get_value("src")

    @src.setter
    def src(self, value: float) -> None:
        """Set the src property."""
        self._cards[0].set_value("src", value)

    @property
    def srp(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P.
        EQ.0: rate effects are ignored.
        """ # nopep8
        return self._cards[0].get_value("srp")

    @srp.setter
    def srp(self, value: float) -> None:
        """Set the srp property."""
        self._cards[0].set_value("srp", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Optional input parameter for defining the initial yield stress. The strain to yield is calculated as described in keyword manual page 75 (volume two).
        LT.0.02: epsilon-yp = SIGY
        GE.0.02: Please see keyword manual page 75 (volume two).
        """ # nopep8
        return self._cards[1].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[1].set_value("sigy", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("vp", value)

    @property
    def epsf(self) -> typing.Optional[float]:
        """Get or set the Plastic failure strain for element deletion.
        """ # nopep8
        return self._cards[1].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        """Set the epsf property."""
        self._cards[1].set_value("epsf", value)

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

