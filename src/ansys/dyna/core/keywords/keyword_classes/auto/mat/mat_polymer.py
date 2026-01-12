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

"""Module providing the MatPolymer class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATPOLYMER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("gamma0", float, 40, 10, None),
    FieldSchema("dg", float, 50, 10, None),
    FieldSchema("sc", float, 60, 10, None),
    FieldSchema("st", float, 70, 10, None),
)

_MATPOLYMER_CARD1 = (
    FieldSchema("temp", float, 0, 10, None),
    FieldSchema("k", float, 10, 10, None),
    FieldSchema("cr", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("c", float, 40, 10, None),
)

_MATPOLYMER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatPolymer(KeywordBase):
    """DYNA MAT_POLYMER keyword"""

    keyword = "MAT"
    subkeyword = "POLYMER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatPolymer class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATPOLYMER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPOLYMER_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatPolymer.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATPOLYMER_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified.
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
    def gamma0(self) -> typing.Optional[float]:
        """Get or set the Pre-exponential factor.
        """ # nopep8
        return self._cards[0].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: float) -> None:
        """Set the gamma0 property."""
        self._cards[0].set_value("gamma0", value)

    @property
    def dg(self) -> typing.Optional[float]:
        """Get or set the Energy barrier to flow.
        """ # nopep8
        return self._cards[0].get_value("dg")

    @dg.setter
    def dg(self, value: float) -> None:
        """Set the dg property."""
        self._cards[0].set_value("dg", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the Shear resistance in compression.
        """ # nopep8
        return self._cards[0].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        """Set the sc property."""
        self._cards[0].set_value("sc", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Shear resistance in tension.
        """ # nopep8
        return self._cards[0].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        """Set the st property."""
        self._cards[0].set_value("st", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Absolute temperature.
        """ # nopep8
        return self._cards[1].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        """Set the temp property."""
        self._cards[1].set_value("temp", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Boltzmann constant.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def cr(self) -> typing.Optional[float]:
        """Get or set the Product.
        """ # nopep8
        return self._cards[1].get_value("cr")

    @cr.setter
    def cr(self, value: float) -> None:
        """Set the cr property."""
        self._cards[1].set_value("cr", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Number of  rigid links' between entanglements.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Relaxation factor.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

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

