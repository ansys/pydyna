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

"""Module providing the Mat269 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT269_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
    FieldSchema("g", float, 30, 10, None),
    FieldSchema("gv", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("nv", float, 60, 10, None),
)

_MAT269_CARD1 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("m", float, 10, 10, None),
    FieldSchema("gam0", float, 20, 10, None),
    FieldSchema("tauh", float, 30, 10, None),
)

class Mat269(KeywordBase):
    """DYNA MAT_269 keyword"""

    keyword = "MAT"
    subkeyword = "269"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat269 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT269_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT269_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat269.option_specs[0],
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
        """Get or set the Material identification.
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Elastic bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def gv(self) -> typing.Optional[float]:
        """Get or set the Viscoelastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gv")

    @gv.setter
    def gv(self, value: float) -> None:
        """Set the gv property."""
        self._cards[0].set_value("gv", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Elastic segment number.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def nv(self) -> typing.Optional[float]:
        """Get or set the Viscoelastic segment number.
        """ # nopep8
        return self._cards[0].get_value("nv")

    @nv.setter
    def nv(self, value: float) -> None:
        """Set the nv property."""
        self._cards[0].set_value("nv", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Inelastic strain exponent, should be less than zero.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Inelastic stress exponent.
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def gam0(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("gam0")

    @gam0.setter
    def gam0(self, value: float) -> None:
        """Set the gam0 property."""
        self._cards[1].set_value("gam0", value)

    @property
    def tauh(self) -> typing.Optional[float]:
        """Get or set the Reference Kirchhoff stress.
        """ # nopep8
        return self._cards[1].get_value("tauh")

    @tauh.setter
    def tauh(self, value: float) -> None:
        """Set the tauh property."""
        self._cards[1].set_value("tauh", value)

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

