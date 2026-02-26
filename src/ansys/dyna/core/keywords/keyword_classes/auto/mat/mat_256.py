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

"""Module providing the Mat256 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT256_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
    FieldSchema("g", float, 30, 10, None),
    FieldSchema("mr", float, 40, 10, None),
    FieldSchema("ll", float, 50, 10, None),
    FieldSchema("nu0", float, 60, 10, None),
    FieldSchema("m", float, 70, 10, None),
)

_MAT256_CARD1 = (
    FieldSchema("alpha", float, 0, 10, None),
    FieldSchema("h0", float, 10, 10, None),
    FieldSchema("scv", float, 20, 10, 1.0),
    FieldSchema("b", float, 30, 10, None),
    FieldSchema("ecv", float, 40, 10, None),
    FieldSchema("g0", float, 50, 10, None),
    FieldSchema("s0", float, 60, 10, None),
)

_MAT256_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat256(KeywordBase):
    """DYNA MAT_256 keyword"""

    keyword = "MAT"
    subkeyword = "256"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat256 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT256_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT256_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat256.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT256_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def mr(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter.
        """ # nopep8
        return self._cards[0].get_value("mr")

    @mr.setter
    def mr(self, value: float) -> None:
        """Set the mr property."""
        self._cards[0].set_value("mr", value)

    @property
    def ll(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter.
        """ # nopep8
        return self._cards[0].get_value("ll")

    @ll.setter
    def ll(self, value: float) -> None:
        """Set the ll property."""
        self._cards[0].set_value("ll", value)

    @property
    def nu0(self) -> typing.Optional[float]:
        """Get or set the Creep parameter.
        """ # nopep8
        return self._cards[0].get_value("nu0")

    @nu0.setter
    def nu0(self, value: float) -> None:
        """Set the nu0 property."""
        self._cards[0].set_value("nu0", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Creep parameter.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Creep parameter.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def h0(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("h0")

    @h0.setter
    def h0(self, value: float) -> None:
        """Set the h0 property."""
        self._cards[1].set_value("h0", value)

    @property
    def scv(self) -> float:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("scv")

    @scv.setter
    def scv(self, value: float) -> None:
        """Set the scv property."""
        self._cards[1].set_value("scv", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def ecv(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("ecv")

    @ecv.setter
    def ecv(self, value: float) -> None:
        """Set the ecv property."""
        self._cards[1].set_value("ecv", value)

    @property
    def g0(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("g0")

    @g0.setter
    def g0(self, value: float) -> None:
        """Set the g0 property."""
        self._cards[1].set_value("g0", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter.
        """ # nopep8
        return self._cards[1].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        """Set the s0 property."""
        self._cards[1].set_value("s0", value)

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

