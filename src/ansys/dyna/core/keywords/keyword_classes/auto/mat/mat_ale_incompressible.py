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

"""Module providing the MatAleIncompressible class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATALEINCOMPRESSIBLE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pc", float, 20, 10, None),
    FieldSchema("mu", float, 30, 10, None),
)

_MATALEINCOMPRESSIBLE_CARD1 = (
    FieldSchema("tol", float, 0, 10, 1e-08),
    FieldSchema("dtout", float, 10, 10, 10000000000.0),
    FieldSchema("ncg", int, 20, 10, 50),
    FieldSchema("meth", int, 30, 10, -7),
)

_MATALEINCOMPRESSIBLE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAleIncompressible(KeywordBase):
    """DYNA MAT_ALE_INCOMPRESSIBLE keyword"""

    keyword = "MAT"
    subkeyword = "ALE_INCOMPRESSIBLE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAleIncompressible class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATALEINCOMPRESSIBLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALEINCOMPRESSIBLE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAleIncompressible.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATALEINCOMPRESSIBLE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID. A unique number or label not exceeding 8 charaters
        must be specified. Material ID is referenced in the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff (< or = 0.0).
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[0].set_value("pc", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Dynamic viscosity coefficient.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[0].set_value("mu", value)

    @property
    def tol(self) -> float:
        """Get or set the Tolerance for the convergence of the conjugate gradient.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[1].set_value("tol", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval between screen outputs.
        """ # nopep8
        return self._cards[1].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[1].set_value("dtout", value)

    @property
    def ncg(self) -> int:
        """Get or set the Maximum number of loops in the conjugate gradient.
        """ # nopep8
        return self._cards[1].get_value("ncg")

    @ncg.setter
    def ncg(self, value: int) -> None:
        """Set the ncg property."""
        self._cards[1].set_value("ncg", value)

    @property
    def meth(self) -> int:
        """Get or set the Conjugate gradient methods:
        EQ.-6: solves the poisson equation for the pressure
        EQ.-7: solves the poisson equation for the pressure increment.
        """ # nopep8
        return self._cards[1].get_value("meth")

    @meth.setter
    def meth(self, value: int) -> None:
        """Set the meth property."""
        if value not in [-7, -6, None]:
            raise Exception("""meth must be `None` or one of {-7,-6}.""")
        self._cards[1].set_value("meth", value)

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

