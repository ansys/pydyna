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

"""Module providing the MatElastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATELASTIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("da", float, 40, 10, None),
    FieldSchema("db", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_MATELASTIC_CARD1 = (
    FieldSchema("efunc", str, 0, 10, "P"),
    FieldSchema("cnvt", float, 10, 10, 0.001),
    FieldSchema("iterlm", int, 20, 10, 3),
)

_MATELASTIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatElastic(KeywordBase):
    """DYNA MAT_ELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatElastic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATELASTIC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATELASTIC_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatElastic._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATELASTIC_OPTION0_CARD0,
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
        """Get or set the Definition of Young's modulus
        GT.0: E is the Young's modulus.
        LT.0: | E | is the ID of a curve defining Young's modulus as a function of elemental variables EFUNC; It is supported for explicit simulation only.
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
    def da(self) -> typing.Optional[float]:
        """Get or set the Axial damping factor (used for Belytschko-Schwer beam, type 2, only).
        """ # nopep8
        return self._cards[0].get_value("da")

    @da.setter
    def da(self, value: float) -> None:
        """Set the da property."""
        self._cards[0].set_value("da", value)

    @property
    def db(self) -> typing.Optional[float]:
        """Get or set the Bending damping factor (used for Belytschko-Schwer beam, type 2, only).
        """ # nopep8
        return self._cards[0].get_value("db")

    @db.setter
    def db(self, value: float) -> None:
        """Set the db property."""
        self._cards[0].set_value("db", value)

    @property
    def efunc(self) -> str:
        """Get or set the The element variable used as the independent variable of curve |E|.P: elemental pressure
        """ # nopep8
        return self._cards[1].get_value("efunc")

    @efunc.setter
    def efunc(self, value: str) -> None:
        """Set the efunc property."""
        self._cards[1].set_value("efunc", value)

    @property
    def cnvt(self) -> float:
        """Get or set the Convergence tolerance, needed when EFUNC is a variant of stress.
        """ # nopep8
        return self._cards[1].get_value("cnvt")

    @cnvt.setter
    def cnvt(self, value: float) -> None:
        """Set the cnvt property."""
        self._cards[1].set_value("cnvt", value)

    @property
    def iterlm(self) -> int:
        """Get or set the Iteration limit, needed when EFUNC is a variant of stress.
        """ # nopep8
        return self._cards[1].get_value("iterlm")

    @iterlm.setter
    def iterlm(self, value: int) -> None:
        """Set the iterlm property."""
        self._cards[1].set_value("iterlm", value)

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

