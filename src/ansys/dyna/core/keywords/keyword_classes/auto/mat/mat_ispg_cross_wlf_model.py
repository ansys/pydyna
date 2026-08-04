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

"""Module providing the MatIspgCrossWlfModel class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATISPGCROSSWLFMODEL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("sften", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("tau", float, 50, 10, None),
    FieldSchema("n", float, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_MATISPGCROSSWLFMODEL_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("d1", float, 10, 10, None),
    FieldSchema("tglass", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("a1", float, 40, 10, None),
    FieldSchema("a2", float, 50, 10, None),
)

_MATISPGCROSSWLFMODEL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatIspgCrossWlfModel(KeywordBase):
    """DYNA MAT_ISPG_CROSS_WLF_MODEL keyword"""

    keyword = "MAT"
    subkeyword = "ISPG_CROSS_WLF_MODEL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatIspgCrossWlfModel class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATISPGCROSSWLFMODEL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATISPGCROSSWLFMODEL_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatIspgCrossWlfModel._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATISPGCROSSWLFMODEL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def sften(self) -> typing.Optional[float]:
        """Get or set the Surface tension coefficient of the fluid
        """ # nopep8
        return self._cards[0].get_value("sften")

    @sften.setter
    def sften(self, value: float) -> None:
        """Set the sften property."""
        self._cards[0].set_value("sften", value)

    @property
    def tau(self) -> typing.Optional[float]:
        """Get or set the Critical stress level at the transition to shear thinning, τ^*
        """ # nopep8
        return self._cards[0].get_value("tau")

    @tau.setter
    def tau(self, value: float) -> None:
        """Set the tau property."""
        self._cards[0].set_value("tau", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Power-law index, n
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Constant D_1 in the zero-shear-rate viscosity equation
        """ # nopep8
        return self._cards[1].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[1].set_value("d1", value)

    @property
    def tglass(self) -> typing.Optional[float]:
        """Get or set the Glass transition temperature, T^*
        """ # nopep8
        return self._cards[1].get_value("tglass")

    @tglass.setter
    def tglass(self, value: float) -> None:
        """Set the tglass property."""
        self._cards[1].set_value("tglass", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Constant A_1 in the zero-shear-rate viscosity equation
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Constant A_2 in the zero-shear-rate viscosity equation
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

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

