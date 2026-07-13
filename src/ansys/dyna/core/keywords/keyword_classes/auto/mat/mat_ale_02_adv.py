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

"""Module providing the MatAle02Adv class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATALE02ADV_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("iadiab", int, 10, 10, 0),
    FieldSchema("runiv", float, 20, 10, None),
    FieldSchema("nsps", int, 30, 10, None),
)

_MATALE02ADV_CARD1 = (
    FieldSchema("molwt", float, 0, 10, None),
    FieldSchema("cpmol", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
)

_MATALE02ADV_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAle02Adv(KeywordBase):
    """DYNA MAT_ALE_02_ADV keyword"""

    keyword = "MAT"
    subkeyword = "ALE_02_ADV"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAle02Adv class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATALE02ADV_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATALE02ADV_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatAle02Adv._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATALE02ADV_OPTION0_CARD0,
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
    def iadiab(self) -> int:
        """Get or set the Flag to turn on/off adiabatic compression logics for an ideal gas (remark 5).
        EQ.0: OFF (default)
        EQ.1: ON.
        """ # nopep8
        return self._cards[0].get_value("iadiab")

    @iadiab.setter
    def iadiab(self, value: int) -> None:
        """Set the iadiab property."""
        if value not in [0, 1, None]:
            raise Exception("""iadiab must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iadiab", value)

    @property
    def runiv(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant in per-mole unit (8.31447 J/(mole x K)).
        """ # nopep8
        return self._cards[0].get_value("runiv")

    @runiv.setter
    def runiv(self, value: float) -> None:
        """Set the runiv property."""
        self._cards[0].set_value("runiv", value)

    @property
    def nsps(self) -> typing.Optional[int]:
        """Get or set the Number of species
        """ # nopep8
        return self._cards[0].get_value("nsps")

    @nsps.setter
    def nsps(self, value: int) -> None:
        """Set the nsps property."""
        self._cards[0].set_value("nsps", value)

    @property
    def molwt(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[1].get_value("molwt")

    @molwt.setter
    def molwt(self, value: float) -> None:
        """Set the molwt property."""
        self._cards[1].set_value("molwt", value)

    @property
    def cpmol(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant pressure in per-mole unit. These are nominal heat capacity values typically at STP. These are denoted by the variable A in the equation in Remark of *MAT_148.
        """ # nopep8
        return self._cards[1].get_value("cpmol")

    @cpmol.setter
    def cpmol(self, value: float) -> None:
        """Set the cpmol property."""
        self._cards[1].set_value("cpmol", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the First-order coefficient for a temperature-dependent heat capacity at constant pressure. These are denoted by the variable B in the equation in Remark 2 of *MAT_148
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Second-order coefficient for a temperature-dependent heat capacity at constant pressure. These are denoted by the variable C in the equation in Remark 2 of *MAT_148.
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

