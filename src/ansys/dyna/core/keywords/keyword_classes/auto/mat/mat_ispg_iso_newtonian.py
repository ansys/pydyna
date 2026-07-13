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

"""Module providing the MatIspgIsoNewtonian class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATISPGISONEWTONIAN_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("visco", float, 20, 10, None),
    FieldSchema("sften", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("unused", float, 70, 10, None),
)

_MATISPGISONEWTONIAN_CARD1 = (
    FieldSchema("alpha", float, 0, 10, None),
    FieldSchema("tref", float, 10, 10, None),
)

_MATISPGISONEWTONIAN_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatIspgIsoNewtonian(KeywordBase):
    """DYNA MAT_ISPG_ISO_NEWTONIAN keyword"""

    keyword = "MAT"
    subkeyword = "ISPG_ISO_NEWTONIAN"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatIspgIsoNewtonian class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATISPGISONEWTONIAN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATISPGISONEWTONIAN_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatIspgIsoNewtonian._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATISPGISONEWTONIAN_OPTION0_CARD0,
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
    def visco(self) -> typing.Optional[float]:
        """Get or set the Zero-shear viscosity of the fluid
        """ # nopep8
        return self._cards[0].get_value("visco")

    @visco.setter
    def visco(self, value: float) -> None:
        """Set the visco property."""
        self._cards[0].set_value("visco", value)

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
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Ratio of the activation energy to thermodynamic constant
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature in unit of Kelvin
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[1].set_value("tref", value)

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

