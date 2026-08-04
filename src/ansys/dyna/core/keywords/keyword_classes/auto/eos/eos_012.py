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

"""Module providing the Eos012 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOS012_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("cvmas", float, 10, 10, None),
    FieldSchema("cpmas", float, 20, 10, None),
    FieldSchema("b", float, 30, 10, None),
    FieldSchema("c", float, 40, 10, None),
    FieldSchema("t0", float, 50, 10, None),
    FieldSchema("v0", float, 60, 10, None),
    FieldSchema("vc0", float, 70, 10, None),
)

_EOS012_CARD1 = (
    FieldSchema("adiab", float, 0, 10, None),
)

_EOS012_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Eos012(KeywordBase):
    """DYNA EOS_012 keyword"""

    keyword = "EOS"
    subkeyword = "012"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Eos012 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOS012_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS012_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Eos012._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _EOS012_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def cvmas(self) -> typing.Optional[float]:
        """Get or set the Nominal constant-volume specific heat coefficient
        """ # nopep8
        return self._cards[0].get_value("cvmas")

    @cvmas.setter
    def cvmas(self, value: float) -> None:
        """Set the cvmas property."""
        self._cards[0].set_value("cvmas", value)

    @property
    def cpmas(self) -> typing.Optional[float]:
        """Get or set the Nominal constant-pressure specific heat coefficient
        """ # nopep8
        return self._cards[0].get_value("cpmas")

    @cpmas.setter
    def cpmas(self, value: float) -> None:
        """Set the cpmas property."""
        self._cards[0].set_value("cpmas", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Linear coefficient, b, for the variations of Cv and Cp as a function of T (see Remark 1)
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Quadratic coefficient, r, for the variations of Cv and Cp as a function of T (see Remark 1)
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial temperature
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume(see the beginning of the *EOS section)
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

    @property
    def vc0(self) -> typing.Optional[float]:
        """Get or set the Van der Waals covolume
        """ # nopep8
        return self._cards[0].get_value("vc0")

    @vc0.setter
    def vc0(self, value: float) -> None:
        """Set the vc0 property."""
        self._cards[0].set_value("vc0", value)

    @property
    def adiab(self) -> typing.Optional[float]:
        """Get or set the Adiabatic flag:
        EQ.0.0: off
        EQ.1.0: on; ideal gas follows adiabatic law
        """ # nopep8
        return self._cards[1].get_value("adiab")

    @adiab.setter
    def adiab(self, value: float) -> None:
        """Set the adiab property."""
        self._cards[1].set_value("adiab", value)

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

