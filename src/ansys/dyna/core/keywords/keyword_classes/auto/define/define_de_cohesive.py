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

"""Module providing the DefineDeCohesive class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDECOHESIVE_CARD0 = (
    FieldSchema("sid", int, 0, 10, 0),
    FieldSchema("styp", int, 10, 10, 0),
)

_DEFINEDECOHESIVE_CARD1 = (
    FieldSchema("gamma", float, 0, 10, 0.0),
    FieldSchema("vol", float, 10, 10, 0.0),
    FieldSchema("ang", float, 20, 10, 0.0),
    FieldSchema("gap", float, 30, 10, 0.0),
)

_DEFINEDECOHESIVE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeCohesive(KeywordBase):
    """DYNA DEFINE_DE_COHESIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_COHESIVE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeCohesive class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDECOHESIVE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDECOHESIVE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeCohesive.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDECOHESIVE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> int:
        """Get or set the Node set ID, part set ID or part ID defining DES with cohesive force.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def styp(self) -> int:
        """Get or set the SID type:
        EQ.0:	Node set
        EQ.1:	Part set
        EQ.2: Part.
        """ # nopep8
        return self._cards[0].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        """Set the styp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""styp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("styp", value)

    @property
    def gamma(self) -> float:
        """Get or set the Liquid surface tension.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[1].set_value("gamma", value)

    @property
    def vol(self) -> float:
        """Get or set the Volume fraction.
        """ # nopep8
        return self._cards[1].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        """Set the vol property."""
        self._cards[1].set_value("vol", value)

    @property
    def ang(self) -> float:
        """Get or set the Contact angle.
        """ # nopep8
        return self._cards[1].get_value("ang")

    @ang.setter
    def ang(self, value: float) -> None:
        """Set the ang property."""
        self._cards[1].set_value("ang", value)

    @property
    def gap(self) -> float:
        """Get or set the Spatial limit for the existence of liquid bridge between particles.
        A liquid bridge will exist when the distance between two particles is less or equal to min(GAP, drup)
        where drup is the rupture distance of the bridge automatically calculated by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        """Set the gap property."""
        self._cards[1].set_value("gap", value)

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

