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

"""Module providing the DefineCurveFldFromTriaxialLimit class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECURVEFLDFROMTRIAXIALLIMIT_CARD0 = (
    FieldSchema("lcid", int, 0, 10, None),
)

_DEFINECURVEFLDFROMTRIAXIALLIMIT_CARD1 = (
    FieldSchema("a1", float, 0, 20, 0.0),
    FieldSchema("o1", float, 20, 20, 0.0),
)

_DEFINECURVEFLDFROMTRIAXIALLIMIT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCurveFldFromTriaxialLimit(KeywordBase):
    """DYNA DEFINE_CURVE_FLD_FROM_TRIAXIAL_LIMIT keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_FLD_FROM_TRIAXIAL_LIMIT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCurveFldFromTriaxialLimit class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFLDFROMTRIAXIALLIMIT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFLDFROMTRIAXIALLIMIT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineCurveFldFromTriaxialLimit.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECURVEFLDFROMTRIAXIALLIMIT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the FLD Load curve ID to be created.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def a1(self) -> float:
        """Get or set the Abscissa values. Only pairs have to be defined.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def o1(self) -> float:
        """Get or set the Ordinate (function) values. Only pairs have to be defined.
        """ # nopep8
        return self._cards[1].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        """Set the o1 property."""
        self._cards[1].set_value("o1", value)

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

