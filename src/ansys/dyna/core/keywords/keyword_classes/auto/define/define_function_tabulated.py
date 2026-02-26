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

"""Module providing the DefineFunctionTabulated class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFUNCTIONTABULATED_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_DEFINEFUNCTIONTABULATED_CARD1 = (
    FieldSchema("function", str, 0, 80, None),
)

_DEFINEFUNCTIONTABULATED_CARD2 = (
    FieldSchema("a1", float, 0, 20, None),
    FieldSchema("o1", float, 20, 20, None),
)

_DEFINEFUNCTIONTABULATED_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFunctionTabulated(KeywordBase):
    """DYNA DEFINE_FUNCTION_TABULATED keyword"""

    keyword = "DEFINE"
    subkeyword = "FUNCTION_TABULATED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFunctionTabulated class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFUNCTIONTABULATED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFUNCTIONTABULATED_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFUNCTIONTABULATED_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFunctionTabulated.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFUNCTIONTABULATED_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Function ID.  Functions, tables (see *DEFINE_TABLE), and load curves may not share common ID's.  A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the An optional descriptive heading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def function(self) -> typing.Optional[str]:
        """Get or set the Arithmetic expression involving a combination of independent variables and other functions, i.e., f(a,b,c)=a*2+b*c+sqrt(a*c) where a, b, and c are the independent variables.  The function name, f(a,b,c), must be unique since other functions can then use and reference this function.  For example, g(a,b,c,d)=f(a,b,c)**2+d.  In this example, two *DEFINE_ FUNCTION definitions are needed to define functions f and g.
        """ # nopep8
        return self._cards[1].get_value("function")

    @function.setter
    def function(self, value: str) -> None:
        """Set the function property."""
        self._cards[1].set_value("function", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Abscissa values.  Only pairs have to be defined, see remarks below.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def o1(self) -> typing.Optional[float]:
        """Get or set the Ordinate (function) values.  Only pairs have to be defined, see remarks below.
        """ # nopep8
        return self._cards[2].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        """Set the o1 property."""
        self._cards[2].set_value("o1", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

