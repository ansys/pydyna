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

"""Module providing the DefineCurveFunction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECURVEFUNCTION_CARD0 = (
    FieldSchema("lcid", int, 0, 10, 0),
    FieldSchema("sidr", int, 10, 10, 0),
    FieldSchema("sfa", float, 20, 10, 1.0),
    FieldSchema("sfo", float, 30, 10, 1.0),
    FieldSchema("offa", float, 40, 10, 0.0),
    FieldSchema("offo", float, 50, 10, 0.0),
    FieldSchema("dattyp", int, 60, 10, 0),
)

_DEFINECURVEFUNCTION_CARD1 = (
    FieldSchema("function", str, 0, 80, None),
)

class DefineCurveFunction(KeywordBase):
    """DYNA DEFINE_CURVE_FUNCTION keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_FUNCTION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCurveFunction class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFUNCTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFUNCTION_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineCurveFunction.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def lcid(self) -> int:
        """Get or set the Load Curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sidr(self) -> int:
        """Get or set the Stress initialization by dynamic relaxation:
        EQ.0:  load curve used in transient analysis only or for other applications,
        EQ.1:  load curve used in stress initialization but not transient analysis,
        EQ.2:  load curve applies to both initialization and transient analysis.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        """Set the sidr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sidr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sidr", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for abscissa value.  This is useful for simple modifications.
        EQ.0.0:  default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[0].set_value("sfa", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for ordinate value (function).  This is useful for simple modifications.
        EQ.0.0:  default set to 1.0..
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        """Set the sfo property."""
        self._cards[0].set_value("sfo", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for abscissa values, see explanation below.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        """Set the offa property."""
        self._cards[0].set_value("offa", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for ordinate values (function), see explanation below.
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        """Set the offo property."""
        self._cards[0].set_value("offo", value)

    @property
    def dattyp(self) -> int:
        """Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
        """ # nopep8
        return self._cards[0].get_value("dattyp")

    @dattyp.setter
    def dattyp(self, value: int) -> None:
        """Set the dattyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dattyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dattyp", value)

    @property
    def function(self) -> typing.Optional[str]:
        """Get or set the Arithmetic expression involving a combination of the following possibilities.
        """ # nopep8
        return self._cards[1].get_value("function")

    @function.setter
    def function(self, value: str) -> None:
        """Set the function property."""
        self._cards[1].set_value("function", value)

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

