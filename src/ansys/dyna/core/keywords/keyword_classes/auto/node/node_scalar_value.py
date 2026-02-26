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

"""Module providing the NodeScalarValue class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_NODESCALARVALUE_CARD0 = (
    FieldSchema("nid", int, 0, 8, None),
    FieldSchema("x1", float, 8, 10, 0.0),
    FieldSchema("x2", float, 18, 10, 0.0),
    FieldSchema("x3", float, 28, 10, 0.0),
    FieldSchema("ndof", int, 38, 8, 0),
)

class NodeScalarValue(KeywordBase):
    """DYNA NODE_SCALAR_VALUE keyword"""

    keyword = "NODE"
    subkeyword = "SCALAR_VALUE"

    def __init__(self, **kwargs):
        """Initialize the NodeScalarValue class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _NODESCALARVALUE_CARD0,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Scalar node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def x1(self) -> float:
        """Get or set the Initial value of Ith degree of freedom .
        """ # nopep8
        return self._cards[0].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[0].set_value("x1", value)

    @property
    def x2(self) -> float:
        """Get or set the Initial value of Ith degree of freedom
        """ # nopep8
        return self._cards[0].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[0].set_value("x2", value)

    @property
    def x3(self) -> float:
        """Get or set the Initial value of Ith degree of freedom
        """ # nopep8
        return self._cards[0].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        """Set the x3 property."""
        self._cards[0].set_value("x3", value)

    @property
    def ndof(self) -> int:
        """Get or set the Number of degrees-of-freedom.
        EQ.0: fully constrained.
        EQ.1: one degree-of-freedom.
        EQ.2: two degree-of-freedom.
        EQ.3: three degree-of-freedom.
        """ # nopep8
        return self._cards[0].get_value("ndof")

    @ndof.setter
    def ndof(self, value: int) -> None:
        """Set the ndof property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ndof must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("ndof", value)

