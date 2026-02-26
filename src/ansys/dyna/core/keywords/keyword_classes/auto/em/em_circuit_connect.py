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

"""Module providing the EmCircuitConnect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCIRCUITCONNECT_CARD0 = (
    FieldSchema("conid", int, 0, 10, None),
    FieldSchema("contype", int, 10, 10, None),
    FieldSchema("circ1", int, 20, 10, None),
    FieldSchema("circ2", int, 30, 10, None),
    FieldSchema("c1", float, 40, 10, None),
    FieldSchema("c2", float, 50, 10, None),
)

class EmCircuitConnect(KeywordBase):
    """DYNA EM_CIRCUIT_CONNECT keyword"""

    keyword = "EM"
    subkeyword = "CIRCUIT_CONNECT"

    def __init__(self, **kwargs):
        """Initialize the EmCircuitConnect class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCIRCUITCONNECT_CARD0,
                **kwargs,
            ),        ]
    @property
    def conid(self) -> typing.Optional[int]:
        """Get or set the Id of the Circuit Connect
        """ # nopep8
        return self._cards[0].get_value("conid")

    @conid.setter
    def conid(self, value: int) -> None:
        """Set the conid property."""
        self._cards[0].set_value("conid", value)

    @property
    def contype(self) -> typing.Optional[int]:
        """Get or set the Type of connection between circuits. For the moment, it is only possible to combine circuits by imposing a linear constraint on the global current (=1).
        """ # nopep8
        return self._cards[0].get_value("contype")

    @contype.setter
    def contype(self, value: int) -> None:
        """Set the contype property."""
        self._cards[0].set_value("contype", value)

    @property
    def circ1(self) -> typing.Optional[int]:
        """Get or set the circuit 1
        """ # nopep8
        return self._cards[0].get_value("circ1")

    @circ1.setter
    def circ1(self, value: int) -> None:
        """Set the circ1 property."""
        self._cards[0].set_value("circ1", value)

    @property
    def circ2(self) -> typing.Optional[int]:
        """Get or set the circuit 2
        """ # nopep8
        return self._cards[0].get_value("circ2")

    @circ2.setter
    def circ2(self, value: int) -> None:
        """Set the circ2 property."""
        self._cards[0].set_value("circ2", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Values of the linear constraints if CONTYPE = 1.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Values of the linear constraints if CONTYPE = 1.
        """ # nopep8
        return self._cards[0].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[0].set_value("c2", value)

