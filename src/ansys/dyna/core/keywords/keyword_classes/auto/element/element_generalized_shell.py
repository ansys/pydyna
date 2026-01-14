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

"""Module providing the ElementGeneralizedShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTGENERALIZEDSHELL_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("nmnp", int, 20, 10, None),
)

_ELEMENTGENERALIZEDSHELL_CARD1 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
    FieldSchema("n5", int, 40, 10, None),
    FieldSchema("n6", int, 50, 10, None),
    FieldSchema("n7", int, 60, 10, None),
    FieldSchema("n8", int, 70, 10, None),
)

class ElementGeneralizedShell(KeywordBase):
    """DYNA ELEMENT_GENERALIZED_SHELL keyword"""

    keyword = "ELEMENT"
    subkeyword = "GENERALIZED_SHELL"

    def __init__(self, **kwargs):
        """Initialize the ElementGeneralizedShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTGENERALIZEDSHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTGENERALIZEDSHELL_CARD1,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. Chose a unique number with respect to other elements
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nmnp(self) -> typing.Optional[int]:
        """Get or set the Number of nodes to define this element.
        """ # nopep8
        return self._cards[0].get_value("nmnp")

    @nmnp.setter
    def nmnp(self, value: int) -> None:
        """Set the nmnp property."""
        self._cards[0].set_value("nmnp", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Nodal point i (defined via *NODE) to define connectivity of this element
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

