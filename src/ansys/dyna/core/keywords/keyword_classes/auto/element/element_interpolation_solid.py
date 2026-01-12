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

"""Module providing the ElementInterpolationSolid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTINTERPOLATIONSOLID_CARD0 = (
    FieldSchema("eids", int, 0, 10, None),
    FieldSchema("eidgs", int, 10, 10, None),
    FieldSchema("ngp", int, 20, 10, None),
)

_ELEMENTINTERPOLATIONSOLID_CARD1 = (
    FieldSchema("ip1", int, 0, 10, None),
    FieldSchema("w1", float, 10, 10, None),
    FieldSchema("ip2", int, 20, 10, None),
    FieldSchema("w2", float, 30, 10, None),
    FieldSchema("ip3", int, 40, 10, None),
    FieldSchema("w3", float, 50, 10, None),
    FieldSchema("ip4", int, 60, 10, None),
    FieldSchema("w4", float, 70, 10, None),
)

class ElementInterpolationSolid(KeywordBase):
    """DYNA ELEMENT_INTERPOLATION_SOLID keyword"""

    keyword = "ELEMENT"
    subkeyword = "INTERPOLATION_SOLID"

    def __init__(self, **kwargs):
        """Initialize the ElementInterpolationSolid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTINTERPOLATIONSOLID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTINTERPOLATIONSOLID_CARD1,
                **kwargs,
            ),        ]
    @property
    def eids(self) -> typing.Optional[int]:
        """Get or set the Element ID of the interpolation solid. This needs to coincide with a
        proper definition of a 8-noded solid element (*ELEMENT_SOLID)
        using interpolation nodes (*CONSTRAINED_NODE_INTERPOLATION).
        """ # nopep8
        return self._cards[0].get_value("eids")

    @eids.setter
    def eids(self, value: int) -> None:
        """Set the eids property."""
        self._cards[0].set_value("eids", value)

    @property
    def eidgs(self) -> typing.Optional[int]:
        """Get or set the Element ID of the master element defined in *ELEMENT_GENERALIZED_SOLID..
        """ # nopep8
        return self._cards[0].get_value("eidgs")

    @eidgs.setter
    def eidgs(self, value: int) -> None:
        """Set the eidgs property."""
        self._cards[0].set_value("eidgs", value)

    @property
    def ngp(self) -> typing.Optional[int]:
        """Get or set the Number of integration points of the master element.
        """ # nopep8
        return self._cards[0].get_value("ngp")

    @ngp.setter
    def ngp(self, value: int) -> None:
        """Set the ngp property."""
        self._cards[0].set_value("ngp", value)

    @property
    def ip1(self) -> typing.Optional[int]:
        """Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID..
        """ # nopep8
        return self._cards[1].get_value("ip1")

    @ip1.setter
    def ip1(self, value: int) -> None:
        """Set the ip1 property."""
        self._cards[1].set_value("ip1", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the Interpolation weight of integration point i.
        """ # nopep8
        return self._cards[1].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[1].set_value("w1", value)

    @property
    def ip2(self) -> typing.Optional[int]:
        """Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID.
        """ # nopep8
        return self._cards[1].get_value("ip2")

    @ip2.setter
    def ip2(self, value: int) -> None:
        """Set the ip2 property."""
        self._cards[1].set_value("ip2", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the Interpolation weight of integration point i.
        """ # nopep8
        return self._cards[1].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[1].set_value("w2", value)

    @property
    def ip3(self) -> typing.Optional[int]:
        """Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
        """ # nopep8
        return self._cards[1].get_value("ip3")

    @ip3.setter
    def ip3(self, value: int) -> None:
        """Set the ip3 property."""
        self._cards[1].set_value("ip3", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the Interpolation weight of integration point i.
        """ # nopep8
        return self._cards[1].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[1].set_value("w3", value)

    @property
    def ip4(self) -> typing.Optional[int]:
        """Get or set the Integration point number (1 to NGP) in the order how they were defined in *DEFINE_ELEMENT_GENERALIZED_SOLID
        """ # nopep8
        return self._cards[1].get_value("ip4")

    @ip4.setter
    def ip4(self, value: int) -> None:
        """Set the ip4 property."""
        self._cards[1].set_value("ip4", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the Interpolation weight of integration point i.
        """ # nopep8
        return self._cards[1].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[1].set_value("w4", value)

