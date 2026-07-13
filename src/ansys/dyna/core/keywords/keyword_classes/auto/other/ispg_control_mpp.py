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

"""Module providing the IspgControlMpp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGCONTROLMPP_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("dx", float, 10, 10, 0.0),
    FieldSchema("dy", float, 20, 10, 0.0),
    FieldSchema("dz", float, 30, 10, 0.0),
)

class IspgControlMpp(KeywordBase):
    """DYNA ISPG_CONTROL_MPP keyword"""

    keyword = "ISPG"
    subkeyword = "CONTROL_MPP"

    def __init__(self, **kwargs):
        """Initialize the IspgControlMpp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLMPP_CARD0,
                **kwargs,
            ),
        ]
    @property
    def dx(self) -> float:
        """Get or set the Size of the box in the global x directions. The box is used to detect the adjacent structural segments used to define the coupling between the structure and ISPG fluids. If zero is given, 10.0xD(avg,max) is used. D(avg,max) is the maximum value of the original averaged nodal distance for all ISPG parts
        """ # nopep8
        return self._cards[0].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        """Set the dx property."""
        self._cards[0].set_value("dx", value)

    @property
    def dy(self) -> float:
        """Get or set the Size of the box in the global y directions. The box is used to detect the adjacent structural segments used to define the coupling between the structure and ISPG fluids. If zero is given, 10.0xD(avg,max) is used. D(avg,max) is the maximum value of the original averaged nodal distance for all ISPG parts
        """ # nopep8
        return self._cards[0].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        """Set the dy property."""
        self._cards[0].set_value("dy", value)

    @property
    def dz(self) -> float:
        """Get or set the Size of the box in the global z directions. The box is used to detect the adjacent structural segments used to define the coupling between the structure and ISPG fluids. If zero is given, 10.0xD(avg,max) is used. D(avg,max) is the maximum value of the original averaged nodal distance for all ISPG parts
        """ # nopep8
        return self._cards[0].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        """Set the dz property."""
        self._cards[0].set_value("dz", value)

