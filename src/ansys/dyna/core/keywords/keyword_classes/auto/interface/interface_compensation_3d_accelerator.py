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

"""Module providing the InterfaceCompensation3DAccelerator class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INTERFACECOMPENSATION3DACCELERATOR_CARD0 = (
    FieldSchema("isteps", int, 0, 10, 0),
    FieldSchema("tolx", float, 10, 10, 0.5),
    FieldSchema("toly", float, 20, 10, 0.5),
    FieldSchema("tolz", float, 30, 10, 0.5),
    FieldSchema("option", int, 40, 10, 1),
)

class InterfaceCompensation3DAccelerator(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_3D_ACCELERATOR keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_3D_ACCELERATOR"

    def __init__(self, **kwargs):
        """Initialize the InterfaceCompensation3DAccelerator class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION3DACCELERATOR_CARD0,
                **kwargs,
            ),        ]
    @property
    def isteps(self) -> int:
        """Get or set the Steps in accelerated compensation procedure, see Remarks.
        """ # nopep8
        return self._cards[0].get_value("isteps")

    @isteps.setter
    def isteps(self, value: int) -> None:
        """Set the isteps property."""
        self._cards[0].set_value("isteps", value)

    @property
    def tolx(self) -> float:
        """Get or set the Part deviation tolerance between current blank and target blank shape in global x-direction.
        """ # nopep8
        return self._cards[0].get_value("tolx")

    @tolx.setter
    def tolx(self, value: float) -> None:
        """Set the tolx property."""
        self._cards[0].set_value("tolx", value)

    @property
    def toly(self) -> float:
        """Get or set the Part deviation tolerance between current blank and target blank	shape in global y-direction.
        """ # nopep8
        return self._cards[0].get_value("toly")

    @toly.setter
    def toly(self, value: float) -> None:
        """Set the toly property."""
        self._cards[0].set_value("toly", value)

    @property
    def tolz(self) -> float:
        """Get or set the Part deviation tolerance between current blank and target blank	shape in global z-direction.
        """ # nopep8
        return self._cards[0].get_value("tolz")

    @tolz.setter
    def tolz(self, value: float) -> None:
        """Set the tolz property."""
        self._cards[0].set_value("tolz", value)

    @property
    def option(self) -> int:
        """Get or set the Compensation acceleration method. Currently available only for method 1..
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: int) -> None:
        """Set the option property."""
        self._cards[0].set_value("option", value)

