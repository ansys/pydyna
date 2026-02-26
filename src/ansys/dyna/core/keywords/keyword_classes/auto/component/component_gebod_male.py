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

"""Module providing the ComponentGebodMale class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_COMPONENTGEBODMALE_CARD0 = (
    FieldSchema("did", int, 0, 10, None),
    FieldSchema("units", int, 10, 10, 1),
    FieldSchema("size", float, 20, 10, None),
)

_COMPONENTGEBODMALE_CARD1 = (
    FieldSchema("vx", float, 0, 10, 0.0),
    FieldSchema("vy", float, 10, 10, 0.0),
    FieldSchema("vz", float, 20, 10, 0.0),
    FieldSchema("gx", float, 30, 10, 0.0),
    FieldSchema("gy", float, 40, 10, 0.0),
    FieldSchema("gz", float, 50, 10, 0.0),
)

class ComponentGebodMale(KeywordBase):
    """DYNA COMPONENT_GEBOD_MALE keyword"""

    keyword = "COMPONENT"
    subkeyword = "GEBOD_MALE"

    def __init__(self, **kwargs):
        """Initialize the ComponentGebodMale class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _COMPONENTGEBODMALE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _COMPONENTGEBODMALE_CARD1,
                **kwargs,
            ),        ]
    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Dummy ID. A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        """Set the did property."""
        self._cards[0].set_value("did", value)

    @property
    def units(self) -> int:
        """Get or set the System of units used in the finite element model.
        EQ.1: lbf*sec^2/in-inch-sec,
        EQ.2: kg-meter-sec,
        EQ.3: kgf*sec^2/mm-mm-sec,
        EQ.4: metric ton-mm-sec,
        EQ.5: kg-mm-msec.
        """ # nopep8
        return self._cards[0].get_value("units")

    @units.setter
    def units(self, value: int) -> None:
        """Set the units property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""units must be `None` or one of {1,2,3,4,5}.""")
        self._cards[0].set_value("units", value)

    @property
    def size(self) -> typing.Optional[float]:
        """Get or set the Size of the dummy. This represents a combined height and weight percentile ranging from 0 to 100.
        """ # nopep8
        return self._cards[0].get_value("size")

    @size.setter
    def size(self, value: float) -> None:
        """Set the size property."""
        self._cards[0].set_value("size", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial velocity of the dummy in the global x-direction.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial velocity of the dummy in the global y-direction.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial velocity of the dummy in the global z-direction.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def gx(self) -> float:
        """Get or set the Global x-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gx")

    @gx.setter
    def gx(self, value: float) -> None:
        """Set the gx property."""
        self._cards[1].set_value("gx", value)

    @property
    def gy(self) -> float:
        """Get or set the Global y-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gy")

    @gy.setter
    def gy(self, value: float) -> None:
        """Set the gy property."""
        self._cards[1].set_value("gy", value)

    @property
    def gz(self) -> float:
        """Get or set the Global z-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gz")

    @gz.setter
    def gz(self, value: float) -> None:
        """Set the gz property."""
        self._cards[1].set_value("gz", value)

