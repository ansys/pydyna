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

"""Module providing the IcfdDefineTransform class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDEFINETRANSFORM_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("trx", float, 10, 10, None),
    FieldSchema("try_", float, 20, 10, None, "try"),
    FieldSchema("trz", float, 30, 10, None),
    FieldSchema("sf", float, 40, 10, 1.0),
)

_ICFDDEFINETRANSFORM_CARD1 = (
    FieldSchema("orx", float, 0, 10, None),
    FieldSchema("ory", float, 10, 10, None),
    FieldSchema("orz", float, 20, 10, None),
    FieldSchema("nx", float, 30, 10, None),
    FieldSchema("ny", float, 40, 10, None),
    FieldSchema("nz", float, 50, 10, None),
    FieldSchema("angle", float, 60, 10, None),
)

class IcfdDefineTransform(KeywordBase):
    """DYNA ICFD_DEFINE_TRANSFORM keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_TRANSFORM"

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineTransform class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINETRANSFORM_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINETRANSFORM_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Surface PID on which to apply the transformation.
        EQ.0: Apply the transformation to the entire model.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def trx(self) -> typing.Optional[float]:
        """Get or set the Translation offset in the global X directions.
        """ # nopep8
        return self._cards[0].get_value("trx")

    @trx.setter
    def trx(self, value: float) -> None:
        """Set the trx property."""
        self._cards[0].set_value("trx", value)

    @property
    def try_(self) -> typing.Optional[float]:
        """Get or set the Translation offset in the global Y directions.
        """ # nopep8
        return self._cards[0].get_value("try_")

    @try_.setter
    def try_(self, value: float) -> None:
        """Set the try_ property."""
        self._cards[0].set_value("try_", value)

    @property
    def trz(self) -> typing.Optional[float]:
        """Get or set the Translation offset in the global Z directions.
        """ # nopep8
        return self._cards[0].get_value("trz")

    @trz.setter
    def trz(self, value: float) -> None:
        """Set the trz property."""
        self._cards[0].set_value("trz", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor to be applied on the X,Y and Z coordinates
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def orx(self) -> typing.Optional[float]:
        """Get or set the Origin point of the rotation.
        """ # nopep8
        return self._cards[1].get_value("orx")

    @orx.setter
    def orx(self, value: float) -> None:
        """Set the orx property."""
        self._cards[1].set_value("orx", value)

    @property
    def ory(self) -> typing.Optional[float]:
        """Get or set the Origin point of the rotation.
        """ # nopep8
        return self._cards[1].get_value("ory")

    @ory.setter
    def ory(self, value: float) -> None:
        """Set the ory property."""
        self._cards[1].set_value("ory", value)

    @property
    def orz(self) -> typing.Optional[float]:
        """Get or set the Origin point of the rotation.
        """ # nopep8
        return self._cards[1].get_value("orz")

    @orz.setter
    def orz(self, value: float) -> None:
        """Set the orz property."""
        self._cards[1].set_value("orz", value)

    @property
    def nx(self) -> typing.Optional[float]:
        """Get or set the Normal for the rotation
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[float]:
        """Get or set the Normal for the rotation
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[1].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[float]:
        """Get or set the Normal for the rotation
        """ # nopep8
        return self._cards[1].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[1].set_value("nz", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Angle of rotation (in degrees)
        """ # nopep8
        return self._cards[1].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[1].set_value("angle", value)

