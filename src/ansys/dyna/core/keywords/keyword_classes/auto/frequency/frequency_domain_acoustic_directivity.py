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

"""Module providing the FrequencyDomainAcousticDirectivity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FREQUENCYDOMAINACOUSTICDIRECTIVITY_CARD0 = (
    FieldSchema("center", int, 0, 10, None),
    FieldSchema("r", float, 10, 10, None),
    FieldSchema("np", int, 20, 10, None),
    FieldSchema("norm", int, 30, 10, None),
    FieldSchema("angle", float, 40, 10, None),
    FieldSchema("x0", float, 50, 10, None),
    FieldSchema("y0", float, 60, 10, None),
    FieldSchema("z0", float, 70, 10, None),
)

class FrequencyDomainAcousticDirectivity(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_DIRECTIVITY keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_DIRECTIVITY"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainAcousticDirectivity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICDIRECTIVITY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def center(self) -> typing.Optional[int]:
        """Get or set the Flag for defining the center point for the circle:
        EQ.1: Mass center of the original structure
        EQ.2: Geometry center of the original structure
        EQ.3: Defined by input variables X,Y,Z
        """ # nopep8
        return self._cards[0].get_value("center")

    @center.setter
    def center(self, value: int) -> None:
        """Set the center property."""
        self._cards[0].set_value("center", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the circle.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def np(self) -> typing.Optional[int]:
        """Get or set the Number of points on the circle.
        """ # nopep8
        return self._cards[0].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        """Set the np property."""
        self._cards[0].set_value("np", value)

    @property
    def norm(self) -> typing.Optional[int]:
        """Get or set the Norm direction of the circle.
        EQ.1: x - direction
        EQ.2: y - direction
        EQ.3: z - direction
        """ # nopep8
        return self._cards[0].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        """Set the norm property."""
        self._cards[0].set_value("norm", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Angle for the first point on the circle.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[0].set_value("angle", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[0].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[0].set_value("z0", value)

