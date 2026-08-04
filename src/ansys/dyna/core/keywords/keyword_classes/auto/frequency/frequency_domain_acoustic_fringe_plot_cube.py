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

"""Module providing the FrequencyDomainAcousticFringePlotCube class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FREQUENCYDOMAINACOUSTICFRINGEPLOTCUBE_CARD0 = (
    FieldSchema("len_x", float, 0, 10, None),
    FieldSchema("len_y", float, 10, 10, None),
    FieldSchema("len_z", float, 20, 10, None),
    FieldSchema("x", float, 30, 10, None),
    FieldSchema("y", float, 40, 10, None),
    FieldSchema("z", float, 50, 10, None),
)

_FREQUENCYDOMAINACOUSTICFRINGEPLOTCUBE_CARD1 = (
    FieldSchema("nelm_x", float, 0, 10, None),
    FieldSchema("nelm_y", float, 10, 10, None),
    FieldSchema("nelm_z", float, 20, 10, None),
)

class FrequencyDomainAcousticFringePlotCube(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_FRINGE_PLOT_CUBE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_FRINGE_PLOT_CUBE"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainAcousticFringePlotCube class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICFRINGEPLOTCUBE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICFRINGEPLOTCUBE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def len_x(self) -> typing.Optional[float]:
        """Get or set the Length of x-side of the cube
        """ # nopep8
        return self._cards[0].get_value("len_x")

    @len_x.setter
    def len_x(self, value: float) -> None:
        """Set the len_x property."""
        self._cards[0].set_value("len_x", value)

    @property
    def len_y(self) -> typing.Optional[float]:
        """Get or set the Length of y-side of the cube
        """ # nopep8
        return self._cards[0].get_value("len_y")

    @len_y.setter
    def len_y(self, value: float) -> None:
        """Set the len_y property."""
        self._cards[0].set_value("len_y", value)

    @property
    def len_z(self) -> typing.Optional[float]:
        """Get or set the Length of z-side of the cube
        """ # nopep8
        return self._cards[0].get_value("len_z")

    @len_z.setter
    def len_z(self, value: float) -> None:
        """Set the len_z property."""
        self._cards[0].set_value("len_z", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the corner of the cube with smallest nodal coordinates
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the corner of the cube with smallest nodal coordinates
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of the corner of the cube with smallest nodal coordinates
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def nelm_x(self) -> typing.Optional[float]:
        """Get or set the Number of elements on x-side of the cube
        """ # nopep8
        return self._cards[1].get_value("nelm_x")

    @nelm_x.setter
    def nelm_x(self, value: float) -> None:
        """Set the nelm_x property."""
        self._cards[1].set_value("nelm_x", value)

    @property
    def nelm_y(self) -> typing.Optional[float]:
        """Get or set the Number of elements on y-side of the cube
        """ # nopep8
        return self._cards[1].get_value("nelm_y")

    @nelm_y.setter
    def nelm_y(self, value: float) -> None:
        """Set the nelm_y property."""
        self._cards[1].set_value("nelm_y", value)

    @property
    def nelm_z(self) -> typing.Optional[float]:
        """Get or set the Number of elements on z-side of the cube
        """ # nopep8
        return self._cards[1].get_value("nelm_z")

    @nelm_z.setter
    def nelm_z(self, value: float) -> None:
        """Set the nelm_z property."""
        self._cards[1].set_value("nelm_z", value)

