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

"""Module providing the ControlRefineMppDistribution class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLREFINEMPPDISTRIBUTION_CARD0 = (
    FieldSchema("id", int, 0, 10, 0),
    FieldSchema("dx", float, 10, 10, 0.0),
    FieldSchema("dy", float, 20, 10, 0.0),
    FieldSchema("dz", float, 30, 10, 0.0),
    FieldSchema("ex", float, 40, 10, 1.0),
    FieldSchema("ey", float, 50, 10, 1.0),
    FieldSchema("ez", float, 60, 10, 1.0),
)

class ControlRefineMppDistribution(KeywordBase):
    """DYNA CONTROL_REFINE_MPP_DISTRIBUTION keyword"""

    keyword = "CONTROL"
    subkeyword = "REFINE_MPP_DISTRIBUTION"

    def __init__(self, **kwargs):
        """Initialize the ControlRefineMppDistribution class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLREFINEMPPDISTRIBUTION_CARD0,
                **kwargs,
            ),        ]
    @property
    def id(self) -> int:
        """Get or set the ID = -NTOTRF in *CONTROL_REFINE_ALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def dx(self) -> float:
        """Get or set the Dimensionless x-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        """Set the dx property."""
        self._cards[0].set_value("dx", value)

    @property
    def dy(self) -> float:
        """Get or set the Dimensionless y-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        """Set the dy property."""
        self._cards[0].set_value("dy", value)

    @property
    def dz(self) -> float:
        """Get or set the Dimensionless z-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        """Set the dz property."""
        self._cards[0].set_value("dz", value)

    @property
    def ex(self) -> float:
        """Get or set the Dimensionless x-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ex")

    @ex.setter
    def ex(self, value: float) -> None:
        """Set the ex property."""
        self._cards[0].set_value("ex", value)

    @property
    def ey(self) -> float:
        """Get or set the Dimensionless y-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ey")

    @ey.setter
    def ey(self, value: float) -> None:
        """Set the ey property."""
        self._cards[0].set_value("ey", value)

    @property
    def ez(self) -> float:
        """Get or set the Dimensionless z-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ez")

    @ez.setter
    def ez(self, value: float) -> None:
        """Set the ez property."""
        self._cards[0].set_value("ez", value)

