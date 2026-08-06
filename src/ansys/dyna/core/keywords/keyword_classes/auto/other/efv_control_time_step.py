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

"""Module providing the EfvControlTimeStep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCONTROLTIMESTEP_CARD0 = (
    FieldSchema("dltmin", float, 0, 10, None),
    FieldSchema("dltmax", float, 10, 10, 100000000.0),
    FieldSchema("dltini", float, 20, 10, 10000000.0),
    FieldSchema("dtfrac", float, 30, 10, 0.66667),
    FieldSchema("chrzone", int, 40, 10, 1),
)

class EfvControlTimeStep(KeywordBase):
    """DYNA EFV_CONTROL_TIME_STEP keyword"""

    keyword = "EFV"
    subkeyword = "CONTROL_TIME_STEP"

    def __init__(self, **kwargs):
        """Initialize the EfvControlTimeStep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCONTROLTIMESTEP_CARD0,
                **kwargs,
            ),
        ]
    @property
    def dltmin(self) -> typing.Optional[float]:
        """Get or set the Minimum time step below which the run is stopped
        """ # nopep8
        return self._cards[0].get_value("dltmin")

    @dltmin.setter
    def dltmin(self, value: float) -> None:
        """Set the dltmin property."""
        self._cards[0].set_value("dltmin", value)

    @property
    def dltmax(self) -> float:
        """Get or set the Maximum time step:
        LT.0: | DLTMAX | is the ID of a * DEFINE_CURVE keyword that defines a maximum time step dependent of time.
        GT.0 : DLTMAX is the maximum time step.
        """ # nopep8
        return self._cards[0].get_value("dltmax")

    @dltmax.setter
    def dltmax(self, value: float) -> None:
        """Set the dltmax property."""
        self._cards[0].set_value("dltmax", value)

    @property
    def dltini(self) -> float:
        """Get or set the Initial time step
        """ # nopep8
        return self._cards[0].get_value("dltini")

    @dltini.setter
    def dltini(self, value: float) -> None:
        """Set the dltini property."""
        self._cards[0].set_value("dltini", value)

    @property
    def dtfrac(self) -> float:
        """Get or set the Time step scale factor (<=2/3)
        """ # nopep8
        return self._cards[0].get_value("dtfrac")

    @dtfrac.setter
    def dtfrac(self, value: float) -> None:
        """Set the dtfrac property."""
        self._cards[0].set_value("dtfrac", value)

    @property
    def chrzone(self) -> int:
        """Get or set the Method of calculating the characteristic length for the time step based on:
        EQ.1: Element diagonals(in 2D, the element area is divided by the largest element diagonal)
        EQ.2: The smallest length between opposing faces of a given element
        EQ.3: The smallest length between faces of a given element
        """ # nopep8
        return self._cards[0].get_value("chrzone")

    @chrzone.setter
    def chrzone(self, value: int) -> None:
        """Set the chrzone property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""chrzone must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("chrzone", value)

