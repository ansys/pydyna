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

"""Module providing the BoundaryElementMethodControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYELEMENTMETHODCONTROL_CARD0 = (
    FieldSchema("lwake", int, 0, 10, 50),
    FieldSchema("dtbem", float, 10, 10, 0.0),
    FieldSchema("iupbem", int, 20, 10, 100),
    FieldSchema("farbem", float, 30, 10, 2.0),
)

class BoundaryElementMethodControl(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_CONTROL keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_CONTROL"

    def __init__(self, **kwargs):
        """Initialize the BoundaryElementMethodControl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODCONTROL_CARD0,
                **kwargs,
            ),        ]
    @property
    def lwake(self) -> int:
        """Get or set the Number of elements in the wake of lifting surfaces. Wakes must be defined for all lifting surfaces.
        """ # nopep8
        return self._cards[0].get_value("lwake")

    @lwake.setter
    def lwake(self, value: int) -> None:
        """Set the lwake property."""
        self._cards[0].set_value("lwake", value)

    @property
    def dtbem(self) -> float:
        """Get or set the Time increment between calls to the boundary element method. The fluid pressures computed during the previous call to the BEM will continue to be used for subsequent LS-DYNA iterations until a time increment of DTBEM has elapsed.
        """ # nopep8
        return self._cards[0].get_value("dtbem")

    @dtbem.setter
    def dtbem(self, value: float) -> None:
        """Set the dtbem property."""
        self._cards[0].set_value("dtbem", value)

    @property
    def iupbem(self) -> int:
        """Get or set the The number of times the BEM routines are called before the matrix of influence coefficients is recomputed and refactored.
        """ # nopep8
        return self._cards[0].get_value("iupbem")

    @iupbem.setter
    def iupbem(self, value: int) -> None:
        """Set the iupbem property."""
        self._cards[0].set_value("iupbem", value)

    @property
    def farbem(self) -> float:
        """Get or set the Nondimensional boundary between near-field and far-field calculation of influence coefficients.
        """ # nopep8
        return self._cards[0].get_value("farbem")

    @farbem.setter
    def farbem(self, value: float) -> None:
        """Set the farbem property."""
        self._cards[0].set_value("farbem", value)

