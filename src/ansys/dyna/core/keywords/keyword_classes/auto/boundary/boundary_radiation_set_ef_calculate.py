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

"""Module providing the BoundaryRadiationSetEfCalculate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYRADIATIONSETEFCALCULATE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("nmat", int, 10, 10, None),
    FieldSchema("npht", int, 20, 10, 1),
    FieldSchema("errmax", int, 30, 10, 0),
)

class BoundaryRadiationSetEfCalculate(KeywordBase):
    """DYNA BOUNDARY_RADIATION_SET_EF_CALCULATE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_SET_EF_CALCULATE"

    def __init__(self, **kwargs):
        """Initialize the BoundaryRadiationSetEfCalculate class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONSETEFCALCULATE_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see also *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def nmat(self) -> typing.Optional[int]:
        """Get or set the NMAT specifies the material type for the portion of the boundary specified by SSID.  NMAT must be an exchange factor material ID. See the *EF_MATERIAL keyword.
        """ # nopep8
        return self._cards[0].get_value("nmat")

    @nmat.setter
    def nmat(self, value: int) -> None:
        """Set the nmat property."""
        self._cards[0].set_value("nmat", value)

    @property
    def npht(self) -> int:
        """Get or set the The segments specified by SSID will emit NPHT*NPHOTON photons. See the *EF_CONTROL keyword.
        """ # nopep8
        return self._cards[0].get_value("npht")

    @npht.setter
    def npht(self, value: int) -> None:
        """Set the npht property."""
        self._cards[0].set_value("npht", value)

    @property
    def errmax(self) -> int:
        """Get or set the the convergence error tolerance for the surface.
        """ # nopep8
        return self._cards[0].get_value("errmax")

    @errmax.setter
    def errmax(self, value: int) -> None:
        """Set the errmax property."""
        self._cards[0].set_value("errmax", value)

