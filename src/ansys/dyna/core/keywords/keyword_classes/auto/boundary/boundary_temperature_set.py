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

"""Module providing the BoundaryTemperatureSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYTEMPERATURESET_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("cmult", float, 20, 10, 1.0),
    FieldSchema("loc", int, 30, 10, 0),
    FieldSchema("tdeath", float, 40, 10, 1e+20),
    FieldSchema("tbirth", float, 50, 10, 0.0),
)

class BoundaryTemperatureSet(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_SET"

    def __init__(self, **kwargs):
        """Initialize the BoundaryTemperatureSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYTEMPERATURESET_CARD0,
                **kwargs,
            ),        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID for temperature versus time:
        EQ.0: use the constant multiplier value given below by CMULT (default).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def cmult(self) -> float:
        """Get or set the Curve multiplier for temperature.
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        """Set the cmult property."""
        self._cards[0].set_value("cmult", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
        EQ.-1: lower surface of thermal shell element,
        EQ. 1: upper surface of thermal shell element
        """ # nopep8
        return self._cards[0].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        """Set the loc property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""loc must be `None` or one of {0,-1,1}.""")
        self._cards[0].set_value("loc", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Deactivation time for temperature boundary condition. At this point in time the temperature constraint is removed.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Activation time for temperature boundary condition. Before this point in time the temperature constraint is ignored
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

