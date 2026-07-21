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

"""Module providing the BoundarySphHeatConvection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYSPHHEATCONVECTION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtype", int, 10, 10, 0),
)

_BOUNDARYSPHHEATCONVECTION_CARD1 = (
    FieldSchema("hlcid", int, 0, 10, None),
    FieldSchema("hmult", float, 10, 10, 0.0),
    FieldSchema("tlcid", int, 20, 10, None),
    FieldSchema("tmult", float, 30, 10, None),
)

class BoundarySphHeatConvection(KeywordBase):
    """DYNA BOUNDARY_SPH_HEAT_CONVECTION keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_HEAT_CONVECTION"

    def __init__(self, **kwargs):
        """Initialize the BoundarySphHeatConvection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHHEATCONVECTION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHHEATCONVECTION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the ID type:
        EQ.0: Part ID(see *PART)
        EQ.1: Part set ID(see *SET_PART)
        """ # nopep8
        return self._cards[0].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        """Set the idtype property."""
        if value not in [0, 1, None]:
            raise Exception("""idtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idtype", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the heat convection coefficient:
        LT.0: The heat convection coefficient depends on film temperature.It is calculated as the average between the ambient and particle temperatures.
        EQ.0: The heat convection coefficient is constant and defined with HMULT.
        GT.0: The heat convection coefficient depends on time.
        """ # nopep8
        return self._cards[1].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        """Set the hlcid property."""
        self._cards[1].set_value("hlcid", value)

    @property
    def hmult(self) -> float:
        """Get or set the Scale factor on load curve |HLCID| if HLCID ? 0 or constant heat convection coefficient if HLCID = 0
        """ # nopep8
        return self._cards[1].get_value("hmult")

    @hmult.setter
    def hmult(self, value: float) -> None:
        """Set the hmult property."""
        self._cards[1].set_value("hmult", value)

    @property
    def tlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for ambient temperature:
        EQ.0: Temperature is constant.
        GT.0: Temperature depends on time
        """ # nopep8
        return self._cards[1].get_value("tlcid")

    @tlcid.setter
    def tlcid(self, value: int) -> None:
        """Set the tlcid property."""
        self._cards[1].set_value("tlcid", value)

    @property
    def tmult(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("tmult")

    @tmult.setter
    def tmult(self, value: float) -> None:
        """Set the tmult property."""
        self._cards[1].set_value("tmult", value)

