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

"""Module providing the IcfdInitialWatervapor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDINITIALWATERVAPOR_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_ICFDINITIALWATERVAPOR_CARD1 = (
    FieldSchema("mfrac", float, 0, 10, None),
    FieldSchema("mfracu", int, 10, 10, None),
)

class IcfdInitialWatervapor(KeywordBase):
    """DYNA ICFD_INITIAL_WATERVAPOR keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_WATERVAPOR"

    def __init__(self, **kwargs):
        """Initialize the IcfdInitialWatervapor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALWATERVAPOR_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALWATERVAPOR_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part Volume ID with the water vapor mass fraction condition.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def mfrac(self) -> typing.Optional[float]:
        """Get or set the Water vapor mass fraction value. If MFRACU = 0, this value is the mass of water vapor in kg divided by the mass of dry air in kg. If MFRACU = 1, this value is the relative humidity and is in the range of [0,1].
        """ # nopep8
        return self._cards[1].get_value("mfrac")

    @mfrac.setter
    def mfrac(self, value: float) -> None:
        """Set the mfrac property."""
        self._cards[1].set_value("mfrac", value)

    @property
    def mfracu(self) -> typing.Optional[int]:
        """Get or set the Water vapor mass fraction type:
        EQ.0:	MFRAC is the mass of the water vapor in kg divided by the mass of the dry air in kg.
        EQ.1 : MFRAC is the relative humidity with a range of[0,1].
        """ # nopep8
        return self._cards[1].get_value("mfracu")

    @mfracu.setter
    def mfracu(self, value: int) -> None:
        """Set the mfracu property."""
        self._cards[1].set_value("mfracu", value)

