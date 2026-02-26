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

"""Module providing the FatigueLoadstep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FATIGUELOADSTEP_CARD0 = (
    FieldSchema("tstart", float, 0, 10, None),
    FieldSchema("tend", float, 10, 10, None),
    FieldSchema("texpos", float, 30, 10, 0.0),
)

class FatigueLoadstep(KeywordBase):
    """DYNA FATIGUE_LOADSTEP keyword"""

    keyword = "FATIGUE"
    subkeyword = "LOADSTEP"

    def __init__(self, **kwargs):
        """Initialize the FatigueLoadstep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FATIGUELOADSTEP_CARD0,
                **kwargs,
            ),        ]
    @property
    def tstart(self) -> typing.Optional[float]:
        """Get or set the Start time of current load step
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        """Set the tstart property."""
        self._cards[0].set_value("tstart", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the End time of current load step
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[0].set_value("tend", value)

    @property
    def texpos(self) -> float:
        """Get or set the Exposure time of current load step
        EQ.0.0:	set to TEND-TSTART (default).
        """ # nopep8
        return self._cards[0].get_value("texpos")

    @texpos.setter
    def texpos(self, value: float) -> None:
        """Set the texpos property."""
        self._cards[0].set_value("texpos", value)

