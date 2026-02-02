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

"""Module providing the DatabasePowerSpectralDensity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEPOWERSPECTRALDENSITY_CARD0 = (
    FieldSchema("fbeg", float, 0, 10, 0.0),
    FieldSchema("fend", float, 10, 10, 0.0),
    FieldSchema("fintval", float, 20, 10, 1.0),
    FieldSchema("ftrunk", float, 30, 10, 1.1),
)

class DatabasePowerSpectralDensity(KeywordBase):
    """DYNA DATABASE_POWER_SPECTRAL_DENSITY keyword"""

    keyword = "DATABASE"
    subkeyword = "POWER_SPECTRAL_DENSITY"

    def __init__(self, **kwargs):
        """Initialize the DatabasePowerSpectralDensity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEPOWERSPECTRALDENSITY_CARD0,
                **kwargs,
            ),        ]
    @property
    def fbeg(self) -> float:
        """Get or set the Beginning frequency for PSD database output.
        """ # nopep8
        return self._cards[0].get_value("fbeg")

    @fbeg.setter
    def fbeg(self, value: float) -> None:
        """Set the fbeg property."""
        self._cards[0].set_value("fbeg", value)

    @property
    def fend(self) -> float:
        """Get or set the Ending frequency for PSD database output.
        """ # nopep8
        return self._cards[0].get_value("fend")

    @fend.setter
    def fend(self, value: float) -> None:
        """Set the fend property."""
        self._cards[0].set_value("fend", value)

    @property
    def fintval(self) -> float:
        """Get or set the Interval of frequencies for PSD database output.
        """ # nopep8
        return self._cards[0].get_value("fintval")

    @fintval.setter
    def fintval(self, value: float) -> None:
        """Set the fintval property."""
        self._cards[0].set_value("fintval", value)

    @property
    def ftrunk(self) -> float:
        """Get or set the If FBEG and FEND are not given, the ending frequency for PSD database output is FTRUNK*the highest resonance frequency. Output for higher frequencies is truncated.
        """ # nopep8
        return self._cards[0].get_value("ftrunk")

    @ftrunk.setter
    def ftrunk(self, value: float) -> None:
        """Set the ftrunk property."""
        self._cards[0].set_value("ftrunk", value)

