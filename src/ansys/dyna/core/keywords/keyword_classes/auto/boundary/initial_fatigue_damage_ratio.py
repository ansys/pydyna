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

"""Module providing the InitialFatigueDamageRatio class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALFATIGUEDAMAGERATIO_CARD0 = (
    FieldSchema("pid/sid", int, 0, 10, None),
    FieldSchema("ptyp", int, 10, 10, 0),
    FieldSchema("dratio", float, 20, 10, None),
)

class InitialFatigueDamageRatio(KeywordBase):
    """DYNA INITIAL_FATIGUE_DAMAGE_RATIO keyword"""

    keyword = "INITIAL"
    subkeyword = "FATIGUE_DAMAGE_RATIO"

    def __init__(self, **kwargs):
        """Initialize the InitialFatigueDamageRatio class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALFATIGUEDAMAGERATIO_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid_sid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID for which the initial damage ratio is defined.
        """ # nopep8
        return self._cards[0].get_value("pid/sid")

    @pid_sid.setter
    def pid_sid(self, value: int) -> None:
        """Set the pid_sid property."""
        self._cards[0].set_value("pid/sid", value)

    @property
    def ptyp(self) -> int:
        """Get or set the Type of PID/PSID:
        EQ.0: part ID
        EQ.1: part set ID.
        """ # nopep8
        return self._cards[0].get_value("ptyp")

    @ptyp.setter
    def ptyp(self, value: int) -> None:
        """Set the ptyp property."""
        if value not in [0, 1, None]:
            raise Exception("""ptyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ptyp", value)

    @property
    def dratio(self) -> typing.Optional[float]:
        """Get or set the Initial damage ratio.
        """ # nopep8
        return self._cards[0].get_value("dratio")

    @dratio.setter
    def dratio(self, value: float) -> None:
        """Set the dratio property."""
        self._cards[0].set_value("dratio", value)

