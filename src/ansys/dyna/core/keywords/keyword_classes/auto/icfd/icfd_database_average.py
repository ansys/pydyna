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

"""Module providing the IcfdDatabaseAverage class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDATABASEAVERAGE_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
)

class IcfdDatabaseAverage(KeywordBase):
    """DYNA ICFD_DATABASE_AVERAGE keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_AVERAGE"

    def __init__(self, **kwargs):
        """Initialize the IcfdDatabaseAverage class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASEAVERAGE_CARD0,
                **kwargs,
            ),        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Over each DT time interval, an average of the different fluid variables will be calculated and then reset when moving to the next DT interval.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

