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

"""Module providing the IcfdControlTransient class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLTRANSIENT_CARD0 = (
    FieldSchema("tord", int, 0, 10, 0),
    FieldSchema("fsord", int, 10, 10, 0),
)

class IcfdControlTransient(KeywordBase):
    """DYNA ICFD_CONTROL_TRANSIENT keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TRANSIENT"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlTransient class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTRANSIENT_CARD0,
                **kwargs,
            ),        ]
    @property
    def tord(self) -> int:
        """Get or set the Time integration order :
        EQ.0:	Second order.
        EQ.1:	First order.
        """ # nopep8
        return self._cards[0].get_value("tord")

    @tord.setter
    def tord(self, value: int) -> None:
        """Set the tord property."""
        if value not in [0, 1, None]:
            raise Exception("""tord must be `None` or one of {0,1}.""")
        self._cards[0].set_value("tord", value)

    @property
    def fsord(self) -> int:
        """Get or set the Fractional step integration order :
        EQ.0:	Second order.
        EQ.1:	First order.
        """ # nopep8
        return self._cards[0].get_value("fsord")

    @fsord.setter
    def fsord(self, value: int) -> None:
        """Set the fsord property."""
        if value not in [0, 1, None]:
            raise Exception("""fsord must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fsord", value)

