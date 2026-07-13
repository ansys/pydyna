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

"""Module providing the IspgLoadGravity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGLOADGRAVITY_CARD0 = (
    FieldSchema("idir", int, 0, 10, None),
    FieldSchema("grav", float, 10, 10, None),
)

class IspgLoadGravity(KeywordBase):
    """DYNA ISPG_LOAD_GRAVITY keyword"""

    keyword = "ISPG"
    subkeyword = "LOAD_GRAVITY"

    def __init__(self, **kwargs):
        """Initialize the IspgLoadGravity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGLOADGRAVITY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def idir(self) -> typing.Optional[int]:
        """Get or set the Global axis direction of the gravity load:
        EQ.1: Global X - axis
        EQ.2: Global Y - axis
        EQ.3: Global Z - axis
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        self._cards[0].set_value("idir", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Value of the gravity load
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[0].set_value("grav", value)

