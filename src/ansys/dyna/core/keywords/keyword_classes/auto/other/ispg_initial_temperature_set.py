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

"""Module providing the IspgInitialTemperatureSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGINITIALTEMPERATURESET_CARD0 = (
    FieldSchema("fp", int, 0, 10, None),
    FieldSchema("fptype", int, 10, 10, 0),
    FieldSchema("temp", float, 20, 10, None),
)

class IspgInitialTemperatureSet(KeywordBase):
    """DYNA ISPG_INITIAL_TEMPERATURE_SET keyword"""

    keyword = "ISPG"
    subkeyword = "INITIAL_TEMPERATURE_SET"

    def __init__(self, **kwargs):
        """Initialize the IspgInitialTemperatureSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGINITIALTEMPERATURESET_CARD0,
                **kwargs,
            ),
        ]
    @property
    def fp(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the fluid particles
        """ # nopep8
        return self._cards[0].get_value("fp")

    @fp.setter
    def fp(self, value: int) -> None:
        """Set the fp property."""
        self._cards[0].set_value("fp", value)

    @property
    def fptype(self) -> int:
        """Get or set the Type for FP:
        EQ.0: Part set ID
        EQ.1: Part ID
        """ # nopep8
        return self._cards[0].get_value("fptype")

    @fptype.setter
    def fptype(self, value: int) -> None:
        """Set the fptype property."""
        if value not in [0, 1, None]:
            raise Exception("""fptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fptype", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Temperature of the fluid particles
        """ # nopep8
        return self._cards[0].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        """Set the temp property."""
        self._cards[0].set_value("temp", value)

