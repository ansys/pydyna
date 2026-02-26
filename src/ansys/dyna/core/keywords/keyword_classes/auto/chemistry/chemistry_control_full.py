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

"""Module providing the ChemistryControlFull class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CHEMISTRYCONTROLFULL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("errlim", float, 10, 10, None),
    FieldSchema("rhomin", float, 20, 10, None),
    FieldSchema("tmin", float, 30, 10, None),
)

class ChemistryControlFull(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_FULL keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_FULL"

    def __init__(self, **kwargs):
        """Initialize the ChemistryControlFull class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYCONTROLFULL_CARD0,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this full chemistry calculation.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def errlim(self) -> typing.Optional[float]:
        """Get or set the Error tolerance for the full chemistry calculation.
        """ # nopep8
        return self._cards[0].get_value("errlim")

    @errlim.setter
    def errlim(self, value: float) -> None:
        """Set the errlim property."""
        self._cards[0].set_value("errlim", value)

    @property
    def rhomin(self) -> typing.Optional[float]:
        """Get or set the Minimum fluid density above which chemical reactions are computed.
        """ # nopep8
        return self._cards[0].get_value("rhomin")

    @rhomin.setter
    def rhomin(self, value: float) -> None:
        """Set the rhomin property."""
        self._cards[0].set_value("rhomin", value)

    @property
    def tmin(self) -> typing.Optional[float]:
        """Get or set the Minimum temperature above which chemical reactions are computed
        """ # nopep8
        return self._cards[0].get_value("tmin")

    @tmin.setter
    def tmin(self, value: float) -> None:
        """Set the tmin property."""
        self._cards[0].set_value("tmin", value)

