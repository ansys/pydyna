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

"""Module providing the EosMurnaghan class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSMURNAGHAN_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("gamma", float, 10, 10, None),
    FieldSchema("k0", float, 20, 10, None),
    FieldSchema("v0", float, 30, 10, None),
)

class EosMurnaghan(KeywordBase):
    """DYNA EOS_MURNAGHAN keyword"""

    keyword = "EOS"
    subkeyword = "MURNAGHAN"

    def __init__(self, **kwargs):
        """Initialize the EosMurnaghan class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSMURNAGHAN_CARD0,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID, a unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def k0(self) -> typing.Optional[float]:
        """Get or set the Constants in the equation of state
        """ # nopep8
        return self._cards[0].get_value("k0")

    @k0.setter
    def k0(self, value: float) -> None:
        """Set the k0 property."""
        self._cards[0].set_value("k0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

