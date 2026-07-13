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

"""Module providing the EfvFailureAddStochastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILUREADDSTOCHASTIC_CARD0 = (
    FieldSchema("stchid", int, 0, 10, None),
    FieldSchema("varia", float, 10, 10, None),
    FieldSchema("frac", float, 20, 10, None),
    FieldSchema("distyp", int, 30, 10, 0),
)

class EfvFailureAddStochastic(KeywordBase):
    """DYNA EFV_FAILURE_ADD_STOCHASTIC keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_ADD_STOCHASTIC"

    def __init__(self, **kwargs):
        """Initialize the EfvFailureAddStochastic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREADDSTOCHASTIC_CARD0,
                **kwargs,
            ),
        ]
    @property
    def stchid(self) -> typing.Optional[int]:
        """Get or set the Stochastic failure identification. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("stchid")

    @stchid.setter
    def stchid(self, value: int) -> None:
        """Set the stchid property."""
        self._cards[0].set_value("stchid", value)

    @property
    def varia(self) -> typing.Optional[float]:
        """Get or set the Stochastic variance, r. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("varia")

    @varia.setter
    def varia(self, value: float) -> None:
        """Set the varia property."""
        self._cards[0].set_value("varia", value)

    @property
    def frac(self) -> typing.Optional[float]:
        """Get or set the Minimum failure fraction
        """ # nopep8
        return self._cards[0].get_value("frac")

    @frac.setter
    def frac(self, value: float) -> None:
        """Set the frac property."""
        self._cards[0].set_value("frac", value)

    @property
    def distyp(self) -> int:
        """Get or set the Distribution type (see Remark 2):
        EQ.0: Fixed seed
        EQ.1: Random seed
        """ # nopep8
        return self._cards[0].get_value("distyp")

    @distyp.setter
    def distyp(self, value: int) -> None:
        """Set the distyp property."""
        if value not in [0, 1, None]:
            raise Exception("""distyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("distyp", value)

