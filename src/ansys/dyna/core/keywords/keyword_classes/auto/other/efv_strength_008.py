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

"""Module providing the EfvStrength008 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTH008_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("sigy", float, 20, 10, None),
    FieldSchema("b", float, 30, 10, None),
    FieldSchema("n", float, 40, 10, None),
    FieldSchema("d", float, 50, 10, None),
    FieldSchema("q", float, 60, 10, None),
    FieldSchema("stcor", int, 70, 10, 2),
)

class EfvStrength008(KeywordBase):
    """DYNA EFV_STRENGTH_008 keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_008"

    def __init__(self, **kwargs):
        """Initialize the EfvStrength008 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH008_CARD0,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, G
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Hardening constant
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening exponen
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Strain rate coefficient
        """ # nopep8
        return self._cards[0].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[0].set_value("d", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Strain rate exponent.
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[0].set_value("q", value)

    @property
    def stcor(self) -> int:
        """Get or set the Strain rate correction flag (see Remark 2):
        EQ.1: No correction
        EQ.2: First order(default)
        EQ.3: Implicit
        """ # nopep8
        return self._cards[0].get_value("stcor")

    @stcor.setter
    def stcor(self, value: int) -> None:
        """Set the stcor property."""
        if value not in [2, 1, 3, None]:
            raise Exception("""stcor must be `None` or one of {2,1,3}.""")
        self._cards[0].set_value("stcor", value)

