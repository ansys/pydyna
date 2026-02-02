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

"""Module providing the ControlAdaptiveCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLADAPTIVECURVE_CARD0 = (
    FieldSchema("idset", int, 0, 10, None),
    FieldSchema("itype", int, 10, 10, 1),
    FieldSchema("n", int, 20, 10, None),
    FieldSchema("smin", float, 30, 10, None),
    FieldSchema("itriop", int, 40, 10, 0),
)

class ControlAdaptiveCurve(KeywordBase):
    """DYNA CONTROL_ADAPTIVE_CURVE keyword"""

    keyword = "CONTROL"
    subkeyword = "ADAPTIVE_CURVE"

    def __init__(self, **kwargs):
        """Initialize the ControlAdaptiveCurve class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVECURVE_CARD0,
                **kwargs,
            ),        ]
    @property
    def idset(self) -> typing.Optional[int]:
        """Get or set the Set id
        """ # nopep8
        return self._cards[0].get_value("idset")

    @idset.setter
    def idset(self, value: int) -> None:
        """Set the idset property."""
        self._cards[0].set_value("idset", value)

    @property
    def itype(self) -> int:
        """Get or set the EQ.1: IDSET is shell set ID.
        EQ.2: IDSET is part set ID.
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [1, 2, None]:
            raise Exception("""itype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("itype", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Refinement option.
        1:EQ.1: Refine until there are no adaptive constraints remaining in the element mesh around the curve..
        GT.1: Refine no more than N levels
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def smin(self) -> typing.Optional[float]:
        """Get or set the if the elements is smaller than this value, it will not be refined.
        """ # nopep8
        return self._cards[0].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        """Set the smin property."""
        self._cards[0].set_value("smin", value)

    @property
    def itriop(self) -> int:
        """Get or set the Option to refine an enclosed area of a trim curve.
        EQ.0: Refine the elements along the trim curve
        EQ.1: Refine the elements along the trim curve and enclosed by the trim curve..
        """ # nopep8
        return self._cards[0].get_value("itriop")

    @itriop.setter
    def itriop(self, value: int) -> None:
        """Set the itriop property."""
        if value not in [0, 1, None]:
            raise Exception("""itriop must be `None` or one of {0,1}.""")
        self._cards[0].set_value("itriop", value)

