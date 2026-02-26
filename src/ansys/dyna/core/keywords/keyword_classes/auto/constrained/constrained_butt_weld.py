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

"""Module providing the ConstrainedButtWeld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDBUTTWELD_CARD0 = (
    FieldSchema("nsid1", int, 0, 10, None),
    FieldSchema("nsid2", int, 10, 10, None),
    FieldSchema("eppf", float, 20, 10, 0.0),
    FieldSchema("sigf", float, 30, 10, 1e+16),
    FieldSchema("beta", float, 40, 10, 1.0),
)

class ConstrainedButtWeld(KeywordBase):
    """DYNA CONSTRAINED_BUTT_WELD keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "BUTT_WELD"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedButtWeld class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBUTTWELD_CARD0,
                **kwargs,
            ),        ]
    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set ID for one side of the butt weld, See *SET_NODE_option.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        """Set the nsid1 property."""
        self._cards[0].set_value("nsid1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node set ID for the other side of the butt weld, See *SET_NODE_option.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        """Set the nsid2 property."""
        self._cards[0].set_value("nsid2", value)

    @property
    def eppf(self) -> float:
        """Get or set the Plastic Strain at failure
        """ # nopep8
        return self._cards[0].get_value("eppf")

    @eppf.setter
    def eppf(self, value: float) -> None:
        """Set the eppf property."""
        self._cards[0].set_value("eppf", value)

    @property
    def sigf(self) -> float:
        """Get or set the Stress at failure for brittle failure
        """ # nopep8
        return self._cards[0].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[0].set_value("sigf", value)

    @property
    def beta(self) -> float:
        """Get or set the Failure parameter for brittle failure
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

