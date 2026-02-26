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

"""Module providing the DualceseReactionRatePDepend class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEREACTIONRATEPDEPEND_CARD0 = (
    FieldSchema("react_id", int, 0, 10, None),
    FieldSchema("sigma", float, 10, 10, None),
    FieldSchema("nu", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
)

class DualceseReactionRatePDepend(KeywordBase):
    """DYNA DUALCESE_REACTION_RATE_P_DEPEND keyword"""

    keyword = "DUALCESE"
    subkeyword = "REACTION_RATE_P_DEPEND"

    def __init__(self, **kwargs):
        """Initialize the DualceseReactionRatePDepend class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEREACTIONRATEPDEPEND_CARD0,
                **kwargs,
            ),        ]
    @property
    def react_id(self) -> typing.Optional[int]:
        """Get or set the ID of reaction rate law
        """ # nopep8
        return self._cards[0].get_value("react_id")

    @react_id.setter
    def react_id(self, value: int) -> None:
        """Set the react_id property."""
        self._cards[0].set_value("react_id", value)

    @property
    def sigma(self) -> typing.Optional[float]:
        """Get or set the A positive constant
        """ # nopep8
        return self._cards[0].get_value("sigma")

    @sigma.setter
    def sigma(self, value: float) -> None:
        """Set the sigma property."""
        self._cards[0].set_value("sigma", value)

    @property
    def nu(self) -> typing.Optional[float]:
        """Get or set the A positive constant
        """ # nopep8
        return self._cards[0].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        """Set the nu property."""
        self._cards[0].set_value("nu", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the A positive constant
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

