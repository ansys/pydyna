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

"""Module providing the ConstrainedMultipleGlobal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDMULTIPLEGLOBAL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
)

_CONSTRAINEDMULTIPLEGLOBAL_CARD1 = (
    FieldSchema("nmp", int, 0, 10, None),
)

_CONSTRAINEDMULTIPLEGLOBAL_CARD2 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("dir", int, 10, 10, 1),
    FieldSchema("coef", float, 20, 10, None),
)

class ConstrainedMultipleGlobal(KeywordBase):
    """DYNA CONSTRAINED_MULTIPLE_GLOBAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "MULTIPLE_GLOBAL"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedMultipleGlobal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDMULTIPLEGLOBAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDMULTIPLEGLOBAL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDMULTIPLEGLOBAL_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Constraint set identification. All constraint sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def nmp(self) -> typing.Optional[int]:
        """Get or set the Number of nodes to be constrained mutually.
        """ # nopep8
        return self._cards[1].get_value("nmp")

    @nmp.setter
    def nmp(self, value: int) -> None:
        """Set the nmp property."""
        self._cards[1].set_value("nmp", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Nodal ID.
        """ # nopep8
        return self._cards[2].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[2].set_value("nid", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction in three-dimensional space to be constrained
        EQ.1: x direction
        EQ.2: y direction
        EQ.3: z direction.
        """ # nopep8
        return self._cards[2].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {1,2,3}.""")
        self._cards[2].set_value("dir", value)

    @property
    def coef(self) -> typing.Optional[float]:
        """Get or set the Coefficient ¦Ánid in constraint equation.
        """ # nopep8
        return self._cards[2].get_value("coef")

    @coef.setter
    def coef(self, value: float) -> None:
        """Set the coef property."""
        self._cards[2].set_value("coef", value)

