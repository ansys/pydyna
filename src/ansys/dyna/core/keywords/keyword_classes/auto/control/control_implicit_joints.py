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

"""Module providing the ControlImplicitJoints class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITJOINTS_CARD0 = (
    FieldSchema("ispher", int, 0, 10, 1),
    FieldSchema("irevol", int, 10, 10, 1),
    FieldSchema("icylin", int, 20, 10, 1),
)

class ControlImplicitJoints(KeywordBase):
    """DYNA CONTROL_IMPLICIT_JOINTS keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_JOINTS"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitJoints class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITJOINTS_CARD0,
                **kwargs,
            ),        ]
    @property
    def ispher(self) -> int:
        """Get or set the Treatment of spherical joints
        EQ.1: use constraint method for all spherical joints (default)
        EQ.2: use penalty method for all spherical joints
        """ # nopep8
        return self._cards[0].get_value("ispher")

    @ispher.setter
    def ispher(self, value: int) -> None:
        """Set the ispher property."""
        if value not in [1, 2, None]:
            raise Exception("""ispher must be `None` or one of {1,2}.""")
        self._cards[0].set_value("ispher", value)

    @property
    def irevol(self) -> int:
        """Get or set the Treatment of revolute joints
        EQ.1: use constraint method for all revolute joints (default)
        EQ.2: use penalty method for all revolute joints
        """ # nopep8
        return self._cards[0].get_value("irevol")

    @irevol.setter
    def irevol(self, value: int) -> None:
        """Set the irevol property."""
        if value not in [1, 2, None]:
            raise Exception("""irevol must be `None` or one of {1,2}.""")
        self._cards[0].set_value("irevol", value)

    @property
    def icylin(self) -> int:
        """Get or set the Treatment of cylindrical joints.
        EQ.1: use constraint method for all cylindrical joints (default)
        EQ.2: use penalty method for all cylindrical joints
        """ # nopep8
        return self._cards[0].get_value("icylin")

    @icylin.setter
    def icylin(self, value: int) -> None:
        """Set the icylin property."""
        if value not in [1, 2, None]:
            raise Exception("""icylin must be `None` or one of {1,2}.""")
        self._cards[0].set_value("icylin", value)

