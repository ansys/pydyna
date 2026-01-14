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

"""Module providing the LoadGravityPart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADGRAVITYPART_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("dof", int, 10, 10, None),
    FieldSchema("lc", int, 20, 10, None),
    FieldSchema("accel", float, 30, 10, 0.0),
    FieldSchema("lcdr", int, 40, 10, None),
    FieldSchema("stga", int, 50, 10, 0),
    FieldSchema("stgr", int, 60, 10, 0),
)

class LoadGravityPart(KeywordBase):
    """DYNA LOAD_GRAVITY_PART keyword"""

    keyword = "LOAD"
    subkeyword = "GRAVITY_PART"

    def __init__(self, **kwargs):
        """Initialize the LoadGravityPart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADGRAVITYPART_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for application of gravity load
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def dof(self) -> typing.Optional[int]:
        """Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
        """ # nopep8
        return self._cards[0].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        self._cards[0].set_value("dof", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Load curve defining factor vs. time (or zero if STGA, STGR are defined)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        """Set the lc property."""
        self._cards[0].set_value("lc", value)

    @property
    def accel(self) -> float:
        """Get or set the Acceleration (will be multiplied by factor from curve
        """ # nopep8
        return self._cards[0].get_value("accel")

    @accel.setter
    def accel(self, value: float) -> None:
        """Set the accel property."""
        self._cards[0].set_value("accel", value)

    @property
    def lcdr(self) -> typing.Optional[int]:
        """Get or set the Load curve defining factor vs. time during dynamic relaxation
        """ # nopep8
        return self._cards[0].get_value("lcdr")

    @lcdr.setter
    def lcdr(self, value: int) -> None:
        """Set the lcdr property."""
        self._cards[0].set_value("lcdr", value)

    @property
    def stga(self) -> int:
        """Get or set the Construction stage at which part is added (optional)
        """ # nopep8
        return self._cards[0].get_value("stga")

    @stga.setter
    def stga(self, value: int) -> None:
        """Set the stga property."""
        self._cards[0].set_value("stga", value)

    @property
    def stgr(self) -> int:
        """Get or set the Construction stage at which part is removed (optional)
        """ # nopep8
        return self._cards[0].get_value("stgr")

    @stgr.setter
    def stgr(self, value: int) -> None:
        """Set the stgr property."""
        self._cards[0].set_value("stgr", value)

