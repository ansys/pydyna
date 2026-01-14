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

"""Module providing the ControlCheckShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCHECKSHELL_CARD0 = (
    FieldSchema("pid", int, 0, 10, 0),
    FieldSchema("ifauto", int, 10, 10, 0),
    FieldSchema("convex", int, 20, 10, 1),
    FieldSchema("adpt", int, 30, 10, 1),
    FieldSchema("aratio", float, 40, 10, 0.25),
    FieldSchema("angke", float, 50, 10, 150.0),
    FieldSchema("smin", float, 60, 10, 0.0),
)

class ControlCheckShell(KeywordBase):
    """DYNA CONTROL_CHECK_SHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "CHECK_SHELL"

    def __init__(self, **kwargs):
        """Initialize the ControlCheckShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCHECKSHELL_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> int:
        """Get or set the Part ID to be checked:
        EQ.0: Do not check
        GT.0: Part ID
        LT.0: Part set ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ifauto(self) -> int:
        """Get or set the Flag to automatically correct bad elements:
        EQ.0: Write warning message only
        EQ.1 Fix bad element, write message.
        """ # nopep8
        return self._cards[0].get_value("ifauto")

    @ifauto.setter
    def ifauto(self, value: int) -> None:
        """Set the ifauto property."""
        if value not in [0, 1, None]:
            raise Exception("""ifauto must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ifauto", value)

    @property
    def convex(self) -> int:
        """Get or set the Check element convexity (internal angles less than 180 degrees)
        EQ.0: Do not check
        EQ.1: Check.
        """ # nopep8
        return self._cards[0].get_value("convex")

    @convex.setter
    def convex(self, value: int) -> None:
        """Set the convex property."""
        if value not in [1, 0, None]:
            raise Exception("""convex must be `None` or one of {1,0}.""")
        self._cards[0].set_value("convex", value)

    @property
    def adpt(self) -> int:
        """Get or set the Check adaptive constraints
        EQ.0: Do not check
        EQ.1: Check
        """ # nopep8
        return self._cards[0].get_value("adpt")

    @adpt.setter
    def adpt(self, value: int) -> None:
        """Set the adpt property."""
        if value not in [1, 0, None]:
            raise Exception("""adpt must be `None` or one of {1,0}.""")
        self._cards[0].set_value("adpt", value)

    @property
    def aratio(self) -> float:
        """Get or set the Minimum allowable aspect ratio. Elements which do not meet minimum aspect ratio test will be treated according to IFAUTO above
        """ # nopep8
        return self._cards[0].get_value("aratio")

    @aratio.setter
    def aratio(self, value: float) -> None:
        """Set the aratio property."""
        self._cards[0].set_value("aratio", value)

    @property
    def angke(self) -> float:
        """Get or set the Maximum allowable internal angle. Elements which fail this test will be treated according to IFAUTO above.
        """ # nopep8
        return self._cards[0].get_value("angke")

    @angke.setter
    def angke(self, value: float) -> None:
        """Set the angke property."""
        self._cards[0].set_value("angke", value)

    @property
    def smin(self) -> float:
        """Get or set the Minimum element size. Elements which fail this test will be treated according to IFAUTO above
        """ # nopep8
        return self._cards[0].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        """Set the smin property."""
        self._cards[0].set_value("smin", value)

