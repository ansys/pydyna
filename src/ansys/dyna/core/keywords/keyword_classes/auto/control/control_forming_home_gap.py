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

"""Module providing the ControlFormingHomeGap class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGHOMEGAP_CARD0 = (
    FieldSchema("psidu", int, 0, 10, None),
    FieldSchema("psidl", int, 10, 10, None),
    FieldSchema("gap", float, 20, 10, None),
    FieldSchema("mvinc", float, 30, 10, None),
    FieldSchema("istop", int, 40, 10, 0),
)

class ControlFormingHomeGap(KeywordBase):
    """DYNA CONTROL_FORMING_HOME_GAP keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_HOME_GAP"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingHomeGap class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGHOMEGAP_CARD0,
                **kwargs,
            ),        ]
    @property
    def psidu(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the tools above the blank (upper tools)
        """ # nopep8
        return self._cards[0].get_value("psidu")

    @psidu.setter
    def psidu(self, value: int) -> None:
        """Set the psidu property."""
        self._cards[0].set_value("psidu", value)

    @property
    def psidl(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the tools below the blank (lower tools)
        """ # nopep8
        return self._cards[0].get_value("psidl")

    @psidl.setter
    def psidl(self, value: int) -> None:
        """Set the psidl property."""
        self._cards[0].set_value("psidl", value)

    @property
    def gap(self) -> typing.Optional[float]:
        """Get or set the Minimum gap allowed between the upper and lower tools
        """ # nopep8
        return self._cards[0].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        """Set the gap property."""
        self._cards[0].set_value("gap", value)

    @property
    def mvinc(self) -> typing.Optional[float]:
        """Get or set the Incremental movement of tools from home position to starting position to check the gap
        """ # nopep8
        return self._cards[0].get_value("mvinc")

    @mvinc.setter
    def mvinc(self, value: float) -> None:
        """Set the mvinc property."""
        self._cards[0].set_value("mvinc", value)

    @property
    def istop(self) -> int:
        """Get or set the How to proceed if the minimum gap found is less than GAP:
        EQ.0:	Output a warning message.Job continues
        EQ.1 : Terminate the job.
        """ # nopep8
        return self._cards[0].get_value("istop")

    @istop.setter
    def istop(self, value: int) -> None:
        """Set the istop property."""
        if value not in [0, 1, None]:
            raise Exception("""istop must be `None` or one of {0,1}.""")
        self._cards[0].set_value("istop", value)

