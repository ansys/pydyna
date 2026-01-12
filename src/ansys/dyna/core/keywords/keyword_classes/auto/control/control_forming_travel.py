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

"""Module providing the ControlFormingTravel class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGTRAVEL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("vid", int, 10, 10, None),
    FieldSchema("travel", float, 20, 10, None),
    FieldSchema("target", int, 30, 10, None),
    FieldSchema("gap", float, 40, 10, None),
    FieldSchema("phase", int, 50, 10, None),
    FieldSchema("follow", int, 60, 10, None),
)

class ControlFormingTravel(KeywordBase):
    """DYNA CONTROL_FORMING_TRAVEL keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_TRAVEL"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingTravel class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGTRAVEL_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID defining the tool's movement orientation
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def travel(self) -> typing.Optional[float]:
        """Get or set the Move PID a certain distance
        """ # nopep8
        return self._cards[0].get_value("travel")

    @travel.setter
    def travel(self, value: float) -> None:
        """Set the travel property."""
        self._cards[0].set_value("travel", value)

    @property
    def target(self) -> typing.Optional[int]:
        """Get or set the Move PID to meet TARGET
        """ # nopep8
        return self._cards[0].get_value("target")

    @target.setter
    def target(self, value: int) -> None:
        """Set the target property."""
        self._cards[0].set_value("target", value)

    @property
    def gap(self) -> typing.Optional[float]:
        """Get or set the The minimum distance between PID and TARGET
        """ # nopep8
        return self._cards[0].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        """Set the gap property."""
        self._cards[0].set_value("gap", value)

    @property
    def phase(self) -> typing.Optional[int]:
        """Get or set the Phase number
        """ # nopep8
        return self._cards[0].get_value("phase")

    @phase.setter
    def phase(self, value: int) -> None:
        """Set the phase property."""
        self._cards[0].set_value("phase", value)

    @property
    def follow(self) -> typing.Optional[int]:
        """Get or set the PID can also move by following the part defined in FOLLOW. During this phase, the distance between PID and FOLLOW will Be constant
        """ # nopep8
        return self._cards[0].get_value("follow")

    @follow.setter
    def follow(self, value: int) -> None:
        """Set the follow property."""
        self._cards[0].set_value("follow", value)

