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

"""Module providing the ControlFormingMaxid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLFORMINGMAXID_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("maxidn", int, 10, 10, None),
    FieldSchema("maxide", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("i2dynain", int, 40, 10, None),
)

class ControlFormingMaxid(KeywordBase):
    """DYNA CONTROL_FORMING_MAXID keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_MAXID"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingMaxid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGMAXID_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the sheet blank, as in *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def maxidn(self) -> typing.Optional[int]:
        """Get or set the Node ID number from which adaptive node ID numbers will be created
        """ # nopep8
        return self._cards[0].get_value("maxidn")

    @maxidn.setter
    def maxidn(self, value: int) -> None:
        """Set the maxidn property."""
        self._cards[0].set_value("maxidn", value)

    @property
    def maxide(self) -> typing.Optional[int]:
        """Get or set the Element ID number from which adaptive element ID numbers will be created.
        """ # nopep8
        return self._cards[0].get_value("maxide")

    @maxide.setter
    def maxide(self, value: int) -> None:
        """Set the maxide property."""
        self._cards[0].set_value("maxide", value)

    @property
    def i2dynain(self) -> typing.Optional[int]:
        """Get or set the Setting I2DYNAIN to 1 will cause this keyword to be output to a dynain file with the updated maximum node and element IDs. This output simplifies post-processing for multi-step processes since it ensures that element and node IDs generated in subsequent steps are larger than those in previous steps. By default, this keyword is not output to a dynain file.
        """ # nopep8
        return self._cards[0].get_value("i2dynain")

    @i2dynain.setter
    def i2dynain(self, value: int) -> None:
        """Set the i2dynain property."""
        self._cards[0].set_value("i2dynain", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

