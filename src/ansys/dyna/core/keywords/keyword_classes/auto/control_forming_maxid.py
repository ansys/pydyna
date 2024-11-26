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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlFormingMaxid(KeywordBase):
    """DYNA CONTROL_FORMING_MAXID keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_MAXID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "maxidn",
                        int,
                        10,
                        10,
                        kwargs.get("maxidn")
                    ),
                    Field(
                        "maxide",
                        int,
                        20,
                        10,
                        kwargs.get("maxide")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "i2dynain",
                        int,
                        40,
                        10,
                        kwargs.get("i2dynain")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the sheet blank, as in *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def maxidn(self) -> typing.Optional[int]:
        """Get or set the Node ID number from which adaptive node ID numbers will be created
        """ # nopep8
        return self._cards[0].get_value("maxidn")

    @maxidn.setter
    def maxidn(self, value: int) -> None:
        self._cards[0].set_value("maxidn", value)

    @property
    def maxide(self) -> typing.Optional[int]:
        """Get or set the Element ID number from which adaptive element ID numbers will be created.
        """ # nopep8
        return self._cards[0].get_value("maxide")

    @maxide.setter
    def maxide(self, value: int) -> None:
        self._cards[0].set_value("maxide", value)

    @property
    def i2dynain(self) -> typing.Optional[int]:
        """Get or set the Setting I2DYNAIN to 1 will cause this keyword to be output to a dynain file with the updated maximum node and element IDs. This output simplifies post-processing for multi-step processes since it ensures that element and node IDs generated in subsequent steps are larger than those in previous steps. By default, this keyword is not output to a dynain file.
        """ # nopep8
        return self._cards[0].get_value("i2dynain")

    @i2dynain.setter
    def i2dynain(self, value: int) -> None:
        self._cards[0].set_value("i2dynain", value)

