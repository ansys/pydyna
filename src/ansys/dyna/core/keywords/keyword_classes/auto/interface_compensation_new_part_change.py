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

"""Module providing the InterfaceCompensationNewPartChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceCompensationNewPartChange(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_NEW_PART_CHANGE keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_NEW_PART_CHANGE"

    def __init__(self, **kwargs):
        """Initialize the InterfaceCompensationNewPartChange class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "maxgap",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def maxgap(self) -> typing.Optional[float]:
        """Get or set the Maximum gap between the original part and changed part
        """ # nopep8
        return self._cards[0].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        """Set the maxgap property."""
        self._cards[0].set_value("maxgap", value)

