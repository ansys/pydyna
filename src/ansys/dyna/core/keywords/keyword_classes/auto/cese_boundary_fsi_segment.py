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

"""Module providing the CeseBoundaryFsiSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class CeseBoundaryFsiSegment(KeywordBase):
    """DYNA CESE_BOUNDARY_FSI_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_FSI_SEGMENT"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryFsiSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

