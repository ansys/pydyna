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

"""Module providing the ControlMppDecompositionMethod class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlMppDecompositionMethod(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_METHOD keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_METHOD"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionMethod class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "name",
                        str,
                        0,
                        80,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def name(self) -> typing.Optional[str]:
        """Get or set the Name of the decomposition method to use. There are currently two options:
        RCB = recursive coordinate besection
        GREEDY=a simple heuristic method
        In almost all cases the RCB method is superior and should be used.
        """ # nopep8
        return self._cards[0].get_value("name")

    @name.setter
    def name(self, value: str) -> None:
        """Set the name property."""
        self._cards[0].set_value("name", value)

