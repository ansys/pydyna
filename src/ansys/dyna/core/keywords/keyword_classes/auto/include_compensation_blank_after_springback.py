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

"""Module providing the IncludeCompensationBlankAfterSpringback class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IncludeCompensationBlankAfterSpringback(KeywordBase):
    """DYNA INCLUDE_COMPENSATION_BLANK_AFTER_SPRINGBACK keyword"""

    keyword = "INCLUDE"
    subkeyword = "COMPENSATION_BLANK_AFTER_SPRINGBACK"

    def __init__(self, **kwargs):
        """Initialize the IncludeCompensationBlankAfterSpringback class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        256,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the keyword files containing nodes and elements information, with adaptive constraints if exist. Currently all blanks must have the same numbers of nodes and elements.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

