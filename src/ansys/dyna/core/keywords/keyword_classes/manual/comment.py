# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Module to define *COMMENT keyword with multiline support."""

import typing

from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.text_card import TextCard


class Comment(KeywordBase):
    """DYNA COMMENT keyword.

    Supports multiline free-form text comments.
    """

    keyword = "COMMENT"
    subkeyword = "COMMENT"

    def __init__(self, **kwargs):
        """Initialize Comment keyword."""
        super().__init__(**kwargs)
        self._cards = [
            TextCard("comment", kwargs.get("comment")),
        ]

    @property
    def comment(self) -> typing.Optional[str]:
        """Get or set the comment text.

        The comment can span multiple lines. Lines are separated by newline characters.
        """
        return self._cards[0].value

    @comment.setter
    def comment(self, value: str) -> None:
        """Set the comment text."""
        self._cards[0].value = value
