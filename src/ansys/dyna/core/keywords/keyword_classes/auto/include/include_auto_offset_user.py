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

"""Module providing the IncludeAutoOffsetUser class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INCLUDEAUTOOFFSETUSER_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_INCLUDEAUTOOFFSETUSER_CARD1 = (
    FieldSchema("noffset", int, 0, 10, None),
    FieldSchema("neoffset", int, 10, 10, None),
)

class IncludeAutoOffsetUser(KeywordBase):
    """DYNA INCLUDE_AUTO_OFFSET_USER keyword"""

    keyword = "INCLUDE"
    subkeyword = "AUTO_OFFSET_USER"

    def __init__(self, **kwargs):
        """Initialize the IncludeAutoOffsetUser class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INCLUDEAUTOOFFSETUSER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDEAUTOOFFSETUSER_CARD1,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name to be included.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def noffset(self) -> typing.Optional[int]:
        """Get or set the Starting node ID of the include file to be offset.
        """ # nopep8
        return self._cards[1].get_value("noffset")

    @noffset.setter
    def noffset(self, value: int) -> None:
        """Set the noffset property."""
        self._cards[1].set_value("noffset", value)

    @property
    def neoffset(self) -> typing.Optional[int]:
        """Get or set the Starting element ID of the include file to be offset.
        """ # nopep8
        return self._cards[1].get_value("neoffset")

    @neoffset.setter
    def neoffset(self, value: int) -> None:
        """Set the neoffset property."""
        self._cards[1].set_value("neoffset", value)

