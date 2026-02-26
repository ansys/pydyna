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

"""Module providing the IncludeMultiscaleSpotweld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INCLUDEMULTISCALESPOTWELD_CARD0 = (
    FieldSchema("type", int, 0, 10, None),
)

_INCLUDEMULTISCALESPOTWELD_CARD1 = (
    FieldSchema("filename", str, 0, 256, None),
)

class IncludeMultiscaleSpotweld(KeywordBase):
    """DYNA INCLUDE_MULTISCALE_SPOTWELD keyword"""

    keyword = "INCLUDE"
    subkeyword = "MULTISCALE_SPOTWELD"

    def __init__(self, **kwargs):
        """Initialize the IncludeMultiscaleSpotweld class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INCLUDEMULTISCALESPOTWELD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDEMULTISCALESPOTWELD_CARD1,
                **kwargs,
            ),        ]
    @property
    def type(self) -> typing.Optional[int]:
        """Get or set the TYPE for this multiscale spot weld. This type is used in the keyword
        *DEFINE_SPOTWELD_MULTISCALE. Any unique integer will do.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of file from which to read the spot weld definition
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

