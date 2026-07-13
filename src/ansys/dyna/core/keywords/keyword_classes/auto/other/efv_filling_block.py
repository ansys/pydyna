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

"""Module providing the EfvFillingBlock class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFILLINGBLOCK_CARD0 = (
    FieldSchema("file", str, 0, 80, None),
)

class EfvFillingBlock(KeywordBase):
    """DYNA EFV_FILLING_BLOCK keyword"""

    keyword = "EFV"
    subkeyword = "FILLING_BLOCK"

    def __init__(self, **kwargs):
        """Initialize the EfvFillingBlock class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFILLINGBLOCK_CARD0,
                **kwargs,
            ),
        ]
    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Filename of a file that defines which element is active in a structured FCT mesh (see Remark 1). This feature can not be used with other *EFV_FILLING keywords.
        """ # nopep8
        return self._cards[0].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        """Set the file property."""
        self._cards[0].set_value("file", value)

