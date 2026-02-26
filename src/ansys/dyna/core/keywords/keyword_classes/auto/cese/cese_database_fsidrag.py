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

"""Module providing the CeseDatabaseFsidrag class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEDATABASEFSIDRAG_CARD0 = (
    FieldSchema("outlv", int, 0, 10, 0),
)

class CeseDatabaseFsidrag(KeywordBase):
    """DYNA CESE_DATABASE_FSIDRAG keyword"""

    keyword = "CESE"
    subkeyword = "DATABASE_FSIDRAG"

    def __init__(self, **kwargs):
        """Initialize the CeseDatabaseFsidrag class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEDATABASEFSIDRAG_CARD0,
                **kwargs,
            ),        ]
    @property
    def outlv(self) -> int:
        """Get or set the Determines if the output file should be dumped.
        EQ.0: No output file is generated.
        EQ.1: The output file giving the pressure forces is generated.
        """ # nopep8
        return self._cards[0].get_value("outlv")

    @outlv.setter
    def outlv(self, value: int) -> None:
        """Set the outlv property."""
        self._cards[0].set_value("outlv", value)

