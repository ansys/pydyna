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

"""Module providing the DatabaseBinaryD3Prop class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEBINARYD3PROP_CARD0 = (
    FieldSchema("ifile", int, 0, 10, 1),
    FieldSchema("imatl", int, 10, 10, 0),
    FieldSchema("iwall", int, 20, 10, 0),
)

class DatabaseBinaryD3Prop(KeywordBase):
    """DYNA DATABASE_BINARY_D3PROP keyword"""

    keyword = "DATABASE"
    subkeyword = "BINARY_D3PROP"

    def __init__(self, **kwargs):
        """Initialize the DatabaseBinaryD3Prop class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEBINARYD3PROP_CARD0,
                **kwargs,
            ),        ]
    @property
    def ifile(self) -> int:
        """Get or set the Specify file for D3PROP output.  (This can also be defined on the command line by adding d3prop = 1 or d3prop = 2 which also sets IMATL =  IWALL = 1)
        EQ.1: Output data at the end of the first d3plot file.
        EQ.2: Output data to the file d3prop.
        """ # nopep8
        return self._cards[0].get_value("ifile")

    @ifile.setter
    def ifile(self, value: int) -> None:
        """Set the ifile property."""
        if value not in [1, 2, None]:
            raise Exception("""ifile must be `None` or one of {1,2}.""")
        self._cards[0].set_value("ifile", value)

    @property
    def imatl(self) -> int:
        """Get or set the Output *EOS, *HOURGLASS, *MAT, *PART and *SECTION data.
        EQ.0: No
        EQ.1: Yes
        """ # nopep8
        return self._cards[0].get_value("imatl")

    @imatl.setter
    def imatl(self, value: int) -> None:
        """Set the imatl property."""
        if value not in [0, 1, None]:
            raise Exception("""imatl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imatl", value)

    @property
    def iwall(self) -> int:
        """Get or set the Output *RIGIDWALL data.
        EQ.0: No
        EQ.1: Yes
        """ # nopep8
        return self._cards[0].get_value("iwall")

    @iwall.setter
    def iwall(self, value: int) -> None:
        """Set the iwall property."""
        if value not in [0, 1, None]:
            raise Exception("""iwall must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iwall", value)

