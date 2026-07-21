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

"""Module providing the EmOutputBinary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMOUTPUTBINARY_CARD0 = (
    FieldSchema("ibin", int, 0, 10, 0),
)

class EmOutputBinary(KeywordBase):
    """DYNA EM_OUTPUT_BINARY keyword"""

    keyword = "EM"
    subkeyword = "OUTPUT_BINARY"

    def __init__(self, **kwargs):
        """Initialize the EmOutputBinary class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMOUTPUTBINARY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ibin(self) -> int:
        """Get or set the Type of output:
        EQ.0: All EM results are output in ASCII format.See �em_[�].dat� files.
        EQ.1 : Some EM results are output in the binout file under the �emprint� directory.See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("ibin")

    @ibin.setter
    def ibin(self, value: int) -> None:
        """Set the ibin property."""
        if value not in [0, 1, None]:
            raise Exception("""ibin must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ibin", value)

