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

"""Module providing the InterfaceSpg2 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INTERFACESPG2_CARD0 = (
    FieldSchema("ispghis", int, 0, 10, 0),
)

class InterfaceSpg2(KeywordBase):
    """DYNA INTERFACE_SPG_2 keyword"""

    keyword = "INTERFACE"
    subkeyword = "SPG_2"

    def __init__(self, **kwargs):
        """Initialize the InterfaceSpg2 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACESPG2_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ispghis(self) -> int:
        """Get or set the Option to read more internal variables other than the stresses and effective plastic strain if ISPGHIS = 1 in the first stage:
        EQ.0: Read only the stresses and effective plastic strain.
        EQ.1: Read all other internal variables required by the material model.The output file from the first stage only includes these internal history variables if ISPGHIS = 1 on * INTERFACE_SPG_1.
        """ # nopep8
        return self._cards[0].get_value("ispghis")

    @ispghis.setter
    def ispghis(self, value: int) -> None:
        """Set the ispghis property."""
        if value not in [0, 1, None]:
            raise Exception("""ispghis must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ispghis", value)

