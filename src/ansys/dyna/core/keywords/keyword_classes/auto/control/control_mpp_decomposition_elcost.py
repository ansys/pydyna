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

"""Module providing the ControlMppDecompositionElcost class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLMPPDECOMPOSITIONELCOST_CARD0 = (
    FieldSchema("itype", int, 0, 10, 1),
)

class ControlMppDecompositionElcost(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_ELCOST keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_ELCOST"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionElcost class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLMPPDECOMPOSITIONELCOST_CARD0,
                **kwargs,
            ),        ]
    @property
    def itype(self) -> int:
        """Get or set the Hardware specific cost profile.
        1: Fujitsu PrimePower
        2: Intel IA 64, AMD Opteron
        3: Intel Xeon 64
        4: General profile
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""itype must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("itype", value)

