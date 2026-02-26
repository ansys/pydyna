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

"""Module providing the EmControlErosion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCONTROLEROSION_CARD0 = (
    FieldSchema("ectrl", int, 0, 10, 0),
)

class EmControlErosion(KeywordBase):
    """DYNA EM_CONTROL_EROSION keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_EROSION"

    def __init__(self, **kwargs):
        """Initialize the EmControlErosion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLEROSION_CARD0,
                **kwargs,
            ),        ]
    @property
    def ectrl(self) -> int:
        """Get or set the Erosion search:
        EQ.0: Off. This means that the EM solver will ignore eroded elements and still consider them part of the EM problem.
        EQ.1: On. The EM solver will look for potential elements that are eroded and remove them from the EM solve by updating its matrix system.
        """ # nopep8
        return self._cards[0].get_value("ectrl")

    @ectrl.setter
    def ectrl(self, value: int) -> None:
        """Set the ectrl property."""
        if value not in [0, 1, None]:
            raise Exception("""ectrl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ectrl", value)

