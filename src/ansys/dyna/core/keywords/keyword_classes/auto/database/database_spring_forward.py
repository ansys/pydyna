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

"""Module providing the DatabaseSpringForward class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASESPRINGFORWARD_CARD0 = (
    FieldSchema("iflag", int, 0, 10, 0),
)

class DatabaseSpringForward(KeywordBase):
    """DYNA DATABASE_SPRING_FORWARD keyword"""

    keyword = "DATABASE"
    subkeyword = "SPRING_FORWARD"

    def __init__(self, **kwargs):
        """Initialize the DatabaseSpringForward class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASESPRINGFORWARD_CARD0,
                **kwargs,
            ),        ]
    @property
    def iflag(self) -> int:
        """Get or set the Output type:
        EQ.0: off,
        EQ.1: output element nodal force vector for deformable nodes,
        EQ.2: output element nodal force vector for materials, subset for NIKE3D interface file.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iflag must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iflag", value)

