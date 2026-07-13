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

"""Module providing the IcfdBoundaryFsiOneway class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDBOUNDARYFSIONEWAY_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("iowc", int, 10, 10, 1),
)

class IcfdBoundaryFsiOneway(KeywordBase):
    """DYNA ICFD_BOUNDARY_FSI_ONEWAY keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_FSI_ONEWAY"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryFsiOneway class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYFSIONEWAY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of a part from the solid mechanics problem for which only one-way FSI coupling occurs
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def iowc(self) -> int:
        """Get or set the Indicates the coupling direction to the solver:
        EQ.1: The solid mechanics solver transfers displacements to the fluid solver.
        EQ.2: The fluid solver transfers stresses to the solid mechanics solver.
        """ # nopep8
        return self._cards[0].get_value("iowc")

    @iowc.setter
    def iowc(self, value: int) -> None:
        """Set the iowc property."""
        if value not in [1, 2, None]:
            raise Exception("""iowc must be `None` or one of {1,2}.""")
        self._cards[0].set_value("iowc", value)

