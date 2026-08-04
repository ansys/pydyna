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

"""Module providing the IcfdBoundaryPrescribedMovemeshDr class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDBOUNDARYPRESCRIBEDMOVEMESHDR_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("dofx", int, 10, 10, 1),
    FieldSchema("dofy", int, 20, 10, 1),
    FieldSchema("dofz", int, 30, 10, 1),
)

_ICFDBOUNDARYPRESCRIBEDMOVEMESHDR_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("drdofx", int, 10, 10, 1),
    FieldSchema("drdofy", int, 20, 10, 1),
    FieldSchema("drdofz", int, 30, 10, 1),
)

class IcfdBoundaryPrescribedMovemeshDr(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_MOVEMESH_DR keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_MOVEMESH_DR"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPrescribedMovemeshDr class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDMOVEMESHDR_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDMOVEMESHDR_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def dofx(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions:
        EQ.0: Degree of freedom left free (surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        """Set the dofx property."""
        if value not in [1, 0, None]:
            raise Exception("""dofx must be `None` or one of {1,0}.""")
        self._cards[0].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions:
        EQ.0: Degree of freedom left free (surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        """Set the dofy property."""
        if value not in [1, 0, None]:
            raise Exception("""dofy must be `None` or one of {1,0}.""")
        self._cards[0].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the Degrees of freedom in the X,Y and Z directions:
        EQ.0: Degree of freedom left free (surface nodes can translate in the
        chosen direction)
        EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
        """ # nopep8
        return self._cards[0].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        """Set the dofz property."""
        if value not in [1, 0, None]:
            raise Exception("""dofz must be `None` or one of {1,0}.""")
        self._cards[0].set_value("dofz", value)

    @property
    def drdofx(self) -> int:
        """Get or set the Same as DOFX, DOFY, and DOFZ but applies to the mesh displacement during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("drdofx")

    @drdofx.setter
    def drdofx(self, value: int) -> None:
        """Set the drdofx property."""
        if value not in [1, 0, None]:
            raise Exception("""drdofx must be `None` or one of {1,0}.""")
        self._cards[1].set_value("drdofx", value)

    @property
    def drdofy(self) -> int:
        """Get or set the Same as DOFX, DOFY, and DOFZ but applies to the mesh displacement during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("drdofy")

    @drdofy.setter
    def drdofy(self, value: int) -> None:
        """Set the drdofy property."""
        if value not in [1, 0, None]:
            raise Exception("""drdofy must be `None` or one of {1,0}.""")
        self._cards[1].set_value("drdofy", value)

    @property
    def drdofz(self) -> int:
        """Get or set the Same as DOFX, DOFY, and DOFZ but applies to the mesh displacement during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("drdofz")

    @drdofz.setter
    def drdofz(self, value: int) -> None:
        """Set the drdofz property."""
        if value not in [1, 0, None]:
            raise Exception("""drdofz must be `None` or one of {1,0}.""")
        self._cards[1].set_value("drdofz", value)

