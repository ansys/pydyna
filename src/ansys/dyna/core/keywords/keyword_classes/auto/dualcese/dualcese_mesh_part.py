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

"""Module providing the DualceseMeshPart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEMESHPART_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("fsitype", str, 10, 10, " "),
    FieldSchema("mmshid", int, 20, 10, None),
    FieldSchema("icht", int, 30, 10, 0),
)

class DualceseMeshPart(KeywordBase):
    """DYNA DUALCESE_MESH_PART keyword"""

    keyword = "DUALCESE"
    subkeyword = "MESH_PART"

    def __init__(self, **kwargs):
        """Initialize the DualceseMeshPart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEMESHPART_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID (must be different from any other *DUALCESE_MESH_PART part ID). It is important to note that *DUALCESE_PART and *DUALCESE_PART_MULTIPHASE cards should not be used when the newer *DUALCESE_MESH_PART card is used.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def fsitype(self) -> str:
        """Get or set the FSI type to use on this part:
        EQ.<BLANK>: If left blank, no FSI performed.
        EQ.IBM: Immersed boundary FSI solver
        EQ.MOVMESH: Moving mesh FSI solver(FSITYPE = MMM may also be used for the same effect)
        """ # nopep8
        return self._cards[0].get_value("fsitype")

    @fsitype.setter
    def fsitype(self, value: str) -> None:
        """Set the fsitype property."""
        self._cards[0].set_value("fsitype", value)

    @property
    def mmshid(self) -> typing.Optional[int]:
        """Get or set the ID for the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh).  This ID refers to the ID of an instantiation of  a *DUALCESE_CONTROL_MESH_MOV.
        """ # nopep8
        return self._cards[0].get_value("mmshid")

    @mmshid.setter
    def mmshid(self, value: int) -> None:
        """Set the mmshid property."""
        self._cards[0].set_value("mmshid", value)

    @property
    def icht(self) -> int:
        """Get or set the Conjugate heat transfer flag that applies when using the IBM FSI solver:
        EQ.0:	Do not perform conjugate heat transfer when using the IBM FSI solver.
        EQ.1 : Perform conjugate heat transfer when using the IBM FSI solver
        """ # nopep8
        return self._cards[0].get_value("icht")

    @icht.setter
    def icht(self, value: int) -> None:
        """Set the icht property."""
        if value not in [0, 1, None]:
            raise Exception("""icht must be `None` or one of {0,1}.""")
        self._cards[0].set_value("icht", value)

