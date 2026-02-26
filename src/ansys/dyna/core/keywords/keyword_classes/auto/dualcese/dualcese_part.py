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

"""Module providing the DualcesePart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEPART_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("mid", int, 10, 10, None),
    FieldSchema("eosid", int, 20, 10, None),
    FieldSchema("fsitype", str, 30, 10, None),
    FieldSchema("mmshid", int, 40, 10, None),
)

class DualcesePart(KeywordBase):
    """DYNA DUALCESE_PART keyword"""

    keyword = "DUALCESE"
    subkeyword = "PART"

    def __init__(self, **kwargs):
        """Initialize the DualcesePart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEPART_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID (must be different from any other *DUALCESE_PART part ID or from a *DUALCESE_PART_MULTIPHASE PID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID refering to DUALCESE_MAT_material
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID defined by a *DUALCESE_EOS_... card
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def fsitype(self) -> typing.Optional[str]:
        """Get or set the FSI type to use on this part:
        BLANK:	no FSI performed
        EQ.IBM : Immersed boundary FSI solver
        EQ.MOVMESH : Moving mesh FSI solver(FSITYPE =MMM may also be used for the same effect
        """ # nopep8
        return self._cards[0].get_value("fsitype")

    @fsitype.setter
    def fsitype(self, value: str) -> None:
        """Set the fsitype property."""
        self._cards[0].set_value("fsitype", value)

    @property
    def mmshid(self) -> typing.Optional[int]:
        """Get or set the ID of the mesh motion algorithm to use for the moving mesh FSI solver on this part (region of the current dual CESE mesh)).  This ID refers to a *DUALCESE_CONTROL_MESH_MOV card ID.
        """ # nopep8
        return self._cards[0].get_value("mmshid")

    @mmshid.setter
    def mmshid(self, value: int) -> None:
        """Set the mmshid property."""
        self._cards[0].set_value("mmshid", value)

