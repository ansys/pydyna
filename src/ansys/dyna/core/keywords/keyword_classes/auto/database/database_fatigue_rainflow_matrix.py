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

"""Module providing the DatabaseFatigueRainflowMatrix class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DATABASEFATIGUERAINFLOWMATRIX_CARD0 = (
    FieldSchema("binary", int, 0, 10, 0),
    FieldSchema("solid", int, 10, 10, None),
    FieldSchema("beam", int, 20, 10, None),
    FieldSchema("shell", int, 30, 10, None),
    FieldSchema("tshell", int, 40, 10, None),
)

class DatabaseFatigueRainflowMatrix(KeywordBase):
    """DYNA DATABASE_FATIGUE_RAINFLOW_MATRIX keyword"""

    keyword = "DATABASE"
    subkeyword = "FATIGUE_RAINFLOW_MATRIX"
    _link_fields = {
        "beam": LinkType.SET_BEAM,
        "solid": LinkType.SET_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseFatigueRainflowMatrix class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEFATIGUERAINFLOWMATRIX_CARD0,
                **kwargs,
            ),
        ]
    @property
    def binary(self) -> int:
        """Get or set the Flag for writing out the fatigue.rainflow lsda file.
        EQ.0:No fatigue.rainflow output
        EQ.1:Write out fatigue.rainflow lsda file
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [0, 1, None]:
            raise Exception("""binary must be `None` or one of {0,1}.""")
        self._cards[0].set_value("binary", value)

    @property
    def solid(self) -> typing.Optional[int]:
        """Get or set the Solid set ID
        """ # nopep8
        return self._cards[0].get_value("solid")

    @solid.setter
    def solid(self, value: int) -> None:
        """Set the solid property."""
        self._cards[0].set_value("solid", value)

    @property
    def beam(self) -> typing.Optional[int]:
        """Get or set the Beam set ID
        """ # nopep8
        return self._cards[0].get_value("beam")

    @beam.setter
    def beam(self, value: int) -> None:
        """Set the beam property."""
        self._cards[0].set_value("beam", value)

    @property
    def shell(self) -> typing.Optional[int]:
        """Get or set the Shell set ID
        """ # nopep8
        return self._cards[0].get_value("shell")

    @shell.setter
    def shell(self, value: int) -> None:
        """Set the shell property."""
        self._cards[0].set_value("shell", value)

    @property
    def tshell(self) -> typing.Optional[int]:
        """Get or set the TShell set ID
        """ # nopep8
        return self._cards[0].get_value("tshell")

    @tshell.setter
    def tshell(self, value: int) -> None:
        """Set the tshell property."""
        self._cards[0].set_value("tshell", value)

    @property
    def beam_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_BEAM_* keyword for beam."""
        return self._get_set_link("BEAM", self.beam)

    @beam_link.setter
    def beam_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for beam."""
        self.beam = value.sid

    @property
    def solid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SOLID_* keyword for solid."""
        return self._get_set_link("SOLID", self.solid)

    @solid_link.setter
    def solid_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for solid."""
        self.solid = value.sid

