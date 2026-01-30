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

"""Module providing the DatabaseBinaryRunrsf class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DATABASEBINARYRUNRSF_CARD0 = (
    FieldSchema("cycl", float, 0, 10, None),
    FieldSchema("nr", int, 10, 10, None),
    FieldSchema("beam", int, 20, 10, 0),
    FieldSchema("npltc", int, 30, 10, None),
    FieldSchema("psetid", int, 40, 10, None),
)

class DatabaseBinaryRunrsf(KeywordBase):
    """DYNA DATABASE_BINARY_RUNRSF keyword"""

    keyword = "DATABASE"
    subkeyword = "BINARY_RUNRSF"
    _link_fields = {
        "psetid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseBinaryRunrsf class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEBINARYRUNRSF_CARD0,
                **kwargs,
            ),        ]
    @property
    def cycl(self) -> typing.Optional[float]:
        """Get or set the Output interval in time steps (a time step is a cycle). For the D3DRFL
        file a positive number 'n' will cause plot dumps to be written at every
        n'th convergence check interval specified on the *CONTROL_DYNAMIC_RELAXATION card.
        """ # nopep8
        return self._cards[0].get_value("cycl")

    @cycl.setter
    def cycl(self, value: float) -> None:
        """Set the cycl property."""
        self._cards[0].set_value("cycl", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of Running Restart Files, RUNRSF, written in a cyclical fashion.  The default number is one, i.e. the same file is overwritten each time.
        """ # nopep8
        return self._cards[0].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        """Set the nr property."""
        self._cards[0].set_value("nr", value)

    @property
    def beam(self) -> int:
        """Get or set the Option flag:
        EQ.0: Discrete spring and damper elements are added to the D3PART database where they are display as beam elements. The element global X, global Y, global Z and resultant forces are written to the database (default),
        EQ.1 No discrete spring and damper elements are added to the D3PART database. This option is useful when translating old LS-DYNA input decks to KEYWORD input. In older input decks there is no requirement that beam and spring elements have unique ID's, and beam elements may be created for the spring and dampers with identical ID's to existing beam elements causing a fatal error, EQ.2. Discrete spring and damper elements are added to the or D3PART database where they are displayed as beam elements (similar to option 0). In this option the element resultant force is written to its first database position allowing beam axial forces and spring resultant forces to be plotted at the same time. This can be useful during some post-processing applications.
        """ # nopep8
        return self._cards[0].get_value("beam")

    @beam.setter
    def beam(self, value: int) -> None:
        """Set the beam property."""
        self._cards[0].set_value("beam", value)

    @property
    def npltc(self) -> typing.Optional[int]:
        """Get or set the DT=ENDTIME/NPLTC. This overrides the DT specified in the first field.
        """ # nopep8
        return self._cards[0].get_value("npltc")

    @npltc.setter
    def npltc(self, value: int) -> None:
        """Set the npltc property."""
        self._cards[0].set_value("npltc", value)

    @property
    def psetid(self) -> typing.Optional[int]:
        """Get or set the Set part ID, see also *SET_PART_OPTION.
        """ # nopep8
        return self._cards[0].get_value("psetid")

    @psetid.setter
    def psetid(self, value: int) -> None:
        """Set the psetid property."""
        self._cards[0].set_value("psetid", value)

    @property
    def psetid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psetid."""
        return self._get_set_link("PART", self.psetid)

    @psetid_link.setter
    def psetid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psetid."""
        self.psetid = value.sid

