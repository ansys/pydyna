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

"""Module providing the DatabaseFsi class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEFSI_CARD0 = (
    FieldSchema("dtout", float, 0, 10, None),
    FieldSchema("binary", int, 10, 10, 1),
)

_DATABASEFSI_CARD1 = (
    FieldSchema("dbsfi_id", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("stdype", int, 20, 10, 0),
    FieldSchema("swid", int, 30, 10, None),
    FieldSchema("convid", int, 40, 10, None),
    FieldSchema("ndsetid", int, 50, 10, None),
    FieldSchema("cid", int, 60, 10, None),
)

class DatabaseFsi(KeywordBase):
    """DYNA DATABASE_FSI keyword"""

    keyword = "DATABASE"
    subkeyword = "FSI"

    def __init__(self, **kwargs):
        """Initialize the DatabaseFsi class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEFSI_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEFSI_CARD1,
                **kwargs,
            ),        ]
    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Output interval
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
        EQ.1:	ASCII file is written:	This is the default for shared memory parallel (SMP) LS-DYNA executables.
        EQ.2:	Data written to a binary database binout, which contains data that would otherwise be output to the ASCII file.
        The ASCII file in this case is not created.  This is the default for MPP LS-DYNA executables.
        EQ.3:	ASCII file is written, and the data is also written to the binary database (NOTE: MPP LS-DYNA executables will only produce the binary database).
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""binary must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("binary", value)

    @property
    def dbsfi_id(self) -> typing.Optional[int]:
        """Get or set the Surface ID (for reference purposes only)
        """ # nopep8
        return self._cards[1].get_value("dbsfi_id")

    @dbsfi_id.setter
    def dbsfi_id(self, value: int) -> None:
        """Set the dbsfi_id property."""
        self._cards[1].set_value("dbsfi_id", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID .  This Lagrangian SID must be contained in a Lagrangian structure SID defined in a corresponding coupling card, *CONSTRAINED_LAGRANGE_IN_SOLID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[1].set_value("sid", value)

    @property
    def stdype(self) -> int:
        """Get or set the Set type:
        EQ.0: Part set,
        EQ.1: Part,
        EQ.2: Segment set.
        """ # nopep8
        return self._cards[1].get_value("stdype")

    @stdype.setter
    def stdype(self, value: int) -> None:
        """Set the stdype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stdype must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("stdype", value)

    @property
    def swid(self) -> typing.Optional[int]:
        """Get or set the Switch ID from a corresponding *ALE_FSI_SWITCH_MMG_ID card.  If defined, the accumulative mass of the switched ALE multi-material group (AMMG) is written out under the pleak parameter in the dbfsi file.
        """ # nopep8
        return self._cards[1].get_value("swid")

    @swid.setter
    def swid(self, value: int) -> None:
        """Set the swid property."""
        self._cards[1].set_value("swid", value)

    @property
    def convid(self) -> typing.Optional[int]:
        """Get or set the For airbag application only: Convection ID from a corresponding *LOAD_ALE_CONVECTION_ID card (which computes the heat transfer between inflator gas and the inflator canister).  If defined, the temperature of the Lagrangian part having heat transfer with the gas, and its change in temperature as function of time in the dbfsi file.
        """ # nopep8
        return self._cards[1].get_value("convid")

    @convid.setter
    def convid(self, value: int) -> None:
        """Set the convid property."""
        self._cards[1].set_value("convid", value)

    @property
    def ndsetid(self) -> typing.Optional[int]:
        """Get or set the Set ID consisting of the nodes on which the moments of the forces applied on SID are computed.
        """ # nopep8
        return self._cards[1].get_value("ndsetid")

    @ndsetid.setter
    def ndsetid(self, value: int) -> None:
        """Set the ndsetid property."""
        self._cards[1].set_value("ndsetid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

