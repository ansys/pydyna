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

"""Module providing the AleFsiProjection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALEFSIPROJECTION_CARD0 = (
    FieldSchema("lagsid", int, 0, 10, None),
    FieldSchema("alesid", int, 10, 10, None),
    FieldSchema("lsidtyp", int, 20, 10, 0),
    FieldSchema("asidtyp", int, 30, 10, 0),
    FieldSchema("smmgid", int, 40, 10, None),
    FieldSchema("icorrec", int, 50, 10, None),
    FieldSchema("inorm", int, 60, 10, None),
)

_ALEFSIPROJECTION_CARD1 = (
    FieldSchema("birth", float, 0, 10, 0.0),
    FieldSchema("death", float, 10, 10, 10000000000.0),
)

class AleFsiProjection(KeywordBase):
    """DYNA ALE_FSI_PROJECTION keyword"""

    keyword = "ALE"
    subkeyword = "FSI_PROJECTION"

    def __init__(self, **kwargs):
        """Initialize the AleFsiProjection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEFSIPROJECTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEFSIPROJECTION_CARD1,
                **kwargs,
            ),        ]
    @property
    def lagsid(self) -> typing.Optional[int]:
        """Get or set the A set ID defining lagrangian part(s) for this coupling(structures).
        """ # nopep8
        return self._cards[0].get_value("lagsid")

    @lagsid.setter
    def lagsid(self, value: int) -> None:
        """Set the lagsid property."""
        self._cards[0].set_value("lagsid", value)

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the A set ID defining the ALE part(s) for this coupling(fluids).
        """ # nopep8
        return self._cards[0].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        """Set the alesid property."""
        self._cards[0].set_value("alesid", value)

    @property
    def lsidtyp(self) -> int:
        """Get or set the lagrangian Set ID TYPE.
        EQ.0: Part set ID (PSID) (default).
        EQ.1: Part ID (PID)
        """ # nopep8
        return self._cards[0].get_value("lsidtyp")

    @lsidtyp.setter
    def lsidtyp(self, value: int) -> None:
        """Set the lsidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""lsidtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("lsidtyp", value)

    @property
    def asidtyp(self) -> int:
        """Get or set the ALE Set ID type.
        EQ.0: Part set ID (PSID) (default).
        EQ.1: Part ID (PID)
        """ # nopep8
        return self._cards[0].get_value("asidtyp")

    @asidtyp.setter
    def asidtyp(self, value: int) -> None:
        """Set the asidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""asidtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("asidtyp", value)

    @property
    def smmgid(self) -> typing.Optional[int]:
        """Get or set the A set ID referring to a group of one or more ALE-Multi-Material-Group (AMMG) IDs which represents the ALE materials interacting with the Lagrangian structure.  This SMMGID is a set ID defined by *SET_MULTI-MATERIAL_GROUP_LIST.
        """ # nopep8
        return self._cards[0].get_value("smmgid")

    @smmgid.setter
    def smmgid(self, value: int) -> None:
        """Set the smmgid property."""
        self._cards[0].set_value("smmgid", value)

    @property
    def icorrec(self) -> typing.Optional[int]:
        """Get or set the Advection error correction method (See Remark 1).
        EQ.1: ALE mass is conserved.  Leaked mass is moved,
        EQ.2: ALE mass is almost conserved,
        EQ.3: No correction performed (default).  ALE mass is conserved.  Some leakage may occur.  This may be the best solution.
        """ # nopep8
        return self._cards[0].get_value("icorrec")

    @icorrec.setter
    def icorrec(self, value: int) -> None:
        """Set the icorrec property."""
        self._cards[0].set_value("icorrec", value)

    @property
    def inorm(self) -> typing.Optional[int]:
        """Get or set the Type of coupling.
        EQ.0: Couple in all directions,
        EQ.1: Couple in compression and tension (free sliding),
        EQ.2: Couple in compression only (free sliding).  This choice requires ICORREC=3.
        """ # nopep8
        return self._cards[0].get_value("inorm")

    @inorm.setter
    def inorm(self, value: int) -> None:
        """Set the inorm property."""
        self._cards[0].set_value("inorm", value)

    @property
    def birth(self) -> float:
        """Get or set the Start time for coupling
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the End time for coupling
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

