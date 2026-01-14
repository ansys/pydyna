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

"""Module providing the AleCouplingNodalConstraintTitle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALECOUPLINGNODALCONSTRAINTTITLE_CARD0 = (
    FieldSchema("coupid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_ALECOUPLINGNODALCONSTRAINTTITLE_CARD1 = (
    FieldSchema("strsid", int, 0, 10, None),
    FieldSchema("alesid", int, 10, 10, None),
    FieldSchema("strsty", int, 20, 10, 0),
    FieldSchema("alesty", int, 30, 10, 0),
    FieldSchema("ctype", int, 40, 10, 0),
    FieldSchema("mcoup", int, 50, 10, None),
)

_ALECOUPLINGNODALCONSTRAINTTITLE_CARD2 = (
    FieldSchema("start", float, 0, 10, 0.0),
    FieldSchema("end", float, 10, 10, 10000000000.0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("frcmin", float, 50, 10, 0.5),
)

class AleCouplingNodalConstraintTitle(KeywordBase):
    """DYNA ALE_COUPLING_NODAL_CONSTRAINT_TITLE keyword"""

    keyword = "ALE"
    subkeyword = "COUPLING_NODAL_CONSTRAINT_TITLE"

    def __init__(self, **kwargs):
        """Initialize the AleCouplingNodalConstraintTitle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALCONSTRAINTTITLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALCONSTRAINTTITLE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALCONSTRAINTTITLE_CARD2,
                **kwargs,
            ),        ]
    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the Coupling (card) ID number (I10). If not defined, LSDYNA will assign an internal coupling ID based on the order of appearance in the input deck.
        """ # nopep8
        return self._cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        """Set the coupid property."""
        self._cards[0].set_value("coupid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the A description of this coupling definition (A70).
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def strsid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set or segment set ID of the structure (see *PART, *SET_‌PART or *SET_‌SEGMENT). The structure may include Lagrangian solid, shell, beam, thick shell, or discrete sphere elements. EFG, SPH, or EFG nodes may be used, but the boundary conditions may not be satisfied
        """ # nopep8
        return self._cards[1].get_value("strsid")

    @strsid.setter
    def strsid(self, value: int) -> None:
        """Set the strsid property."""
        self._cards[1].set_value("strsid", value)

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART).
        """ # nopep8
        return self._cards[1].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        """Set the alesid property."""
        self._cards[1].set_value("alesid", value)

    @property
    def strsty(self) -> int:
        """Get or set the Set type of STRSID
        EQ.0: Part set ID (PSID).
        EQ.1: Part ID (PID).
        EQ.2: Segment set ID (SGSID).
        EQ.3: Node set ID(NSID)
        """ # nopep8
        return self._cards[1].get_value("strsty")

    @strsty.setter
    def strsty(self, value: int) -> None:
        """Set the strsty property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""strsty must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("strsty", value)

    @property
    def alesty(self) -> int:
        """Get or set the Master set type of "MASTER"
        EQ.0: Part set ID (PSID).
        EQ.1: Part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("alesty")

    @alesty.setter
    def alesty(self, value: int) -> None:
        """Set the alesty property."""
        if value not in [0, 1, None]:
            raise Exception("""alesty must be `None` or one of {0,1}.""")
        self._cards[1].set_value("alesty", value)

    @property
    def ctype(self) -> int:
        """Get or set the Coupling type:
        EQ.1: Constrained acceleration.
        EQ.2: Constrained acceleration and velocity.
        """ # nopep8
        return self._cards[1].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ctype must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("ctype", value)

    @property
    def mcoup(self) -> typing.Optional[int]:
        """Get or set the Multi-material option (CTYPE 4, 5, 6, 11 and 12, ).
        EQ.0: Couple with all multi-material groups,
        EQ.-n: refers to a set ID of an ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP card in which its set ID=n.
        """ # nopep8
        return self._cards[1].get_value("mcoup")

    @mcoup.setter
    def mcoup(self, value: int) -> None:
        """Set the mcoup property."""
        self._cards[1].set_value("mcoup", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for coupling.
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time for coupling.
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        """Set the end property."""
        self._cards[2].set_value("end", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Only to be used with nonzero MCOUP. Minimum volume fraction of the fluid materials included in the list of AMMGs to activate coupling. Default value is 0.5. Reducing FRCMIN (typically, between 0.1 and 0.3) would turn on coupling earlier to prevent leakage in hypervelocity impact cases.
        """ # nopep8
        return self._cards[2].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        """Set the frcmin property."""
        self._cards[2].set_value("frcmin", value)

