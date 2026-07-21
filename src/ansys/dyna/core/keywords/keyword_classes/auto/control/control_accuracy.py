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

"""Module providing the ControlAccuracy class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLACCURACY_CARD0 = (
    FieldSchema("osu", int, 0, 10, 0),
    FieldSchema("inn", int, 10, 10, 1),
    FieldSchema("pidos", int, 20, 10, None),
    FieldSchema("iacc", int, 30, 10, 0),
    FieldSchema("exacc", float, 40, 10, 0.0),
    FieldSchema("srtflg", int, 50, 10, 0),
)

class ControlAccuracy(KeywordBase):
    """DYNA CONTROL_ACCURACY keyword"""

    keyword = "CONTROL"
    subkeyword = "ACCURACY"
    _link_fields = {
        "pidos": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlAccuracy class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLACCURACY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def osu(self) -> int:
        """Get or set the Global flag for 2nd order objective stress update:
        EQ.0: off (default)
        EQ.1: on
        """ # nopep8
        return self._cards[0].get_value("osu")

    @osu.setter
    def osu(self, value: int) -> None:
        """Set the osu property."""
        if value not in [0, 1, None]:
            raise Exception("""osu must be `None` or one of {0,1}.""")
        self._cards[0].set_value("osu", value)

    @property
    def inn(self) -> int:
        """Get or set the Invariant node numbering for shell and solid elements:
        EQ.1: off (default for explicit)
        EQ.2: on for shell and thick shell elements(default for implicit)
        EQ.3: On for solid elements
        EQ.4: On for shell, thick shell and solid elements
        EQ.-2:On for shell elements except triangular shells
        EQ.-4:On for both shell and solid elements except triangular shells
        """ # nopep8
        return self._cards[0].get_value("inn")

    @inn.setter
    def inn(self, value: int) -> None:
        """Set the inn property."""
        if value not in [1, 2, 3, 4, -2, -4, None]:
            raise Exception("""inn must be `None` or one of {1,2,3,4,-2,-4}.""")
        self._cards[0].set_value("inn", value)

    @property
    def pidos(self) -> typing.Optional[int]:
        """Get or set the Part set ID for objective stress updates.  If this part set ID is given only those part IDs listed will use the objective stress update; therefore, OSU is ignored.
        """ # nopep8
        return self._cards[0].get_value("pidos")

    @pidos.setter
    def pidos(self, value: int) -> None:
        """Set the pidos property."""
        self._cards[0].set_value("pidos", value)

    @property
    def iacc(self) -> int:
        """Get or set the Implicit accuracy flag, turns on some specific accuracy considerations in implicit analysis at an extra CPU cost.
        -1: Off
        EQ.0: On (only for implicit)
        EQ.1: On (only for implicit)
        EQ.2: On (partially also for explicit, for compatibility when switching between implicit and explicit)
        """ # nopep8
        return self._cards[0].get_value("iacc")

    @iacc.setter
    def iacc(self, value: int) -> None:
        """Set the iacc property."""
        if value not in [0, -1, 1, 2, None]:
            raise Exception("""iacc must be `None` or one of {0,-1,1,2}.""")
        self._cards[0].set_value("iacc", value)

    @property
    def exacc(self) -> float:
        """Get or set the Explicit accuracy parameter:
        EQ.0.0: Off(default)
        GT.0.0: On(see Remark 5)
        """ # nopep8
        return self._cards[0].get_value("exacc")

    @exacc.setter
    def exacc(self, value: float) -> None:
        """Set the exacc property."""
        self._cards[0].set_value("exacc", value)

    @property
    def srtflg(self) -> int:
        """Get or set the Flag to process parts, contacts, nodal rigid bodies, and elements in a sequence sorted by their respective user IDs, regardless of the order they appear in the input files. When turned on, it ensures consistent results if the user modifies the order of these entities in the input files, such as by changing the order of include files between runs of the same model. See Remark 6.
        EQ.0:	Off(default)
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("srtflg")

    @srtflg.setter
    def srtflg(self, value: int) -> None:
        """Set the srtflg property."""
        if value not in [0, 1, None]:
            raise Exception("""srtflg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("srtflg", value)

    @property
    def pidos_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for pidos."""
        return self._get_set_link("PART", self.pidos)

    @pidos_link.setter
    def pidos_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pidos."""
        self.pidos = value.sid

