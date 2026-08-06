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

"""Module providing the EmEpEikonal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMEPEIKONAL_CARD0 = (
    FieldSchema("eikid", int, 0, 10, None),
    FieldSchema("eikpst", int, 10, 10, 0),
    FieldSchema("eiksns", int, 20, 10, None),
    FieldSchema("eiksdf", int, 30, 10, None),
)

_EMEPEIKONAL_CARD1 = (
    FieldSchema("ftype", int, 0, 10, None),
    FieldSchema("ft", float, 10, 10, None),
    FieldSchema("fa", float, 20, 10, None),
)

_EMEPEIKONAL_CARD2 = (
    FieldSchema("solvetyp", int, 0, 10, 0),
)

class EmEpEikonal(KeywordBase):
    """DYNA EM_EP_EIKONAL keyword"""

    keyword = "EM"
    subkeyword = "EP_EIKONAL"
    _link_fields = {
        "eiksns": LinkType.SET_NODE,
        "eikpst": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpEikonal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPEIKONAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMEPEIKONAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMEPEIKONAL_CARD2,
                **kwargs,
            ),
        ]
    @property
    def eikid(self) -> typing.Optional[int]:
        """Get or set the ID of the Eikonal Solver.
        """ # nopep8
        return self._cards[0].get_value("eikid")

    @eikid.setter
    def eikid(self, value: int) -> None:
        """Set the eikid property."""
        self._cards[0].set_value("eikid", value)

    @property
    def eikpst(self) -> int:
        """Get or set the Part set ID on which the eikonal solve is performed.
        EQ.0:	Perform the eikonal solve on the whole conducting mesh
        """ # nopep8
        return self._cards[0].get_value("eikpst")

    @eikpst.setter
    def eikpst(self, value: int) -> None:
        """Set the eikpst property."""
        self._cards[0].set_value("eikpst", value)

    @property
    def eiksns(self) -> typing.Optional[int]:
        """Get or set the Node set ID where the seed (meaning initial activation time values) is set.
        """ # nopep8
        return self._cards[0].get_value("eiksns")

    @eiksns.setter
    def eiksns(self, value: int) -> None:
        """Set the eiksns property."""
        self._cards[0].set_value("eiksns", value)

    @property
    def eiksdf(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID for defining the seed value. Accepted arguments by the *DEFINE_FUNCTION are the node coordinates using the names 'x_node, y_node, z_node." If no *DEFINE_FUNCTION is used (EIKSDF = 0), the activation time is set to 0.0 on EIKSNS.
        """ # nopep8
        return self._cards[0].get_value("eiksdf")

    @eiksdf.setter
    def eiksdf(self, value: int) -> None:
        """Set the eiksdf property."""
        self._cards[0].set_value("eiksdf", value)

    @property
    def ftype(self) -> typing.Optional[int]:
        """Get or set the Type of foot current:
        EQ.1: Neic model.FA gives the foot current between the activation time and the activation time plus FT.
        """ # nopep8
        return self._cards[1].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        """Set the ftype property."""
        self._cards[1].set_value("ftype", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Duration of the foot current
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        """Set the ft property."""
        self._cards[1].set_value("ft", value)

    @property
    def fa(self) -> typing.Optional[float]:
        """Get or set the Amplitude of the foot current
        """ # nopep8
        return self._cards[1].get_value("fa")

    @fa.setter
    def fa(self, value: float) -> None:
        """Set the fa property."""
        self._cards[1].set_value("fa", value)

    @property
    def solvetyp(self) -> int:
        """Get or set the Type of eikonal solve:
        EQ.0:	Simple eikonal that only computes activation times.It can only simulate for one cycle.
        EQ.1 : Time - stepping multifront eikonal.This version supports simulating several cycles and reentries.
        """ # nopep8
        return self._cards[2].get_value("solvetyp")

    @solvetyp.setter
    def solvetyp(self, value: int) -> None:
        """Set the solvetyp property."""
        if value not in [0, 1, None]:
            raise Exception("""solvetyp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("solvetyp", value)

    @property
    def eiksns_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for eiksns."""
        return self._get_set_link("NODE", self.eiksns)

    @eiksns_link.setter
    def eiksns_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for eiksns."""
        self.eiksns = value.sid

    @property
    def eikpst_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for eikpst."""
        return self._get_set_link("PART", self.eikpst)

    @eikpst_link.setter
    def eikpst_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for eikpst."""
        self.eikpst = value.sid

