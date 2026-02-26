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

"""Module providing the AleCouplingNodalDragTitle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALECOUPLINGNODALDRAGTITLE_CARD0 = (
    FieldSchema("coupid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_ALECOUPLINGNODALDRAGTITLE_CARD1 = (
    FieldSchema("strsid", int, 0, 10, None),
    FieldSchema("alesid", int, 10, 10, None),
    FieldSchema("strsty", int, 20, 10, 0),
    FieldSchema("alesty", int, 30, 10, 0),
)

_ALECOUPLINGNODALDRAGTITLE_CARD2 = (
    FieldSchema("start", float, 0, 10, 0.0),
    FieldSchema("end", float, 10, 10, 10000000000.0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("fcoef", int, 30, 10, 1),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("direcg", int, 60, 10, 1),
    FieldSchema("grav", float, 70, 10, 0.0),
)

class AleCouplingNodalDragTitle(KeywordBase):
    """DYNA ALE_COUPLING_NODAL_DRAG_TITLE keyword"""

    keyword = "ALE"
    subkeyword = "COUPLING_NODAL_DRAG_TITLE"

    def __init__(self, **kwargs):
        """Initialize the AleCouplingNodalDragTitle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALDRAGTITLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALDRAGTITLE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGNODALDRAGTITLE_CARD2,
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
        """Get or set the Set ID defining a part, part set or segment set ID of the particles (see *PART, *SET_PART or *SET_SEGMENT).The particles can be SPH or discrete elements
        """ # nopep8
        return self._cards[1].get_value("strsid")

    @strsid.setter
    def strsid(self, value: int) -> None:
        """Set the strsid property."""
        self._cards[1].set_value("strsid", value)

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_PART, and see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        """Set the alesid property."""
        self._cards[1].set_value("alesid", value)

    @property
    def strsty(self) -> int:
        """Get or set the Particle set type:
        EQ.0: Part set ID (PSID).
        EQ.1: Part ID (PID).
        EQ.2: Segment set ID (SSID).
        EQ.3: Node set ID (NSID).
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
    def fcoef(self) -> int:
        """Get or set the Drag coefficient scale factor or function ID to calculate drag coefficient
        GT.0:	Drag coefficient scale factor.
        LT.0 : The absolute value of FCOEF is the Function ID of the user provided function to calculate drag coefficient; See Remark 1
        """ # nopep8
        return self._cards[2].get_value("fcoef")

    @fcoef.setter
    def fcoef(self, value: int) -> None:
        """Set the fcoef property."""
        self._cards[2].set_value("fcoef", value)

    @property
    def direcg(self) -> int:
        """Get or set the Gravity force direction.
        EQ.1:	Global x direction
        EQ.2 : Global y direction
        EQ.3 : Global z direction
        """ # nopep8
        return self._cards[2].get_value("direcg")

    @direcg.setter
    def direcg(self, value: int) -> None:
        """Set the direcg property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""direcg must be `None` or one of {1,2,3}.""")
        self._cards[2].set_value("direcg", value)

    @property
    def grav(self) -> float:
        """Get or set the Gravity value. This value is used to calculate buoyance force
        """ # nopep8
        return self._cards[2].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[2].set_value("grav", value)

