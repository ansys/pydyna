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

"""Module providing the AleUpSwitch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALEUPSWITCH_CARD0 = (
    FieldSchema("upid", int, 0, 10, 0),
    FieldSchema("swtime", float, 10, 10, 1e+16),
)

_ALEUPSWITCH_CARD1 = (
    FieldSchema("fsi_id1", int, 0, 10, 0),
    FieldSchema("fsi_id2", int, 10, 10, 0),
    FieldSchema("fsi_id3", int, 20, 10, 0),
    FieldSchema("fsi_id4", int, 30, 10, 0),
    FieldSchema("fsi_id5", int, 40, 10, 0),
    FieldSchema("fsi_id6", int, 50, 10, 0),
    FieldSchema("fsi_id7", int, 60, 10, 0),
    FieldSchema("fsi_id8", int, 70, 10, 0),
)

_ALEUPSWITCH_CARD2 = (
    FieldSchema("sid", int, 0, 10, 0),
    FieldSchema("sidtype", int, 10, 10, 0),
    FieldSchema("mmgair", int, 20, 10, 0),
    FieldSchema("mmggas", int, 30, 10, 0),
)

class AleUpSwitch(KeywordBase):
    """DYNA ALE_UP_SWITCH keyword"""

    keyword = "ALE"
    subkeyword = "UP_SWITCH"

    def __init__(self, **kwargs):
        """Initialize the AleUpSwitch class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEUPSWITCH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEUPSWITCH_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEUPSWITCH_CARD2,
                **kwargs,
            ),        ]
    @property
    def upid(self) -> int:
        """Get or set the An ID defines a corresponding *AIRBAG_HYBRID_ID card for use in
        an ALE-method-switching-to-CV-method simulation. The simulation
        starts with ALE computational method, then switches to a CV (or UP)
        method at some given time.
        EQ.0: (or blank) The code will construct an equivalent
        *AIRBAG_HYBRID_ID card automatically internally, (default).
        The 3rd optional line is then a required input.
        NE.0: An ID points to a corresponding *AIRBAG_HYBRID_ID
        card which must be defined for use after the switch. If UPID is
        defined, do not define the 3rd optional card.
        """ # nopep8
        return self._cards[0].get_value("upid")

    @upid.setter
    def upid(self, value: int) -> None:
        """Set the upid property."""
        self._cards[0].set_value("upid", value)

    @property
    def swtime(self) -> float:
        """Get or set the The time at which the computation does a switch from an ALE-method-to-CV-method.
        """ # nopep8
        return self._cards[0].get_value("swtime")

    @swtime.setter
    def swtime(self, value: float) -> None:
        """Set the swtime property."""
        self._cards[0].set_value("swtime", value)

    @property
    def fsi_id1(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id1")

    @fsi_id1.setter
    def fsi_id1(self, value: int) -> None:
        """Set the fsi_id1 property."""
        self._cards[1].set_value("fsi_id1", value)

    @property
    def fsi_id2(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id2")

    @fsi_id2.setter
    def fsi_id2(self, value: int) -> None:
        """Set the fsi_id2 property."""
        self._cards[1].set_value("fsi_id2", value)

    @property
    def fsi_id3(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase
        """ # nopep8
        return self._cards[1].get_value("fsi_id3")

    @fsi_id3.setter
    def fsi_id3(self, value: int) -> None:
        """Set the fsi_id3 property."""
        self._cards[1].set_value("fsi_id3", value)

    @property
    def fsi_id4(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id4")

    @fsi_id4.setter
    def fsi_id4(self, value: int) -> None:
        """Set the fsi_id4 property."""
        self._cards[1].set_value("fsi_id4", value)

    @property
    def fsi_id5(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id5")

    @fsi_id5.setter
    def fsi_id5(self, value: int) -> None:
        """Set the fsi_id5 property."""
        self._cards[1].set_value("fsi_id5", value)

    @property
    def fsi_id6(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id6")

    @fsi_id6.setter
    def fsi_id6(self, value: int) -> None:
        """Set the fsi_id6 property."""
        self._cards[1].set_value("fsi_id6", value)

    @property
    def fsi_id7(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id7")

    @fsi_id7.setter
    def fsi_id7(self, value: int) -> None:
        """Set the fsi_id7 property."""
        self._cards[1].set_value("fsi_id7", value)

    @property
    def fsi_id8(self) -> int:
        """Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
        *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
        """ # nopep8
        return self._cards[1].get_value("fsi_id8")

    @fsi_id8.setter
    def fsi_id8(self, value: int) -> None:
        """Set the fsi_id8 property."""
        self._cards[1].set_value("fsi_id8", value)

    @property
    def sid(self) -> int:
        """Get or set the A set ID defines the Lagrangian parts which make up the airbag.
        """ # nopep8
        return self._cards[2].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[2].set_value("sid", value)

    @property
    def sidtype(self) -> int:
        """Get or set the Set ID type for the above SETID (following the conventions in
        *AIRBAG_HYBRID card).
        EQ.0: SID is a segment set ID (SGSID).
        NE.0: SID is a part set ID (PSID).
        """ # nopep8
        return self._cards[2].get_value("sidtype")

    @sidtype.setter
    def sidtype(self, value: int) -> None:
        """Set the sidtype property."""
        self._cards[2].set_value("sidtype", value)

    @property
    def mmgair(self) -> int:
        """Get or set the The AMMG (ALE multi-material group) ID of surrounding air.
        """ # nopep8
        return self._cards[2].get_value("mmgair")

    @mmgair.setter
    def mmgair(self, value: int) -> None:
        """Set the mmgair property."""
        self._cards[2].set_value("mmgair", value)

    @property
    def mmggas(self) -> int:
        """Get or set the The AMMG ID of inflator gas injected into the airbag.
        """ # nopep8
        return self._cards[2].get_value("mmggas")

    @mmggas.setter
    def mmggas(self, value: int) -> None:
        """Set the mmggas property."""
        self._cards[2].set_value("mmggas", value)

