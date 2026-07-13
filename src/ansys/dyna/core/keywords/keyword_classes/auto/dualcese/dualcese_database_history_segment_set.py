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

"""Module providing the DualceseDatabaseHistorySegmentSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DUALCESEDATABASEHISTORYSEGMENTSET_CARD0 = (
    FieldSchema("dt", float, 0, 10, 0.0),
    FieldSchema("lcur", int, 10, 10, 0),
    FieldSchema("ioopt", int, 20, 10, 0),
)

_DUALCESEDATABASEHISTORYSEGMENTSET_CARD1 = (
    FieldSchema("ssid1", int, 0, 10, None),
    FieldSchema("ssid2", int, 10, 10, None),
    FieldSchema("ssid3", int, 20, 10, None),
    FieldSchema("ssid4", int, 30, 10, None),
    FieldSchema("ssid5", int, 40, 10, None),
    FieldSchema("ssid6", int, 50, 10, None),
    FieldSchema("ssid7", int, 60, 10, None),
    FieldSchema("ssid8", int, 70, 10, None),
)

class DualceseDatabaseHistorySegmentSet(KeywordBase):
    """DYNA DUALCESE_DATABASE_HISTORY_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "DATABASE_HISTORY_SEGMENT_SET"
    _link_fields = {
        "lcur": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DualceseDatabaseHistorySegmentSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYSEGMENTSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYSEGMENTSET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs. If DT is zero, no output is generated.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcur(self) -> int:
        """Get or set the Optional curve ID specifying the time interval between outputs. Use *DEFINE_CURVE to define the curve, The abscissa is time and the ordinate is time interval between outputs.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of the output frequency load curve defined by LCUR:
        EQ.1: When output is generated at time t_n, the next output time  t_(n + 1) is computed as(2 / 2) t_(n + 1) = t_n + LCUR (t_n)  .This is the default behavior.
        EQ.2: When output is generated at time t_n, the next output time t_(n + 1) is computed as(2 / 2) t_(n + 1) = t_n + LCUR (t_(n + 1))  .
        EQ.3: Output is generated for each abscissa point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        """Set the ioopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ioopt must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("ioopt", value)

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        """Set the ssid1 property."""
        self._cards[1].set_value("ssid1", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        """Set the ssid2 property."""
        self._cards[1].set_value("ssid2", value)

    @property
    def ssid3(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid3")

    @ssid3.setter
    def ssid3(self, value: int) -> None:
        """Set the ssid3 property."""
        self._cards[1].set_value("ssid3", value)

    @property
    def ssid4(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid4")

    @ssid4.setter
    def ssid4(self, value: int) -> None:
        """Set the ssid4 property."""
        self._cards[1].set_value("ssid4", value)

    @property
    def ssid5(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid5")

    @ssid5.setter
    def ssid5(self, value: int) -> None:
        """Set the ssid5 property."""
        self._cards[1].set_value("ssid5", value)

    @property
    def ssid6(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid6")

    @ssid6.setter
    def ssid6(self, value: int) -> None:
        """Set the ssid6 property."""
        self._cards[1].set_value("ssid6", value)

    @property
    def ssid7(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid7")

    @ssid7.setter
    def ssid7(self, value: int) -> None:
        """Set the ssid7 property."""
        self._cards[1].set_value("ssid7", value)

    @property
    def ssid8(self) -> typing.Optional[int]:
        """Get or set the ith dual CESE segment set ID (see *DUALCESE_SEGMENTSET). For each of these segment sets, an average value of each dual CESE output variable is output to the binout file at the times selected by the fields in Card 1.
        """ # nopep8
        return self._cards[1].get_value("ssid8")

    @ssid8.setter
    def ssid8(self, value: int) -> None:
        """Set the ssid8 property."""
        self._cards[1].set_value("ssid8", value)

    @property
    def lcur_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcur."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcur:
                return kwd
        return None

    @lcur_link.setter
    def lcur_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcur."""
        self.lcur = value.lcid

