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

"""Module providing the BoundaryRadiationSetVfCalculate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYRADIATIONSETVFCALCULATE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 2),
    FieldSchema("rad_grp", int, 20, 10, 0),
    FieldSchema("file_no", int, 30, 10, 0),
    FieldSchema("block", int, 40, 10, 0),
    FieldSchema("nint", int, 50, 10, 0),
)

_BOUNDARYRADIATIONSETVFCALCULATE_CARD1 = (
    FieldSchema("selcid", int, 0, 10, 0),
    FieldSchema("semult", float, 10, 10, 1.0),
)

class BoundaryRadiationSetVfCalculate(KeywordBase):
    """DYNA BOUNDARY_RADIATION_SET_VF_CALCULATE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_SET_VF_CALCULATE"
    _link_fields = {
        "selcid": LinkType.DEFINE_CURVE,
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryRadiationSetVfCalculate class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONSETVFCALCULATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONSETVFCALCULATE_CARD1,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see also *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def type(self) -> int:
        """Get or set the Radiation type:
        EQ.2: radiation in enclosure.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def rad_grp(self) -> int:
        """Get or set the Radiation enclosure group ID. The segment sets from all radiation enclosure definitions with the same group ID are augmented to form a single enclosure definition. If RAD_GRP is not specified or set to zero, then the segments are placed in group zero. All segments defined by the _SEGMENT option are placed in set zero.
        """ # nopep8
        return self._cards[0].get_value("rad_grp")

    @rad_grp.setter
    def rad_grp(self, value: int) -> None:
        """Set the rad_grp property."""
        self._cards[0].set_value("rad_grp", value)

    @property
    def file_no(self) -> int:
        """Get or set the File number for view factor file. FILE_NO is added to viewfl_ to form the name of the file containing the view factors. For example, if FILE_NO is specified as 22, then the view factors are read from viewfl_22. For radiation enclosure group zero FILE_NO is ignored and view factors are read from viewfl. The same file may be used for different radiation enclosure group definitions.
        """ # nopep8
        return self._cards[0].get_value("file_no")

    @file_no.setter
    def file_no(self, value: int) -> None:
        """Set the file_no property."""
        self._cards[0].set_value("file_no", value)

    @property
    def block(self) -> int:
        """Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces.
        EQ.0: no blocking (default)
        EQ.1: blocking.
        """ # nopep8
        return self._cards[0].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        """Set the block property."""
        if value not in [0, 1, None]:
            raise Exception("""block must be `None` or one of {0,1}.""")
        self._cards[0].set_value("block", value)

    @property
    def nint(self) -> int:
        """Get or set the Number of integration points for viewfactor calculation.
        EQ.0: LS-DYNA determines the number of integration points based on the segment size and separation distance
        1 <= NINT <= 10: User specified number.
        """ # nopep8
        return self._cards[0].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        """Set the nint property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, None]:
            raise Exception("""nint must be `None` or one of {0,1,2,3,4,5,6,7,8,9,10}.""")
        self._cards[0].set_value("nint", value)

    @property
    def selcid(self) -> int:
        """Get or set the Load curve ID for surface emissivity, see *DEFINE_CURVE.
        GT.0: function versus time,
        EQ.0: use constant multiplier value, SEMULT (default),
        LT.0: function versus temperature.
        """ # nopep8
        return self._cards[1].get_value("selcid")

    @selcid.setter
    def selcid(self, value: int) -> None:
        """Set the selcid property."""
        self._cards[1].set_value("selcid", value)

    @property
    def semult(self) -> float:
        """Get or set the Curve multiplier for surface emissivity, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("semult")

    @semult.setter
    def semult(self, value: float) -> None:
        """Set the semult property."""
        self._cards[1].set_value("semult", value)

    @property
    def selcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for selcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.selcid:
                return kwd
        return None

    @selcid_link.setter
    def selcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for selcid."""
        self.selcid = value.lcid

    @property
    def ssid_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

