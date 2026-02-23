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

"""Module providing the BoundarySymmetryFailure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYSYMMETRYFAILURE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("fs", float, 10, 10, 0.0),
    FieldSchema("vtx", float, 20, 10, 0.0),
    FieldSchema("vty", float, 30, 10, 0.0),
    FieldSchema("vtz", float, 40, 10, 0.0),
    FieldSchema("vhx", float, 50, 10, 0.0),
    FieldSchema("vhy", float, 60, 10, 0.0),
    FieldSchema("vhz", float, 70, 10, 0.0),
)

class BoundarySymmetryFailure(KeywordBase):
    """DYNA BOUNDARY_SYMMETRY_FAILURE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SYMMETRY_FAILURE"
    _link_fields = {
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySymmetryFailure class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSYMMETRYFAILURE_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def fs(self) -> float:
        """Get or set the Tensile failure stress FS > 0.0. The average stress in the elements surrounding the boundary nodes in a direction perpendicular to the boundary is used.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def vtx(self) -> float:
        """Get or set the x-coordinate of tail of a normal vector originating on the wall (tail) and terminating in the body (head), i.e., vector points from the symmetry plane into the body.
        """ # nopep8
        return self._cards[0].get_value("vtx")

    @vtx.setter
    def vtx(self, value: float) -> None:
        """Set the vtx property."""
        self._cards[0].set_value("vtx", value)

    @property
    def vty(self) -> float:
        """Get or set the y-coordinate of tail.
        """ # nopep8
        return self._cards[0].get_value("vty")

    @vty.setter
    def vty(self, value: float) -> None:
        """Set the vty property."""
        self._cards[0].set_value("vty", value)

    @property
    def vtz(self) -> float:
        """Get or set the z-coordinate of tail.
        """ # nopep8
        return self._cards[0].get_value("vtz")

    @vtz.setter
    def vtz(self, value: float) -> None:
        """Set the vtz property."""
        self._cards[0].set_value("vtz", value)

    @property
    def vhx(self) -> float:
        """Get or set the x-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhx")

    @vhx.setter
    def vhx(self, value: float) -> None:
        """Set the vhx property."""
        self._cards[0].set_value("vhx", value)

    @property
    def vhy(self) -> float:
        """Get or set the y-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhy")

    @vhy.setter
    def vhy(self, value: float) -> None:
        """Set the vhy property."""
        self._cards[0].set_value("vhy", value)

    @property
    def vhz(self) -> float:
        """Get or set the z-coordinate of head.
        """ # nopep8
        return self._cards[0].get_value("vhz")

    @vhz.setter
    def vhz(self, value: float) -> None:
        """Set the vhz property."""
        self._cards[0].set_value("vhz", value)

    @property
    def ssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

