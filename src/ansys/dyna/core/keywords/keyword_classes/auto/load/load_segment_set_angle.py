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

"""Module providing the LoadSegmentSetAngle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_LOADSEGMENTSETANGLE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
    FieldSchema("ioptp", int, 40, 10, 0),
    FieldSchema("ioptd", int, 50, 10, 0),
)

_LOADSEGMENTSETANGLE_CARD1 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("na", int, 20, 10, None),
    FieldSchema("ni", int, 30, 10, None),
)

class LoadSegmentSetAngle(KeywordBase):
    """DYNA LOAD_SEGMENT_SET_ANGLE keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT_SET_ANGLE"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "na": LinkType.NODE,
        "ni": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadSegmentSetAngle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTSETANGLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTSETANGLE_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve or function ID defining the traction as a function of the angle.  If IOPT=0 below, define the abscissa between 0 and 2??radians or 0 and 360 degrees if IOPD=1.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor on value of the load curve or function.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def ioptp(self) -> int:
        """Get or set the Flag for periodicity. The default (IOPTP=0) requires the load curve to be defined between 0 and 2?. This is useful, for example, for modeling an engine that is running at a steady state since each rotation will experience the same loading. To model a transient response, IOPTP=1 uses a load curve defined over the full range of angles, permitting a different response on the second and subsequent revolutions.
        """ # nopep8
        return self._cards[0].get_value("ioptp")

    @ioptp.setter
    def ioptp(self, value: int) -> None:
        """Set the ioptp property."""
        if value not in [0, 1, None]:
            raise Exception("""ioptp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ioptp", value)

    @property
    def ioptd(self) -> int:
        """Get or set the Flag for specifying if the load curve or function argument is in radians (IOPTD=0, the default) or degrees (IOPTD=1).
        """ # nopep8
        return self._cards[0].get_value("ioptd")

    @ioptd.setter
    def ioptd(self, value: int) -> None:
        """Set the ioptd property."""
        if value not in [0, 1, None]:
            raise Exception("""ioptd must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ioptd", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the The node specifying the tail of the rotating vector
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the The node specifying the head of the rotating vector
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def na(self) -> typing.Optional[int]:
        """Get or set the The node specifying the head of the vector defining the axis of rotation. The node N1 specifies the tail.
        """ # nopep8
        return self._cards[1].get_value("na")

    @na.setter
    def na(self, value: int) -> None:
        """Set the na property."""
        self._cards[1].set_value("na", value)

    @property
    def ni(self) -> typing.Optional[int]:
        """Get or set the The node specifying the orientation of the vector at an angle of zero. If the initial angle is zero, NI should be equal to N2.
        """ # nopep8
        return self._cards[1].get_value("ni")

    @ni.setter
    def ni(self, value: int) -> None:
        """Set the ni property."""
        self._cards[1].set_value("ni", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def na_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given na."""
        return self._get_link_by_attr("NODE", "nid", self.na, "parts")

    @property
    def ni_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given ni."""
        return self._get_link_by_attr("NODE", "nid", self.ni, "parts")

