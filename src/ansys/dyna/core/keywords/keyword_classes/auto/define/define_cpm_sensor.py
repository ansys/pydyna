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

"""Module providing the DefineCpmSensor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINECPMSENSOR_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("bagid", int, 10, 10, None),
    FieldSchema("ssid", int, 20, 10, None),
    FieldSchema("nfreq", int, 30, 10, 1),
    FieldSchema("off", float, 40, 10, None),
    FieldSchema("r", float, 50, 10, None),
    FieldSchema("height", float, 60, 10, None),
)

_DEFINECPMSENSOR_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpmSensor(KeywordBase):
    """DYNA DEFINE_CPM_SENSOR keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_SENSOR"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCpmSensor class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPMSENSOR_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpmSensor._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPMSENSOR_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this option
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def bagid(self) -> typing.Optional[int]:
        """Get or set the *AIRBAG_PARTICLE ID to apply this option
        """ # nopep8
        return self._cards[0].get_value("bagid")

    @bagid.setter
    def bagid(self, value: int) -> None:
        """Set the bagid property."""
        self._cards[0].set_value("bagid", value)

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
    def nfreq(self) -> int:
        """Get or set the Number of cycles to skip the particle-to-particle collision
        GT.0: Number of cycles
        LT.0: Time dependent load curve with ID NFREQ
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[0].set_value("nfreq", value)

    @property
    def off(self) -> typing.Optional[float]:
        """Get or set the Offset distance of the region's origin from the segment center along the normal direction
        """ # nopep8
        return self._cards[0].get_value("off")

    @off.setter
    def off(self, value: float) -> None:
        """Set the off property."""
        self._cards[0].set_value("off", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the region.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def height(self) -> typing.Optional[float]:
        """Get or set the Height of the cylinder for cylindrical domains:
        EQ.0: Spherical domain
        GT.0: Height of the cylindrical domain.
        """ # nopep8
        return self._cards[0].get_value("height")

    @height.setter
    def height(self, value: float) -> None:
        """Set the height property."""
        self._cards[0].set_value("height", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def ssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

