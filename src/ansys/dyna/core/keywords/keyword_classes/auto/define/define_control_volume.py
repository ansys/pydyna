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

"""Module providing the DefineControlVolume class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINECONTROLVOLUME_CARD0 = (
    FieldSchema("cvid", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
    FieldSchema("rho", float, 20, 10, None),
    FieldSchema("lcsrc", int, 30, 10, None),
)

_DEFINECONTROLVOLUME_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineControlVolume(KeywordBase):
    """DYNA DEFINE_CONTROL_VOLUME keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTROL_VOLUME"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineControlVolume class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONTROLVOLUME_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineControlVolume._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONTROLVOLUME_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def cvid(self) -> typing.Optional[int]:
        """Get or set the Control volume ID.
        """ # nopep8
        return self._cards[0].get_value("cvid")

    @cvid.setter
    def cvid(self, value: int) -> None:
        """Set the cvid property."""
        self._cards[0].set_value("cvid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID giving the control volume geometry
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Density of fluid contained in the domain (and entering the domain through the source curve)
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def lcsrc(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying the flow rate density (mass/time) into the domain, note that RHO must be set to a nonzero number for this to work
        """ # nopep8
        return self._cards[0].get_value("lcsrc")

    @lcsrc.setter
    def lcsrc(self, value: int) -> None:
        """Set the lcsrc property."""
        self._cards[0].set_value("lcsrc", value)

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

