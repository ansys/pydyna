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

"""Module providing the ControlFormingScale class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLFORMINGSCALE_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("scpsx", float, 10, 10, None),
    FieldSchema("scpsy", float, 20, 10, None),
    FieldSchema("scpsz", float, 30, 10, None),
    FieldSchema("scpox", float, 40, 10, None),
    FieldSchema("scpoy", float, 50, 10, None),
    FieldSchema("scpoz", float, 60, 10, None),
)

class ControlFormingScale(KeywordBase):
    """DYNA CONTROL_FORMING_SCALE keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_SCALE"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingScale class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGSCALE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the blank
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def scpsx(self) -> typing.Optional[float]:
        """Get or set the Scale factor in the x-direction. We recommend a value between 1.001 and 1.01.
        """ # nopep8
        return self._cards[0].get_value("scpsx")

    @scpsx.setter
    def scpsx(self, value: float) -> None:
        """Set the scpsx property."""
        self._cards[0].set_value("scpsx", value)

    @property
    def scpsy(self) -> typing.Optional[float]:
        """Get or set the Scale factor in the y-direction. We recommend a value between 1.001 and 1.01.
        """ # nopep8
        return self._cards[0].get_value("scpsy")

    @scpsy.setter
    def scpsy(self, value: float) -> None:
        """Set the scpsy property."""
        self._cards[0].set_value("scpsy", value)

    @property
    def scpsz(self) -> typing.Optional[float]:
        """Get or set the Scale factor in the z-direction. We recommend a value between 1.001 and 1.01.
        """ # nopep8
        return self._cards[0].get_value("scpsz")

    @scpsz.setter
    def scpsz(self, value: float) -> None:
        """Set the scpsz property."""
        self._cards[0].set_value("scpsz", value)

    @property
    def scpox(self) -> typing.Optional[float]:
        """Get or set the x-component of the origin for scaling the blank. The x-component of all physical points with this x-component remain fixed.
        """ # nopep8
        return self._cards[0].get_value("scpox")

    @scpox.setter
    def scpox(self, value: float) -> None:
        """Set the scpox property."""
        self._cards[0].set_value("scpox", value)

    @property
    def scpoy(self) -> typing.Optional[float]:
        """Get or set the y-component of the origin for scaling the blank. The y-component of all physical points with this y-component remain fixed.
        """ # nopep8
        return self._cards[0].get_value("scpoy")

    @scpoy.setter
    def scpoy(self, value: float) -> None:
        """Set the scpoy property."""
        self._cards[0].set_value("scpoy", value)

    @property
    def scpoz(self) -> typing.Optional[float]:
        """Get or set the z-component of the origin for scaling the blank. The z-component of all physical points with this z-component remain fixed.
        """ # nopep8
        return self._cards[0].get_value("scpoz")

    @scpoz.setter
    def scpoz(self, value: float) -> None:
        """Set the scpoz property."""
        self._cards[0].set_value("scpoz", value)

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

