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

"""Module providing the ConstrainedFemPeriTieBreak class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDFEMPERITIEBREAK_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("msid", int, 10, 10, None),
    FieldSchema("ssid", int, 20, 10, None),
    FieldSchema("ft", int, 30, 10, 100000000000000000000),
    FieldSchema("fs", int, 40, 10, 100000000000000000000),
)

class ConstrainedFemPeriTieBreak(KeywordBase):
    """DYNA CONSTRAINED_FEM_PERI_TIE_BREAK keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "FEM_PERI_TIE_BREAK"
    _link_fields = {
        "ft": LinkType.DEFINE_COORDINATE_SYSTEM,
        "msid": LinkType.PART,
        "ssid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedFemPeriTieBreak class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDFEMPERITIEBREAK_CARD0,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def msid(self) -> typing.Optional[int]:
        """Get or set the The FEM part ID.
        """ # nopep8
        return self._cards[0].get_value("msid")

    @msid.setter
    def msid(self, value: int) -> None:
        """Set the msid property."""
        self._cards[0].set_value("msid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the The peridynamic part ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def ft(self) -> int:
        """Get or set the The tensile pressure to break the tie.
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: int) -> None:
        """Set the ft property."""
        self._cards[0].set_value("ft", value)

    @property
    def fs(self) -> int:
        """Get or set the The shear pressure to break the tie.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: int) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def ft_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for ft."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.ft:
                return kwd
        return None

    @ft_link.setter
    def ft_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for ft."""
        self.ft = value.cid

    @property
    def msid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given msid."""
        return self._get_link_by_attr("PART", "pid", self.msid, "parts")

    @property
    def ssid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given ssid."""
        return self._get_link_by_attr("PART", "pid", self.ssid, "parts")

