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

"""Module providing the InterfaceSsiAuxEmbeddedConstrainedOffset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INTERFACESSIAUXEMBEDDEDCONSTRAINEDOFFSET_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_INTERFACESSIAUXEMBEDDEDCONSTRAINEDOFFSET_CARD1 = (
    FieldSchema("gmset", int, 0, 10, None),
    FieldSchema("strid", int, 10, 10, None),
    FieldSchema("soilid", int, 20, 10, None),
    FieldSchema("spr", int, 30, 10, None),
    FieldSchema("mpr", int, 40, 10, None),
)

class InterfaceSsiAuxEmbeddedConstrainedOffset(KeywordBase):
    """DYNA INTERFACE_SSI_AUX_EMBEDDED_CONSTRAINED_OFFSET keyword"""

    keyword = "INTERFACE"
    subkeyword = "SSI_AUX_EMBEDDED_CONSTRAINED_OFFSET"
    _link_fields = {
        "strid": LinkType.SET_SEGMENT,
        "soilid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the InterfaceSsiAuxEmbeddedConstrainedOffset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACESSIAUXEMBEDDEDCONSTRAINEDOFFSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INTERFACESSIAUXEMBEDDEDCONSTRAINEDOFFSET_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID. This is required and must be unique amongst all the contact interface IDs in the model.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A descriptor for the given ID.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def gmset(self) -> typing.Optional[int]:
        """Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
        """ # nopep8
        return self._cards[1].get_value("gmset")

    @gmset.setter
    def gmset(self, value: int) -> None:
        """Set the gmset property."""
        self._cards[1].set_value("gmset", value)

    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID of base of structure at soil-structure interface.
        """ # nopep8
        return self._cards[1].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[1].set_value("strid", value)

    @property
    def soilid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID of soil at soil-structure interface.
        """ # nopep8
        return self._cards[1].get_value("soilid")

    @soilid.setter
    def soilid(self, value: int) -> None:
        """Set the soilid property."""
        self._cards[1].set_value("soilid", value)

    @property
    def spr(self) -> typing.Optional[int]:
        """Get or set the Include the slave side in the *DATABASE_NCFORC and the
        *DATABASE_BINARY_INTFOR interface force files:
        EQ.1: slave side forces included.
        """ # nopep8
        return self._cards[1].get_value("spr")

    @spr.setter
    def spr(self, value: int) -> None:
        """Set the spr property."""
        self._cards[1].set_value("spr", value)

    @property
    def mpr(self) -> typing.Optional[int]:
        """Get or set the Include the master side in the *DATABASE_NCFORC and the
        *DATABASE_BINARY_INTFOR interface force files:
        EQ.1: master side forces included.
        """ # nopep8
        return self._cards[1].get_value("mpr")

    @mpr.setter
    def mpr(self, value: int) -> None:
        """Set the mpr property."""
        self._cards[1].set_value("mpr", value)

    @property
    def strid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for strid."""
        return self._get_set_link("SEGMENT", self.strid)

    @strid_link.setter
    def strid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for strid."""
        self.strid = value.sid

    @property
    def soilid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for soilid."""
        return self._get_set_link("SEGMENT", self.soilid)

    @soilid_link.setter
    def soilid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for soilid."""
        self.soilid = value.sid

