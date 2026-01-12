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

"""Module providing the LoadSeismicSsiAuxId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADSEISMICSSIAUXID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_LOADSEISMICSSIAUXID_CARD1 = (
    FieldSchema("filename", str, 0, 80, None),
)

_LOADSEISMICSSIAUXID_CARD2 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("gmset", int, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("birth", float, 30, 10, 0.0),
    FieldSchema("death", float, 40, 10, 1e+28),
    FieldSchema("isg", int, 50, 10, 0),
    FieldSchema("memgm", int, 60, 10, 2500000),
)

class LoadSeismicSsiAuxId(KeywordBase):
    """DYNA LOAD_SEISMIC_SSI_AUX_ID keyword"""

    keyword = "LOAD"
    subkeyword = "SEISMIC_SSI_AUX_ID"

    def __init__(self, **kwargs):
        """Initialize the LoadSeismicSsiAuxId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSIAUXID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSIAUXID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSIAUXID_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of binary file containing recorded motions
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[2].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[2].set_value("ssid", value)

    @property
    def gmset(self) -> typing.Optional[int]:
        """Get or set the Identifier for set of recorded motions; see *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_?EMBEDDED
        """ # nopep8
        return self._cards[2].get_value("gmset")

    @gmset.setter
    def gmset(self, value: int) -> None:
        """Set the gmset property."""
        self._cards[2].set_value("gmset", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[2].set_value("sf", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[2].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[2].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[2].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[2].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
        """ # nopep8
        return self._cards[2].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        """Set the isg property."""
        if value not in [0, 1, None]:
            raise Exception("""isg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("isg", value)

    @property
    def memgm(self) -> int:
        """Get or set the Size in words of buffer allocated to read in recorded motions
        """ # nopep8
        return self._cards[2].get_value("memgm")

    @memgm.setter
    def memgm(self, value: int) -> None:
        """Set the memgm property."""
        self._cards[2].set_value("memgm", value)

