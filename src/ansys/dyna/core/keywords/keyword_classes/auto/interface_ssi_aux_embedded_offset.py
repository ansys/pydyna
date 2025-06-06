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

"""Module providing the InterfaceSsiAuxEmbeddedOffset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceSsiAuxEmbeddedOffset(KeywordBase):
    """DYNA INTERFACE_SSI_AUX_EMBEDDED_OFFSET keyword"""

    keyword = "INTERFACE"
    subkeyword = "SSI_AUX_EMBEDDED_OFFSET"

    def __init__(self, **kwargs):
        """Initialize the InterfaceSsiAuxEmbeddedOffset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "heading",
                        str,
                        10,
                        70,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gmset",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "strid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "soilid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "spr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mpr",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

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

