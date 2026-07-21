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

"""Module providing the IcfdInitialBin class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDINITIALBIN_CARD0 = (
    FieldSchema("ireadlv", int, 0, 10, 0),
    FieldSchema("iframe", int, 10, 10, None),
)

_ICFDINITIALBIN_CARD1 = (
    FieldSchema("filename", str, 0, 80, None),
)

class IcfdInitialBin(KeywordBase):
    """DYNA ICFD_INITIAL_BIN keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_BIN"

    def __init__(self, **kwargs):
        """Initialize the IcfdInitialBin class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALBIN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALBIN_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ireadlv(self) -> int:
        """Get or set the Flag to determines if the binary file should be read.
        EQ.0 : Off.This keyword is ignored.
        EQ.1 : On.The simulation is initialized by reading binary file icfdbintot.
        EQ.2 : The simulation is initialized by reading the binary file with the name given on the second line.
        """ # nopep8
        return self._cards[0].get_value("ireadlv")

    @ireadlv.setter
    def ireadlv(self, value: int) -> None:
        """Set the ireadlv property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ireadlv must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ireadlv", value)

    @property
    def iframe(self) -> typing.Optional[int]:
        """Get or set the Frame from the binary file that is used for initialization. If no specific frame is provided, the last frame from the binary file is used.
        """ # nopep8
        return self._cards[0].get_value("iframe")

    @iframe.setter
    def iframe(self, value: int) -> None:
        """Set the iframe property."""
        self._cards[0].set_value("iframe", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the binary file if IREADLV = 2.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

