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

"""Module providing the IcfdDatabaseBin class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDATABASEBIN_CARD0 = (
    FieldSchema("ioutlv", int, 0, 10, 0),
    FieldSchema("dtout", float, 10, 10, None),
    FieldSchema("iavg", int, 20, 10, None),
)

_ICFDDATABASEBIN_CARD1 = (
    FieldSchema("filename", str, 0, 80, None),
)

class IcfdDatabaseBin(KeywordBase):
    """DYNA ICFD_DATABASE_BIN keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_BIN"

    def __init__(self, **kwargs):
        """Initialize the IcfdDatabaseBin class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASEBIN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASEBIN_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ioutlv(self) -> int:
        """Get or set the Lfag to specify if the binary file should be output:.
        EQ.0 : No output file is generated.
        EQ.1 : The output file is generated with the default file name icfdbinot.
        EQ.2 : The output file is generated with a file name specified.See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("ioutlv")

    @ioutlv.setter
    def ioutlv(self, value: int) -> None:
        """Set the ioutlv property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ioutlv must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ioutlv", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD time step is used
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def iavg(self) -> typing.Optional[int]:
        """Get or set the Flag to specify whether the output quantities are instantaneous or average.
        EQ.0 : Instantaneous fluid quantities are printed
        EQ.1 : Time averaged fluid quantities are printed.
        """ # nopep8
        return self._cards[0].get_value("iavg")

    @iavg.setter
    def iavg(self, value: int) -> None:
        """Set the iavg property."""
        self._cards[0].set_value("iavg", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the binary file if IOUTLV = 2
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

