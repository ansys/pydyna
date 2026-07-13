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

"""Module providing the EmOutputForces class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMOUTPUTFORCES_CARD0 = (
    FieldSchema("iout", int, 0, 10, 0),
    FieldSchema("outdt", float, 10, 10, 0.0),
    FieldSchema("lcoff", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
)

class EmOutputForces(KeywordBase):
    """DYNA EM_OUTPUT_FORCES keyword"""

    keyword = "EM"
    subkeyword = "OUTPUT_FORCES"

    def __init__(self, **kwargs):
        """Initialize the EmOutputForces class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMOUTPUTFORCES_CARD0,
                **kwargs,
            ),
        ]
    @property
    def iout(self) -> int:
        """Get or set the Type of the output.
        EQ.1: Outputs the binary file only.
        EQ.2: Outputs the binary file and generates a set of keyword files em_loadnid and em_loadlcid  giving the EM forces at each node at the given output time.
        EQ.3: Same as 2 except the forces are output as function of time.
        """ # nopep8
        return self._cards[0].get_value("iout")

    @iout.setter
    def iout(self, value: int) -> None:
        """Set the iout property."""
        self._cards[0].set_value("iout", value)

    @property
    def outdt(self) -> float:
        """Get or set the Time period at which the keyword files are generated. Setting OUTDT to 0.0 causes the output to be generated at each EM time step. The absolute value of a negative value refers to a time-dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("outdt")

    @outdt.setter
    def outdt(self, value: float) -> None:
        """Set the outdt property."""
        self._cards[0].set_value("outdt", value)

    @property
    def lcoff(self) -> typing.Optional[int]:
        """Get or set the Optional offset in the load curve IDs that are associated with the forces at each node when generating the keyword files.
        """ # nopep8
        return self._cards[0].get_value("lcoff")

    @lcoff.setter
    def lcoff(self, value: int) -> None:
        """Set the lcoff property."""
        self._cards[0].set_value("lcoff", value)

    @property
    def sf(self) -> float:
        """Get or set the Optional scaling factor that can be applied on the output forces when generating the keyword files.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

