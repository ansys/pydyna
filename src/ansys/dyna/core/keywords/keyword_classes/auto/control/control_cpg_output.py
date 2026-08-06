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

"""Module providing the ControlCpgOutput class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCPGOUTPUT_CARD0 = (
    FieldSchema("outlv", int, 0, 10, 0),
)

_CONTROLCPGOUTPUT_CARD1 = (
    FieldSchema("vname", int, 0, 40, None),
    FieldSchema("ion", int, 40, 40, 0),
)

class ControlCpgOutput(KeywordBase):
    """DYNA CONTROL_CPG_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "CPG_OUTPUT"

    def __init__(self, **kwargs):
        """Initialize the ControlCpgOutput class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCPGOUTPUT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCPGOUTPUT_CARD1,
                **kwargs,
            ),
        ]
    @property
    def outlv(self) -> int:
        """Get or set the Global CPG data output level control:
        EQ.0 : Default.Every CPG variable is output.
        EQ.-1 : No CPG variable is output.
        """ # nopep8
        return self._cards[0].get_value("outlv")

    @outlv.setter
    def outlv(self, value: int) -> None:
        """Set the outlv property."""
        if value not in [0, -1, None]:
            raise Exception("""outlv must be `None` or one of {0,-1}.""")
        self._cards[0].set_value("outlv", value)

    @property
    def vname(self) -> typing.Optional[int]:
        """Get or set the CPG variable name � case insensitive. See table below for a complete list
        """ # nopep8
        return self._cards[1].get_value("vname")

    @vname.setter
    def vname(self, value: int) -> None:
        """Set the vname property."""
        self._cards[1].set_value("vname", value)

    @property
    def ion(self) -> int:
        """Get or set the Output status:
        EQ.0 : Default behavior depending on OUTLV.
        EQ.1 : On.
        EQ.2 : Off.
        """ # nopep8
        return self._cards[1].get_value("ion")

    @ion.setter
    def ion(self, value: int) -> None:
        """Set the ion property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ion must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("ion", value)

