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

"""Module providing the DatabaseD3Max class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASED3MAX_CARD0 = (
    FieldSchema("dtcheck", float, 0, 10, None),
    FieldSchema("me", int, 10, 10, 1),
    FieldSchema("pstrs", int, 20, 10, 0),
    FieldSchema("pstrn", int, 30, 10, 0),
    FieldSchema("ifilt", int, 40, 10, 0),
    FieldSchema("output", int, 50, 10, 0),
    FieldSchema("fcutout", float, 60, 10, 0.0),
)

class DatabaseD3Max(KeywordBase):
    """DYNA DATABASE_D3MAX keyword"""

    keyword = "DATABASE"
    subkeyword = "D3MAX"

    def __init__(self, **kwargs):
        """Initialize the DatabaseD3Max class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASED3MAX_CARD0,
                **kwargs,
            ),        ]
    @property
    def dtcheck(self) -> typing.Optional[float]:
        """Get or set the Time step for checking and updating maximum values. For instance, if DTCHECK = 10-6, LS-DYNA will check and update the maximum values every 10-6 seconds (assuming for this example the time units are seconds). It will compare the current values (stress or strain) with the maximum values up to now. If the current values are larger, the maximum values will be replaced by the current values. Otherwise, the maximum values will remain unchanged
        """ # nopep8
        return self._cards[0].get_value("dtcheck")

    @dtcheck.setter
    def dtcheck(self, value: float) -> None:
        """Set the dtcheck property."""
        self._cards[0].set_value("dtcheck", value)

    @property
    def me(self) -> int:
        """Get or set the Method for extraction of stresses:
        EQ.1:	extracting max stress / strain during transient analysis.
        EQ.2 : extracting max stress / strain after transient analysis(not used)
        """ # nopep8
        return self._cards[0].get_value("me")

    @me.setter
    def me(self, value: int) -> None:
        """Set the me property."""
        if value not in [1, 2, None]:
            raise Exception("""me must be `None` or one of {1,2}.""")
        self._cards[0].set_value("me", value)

    @property
    def pstrs(self) -> int:
        """Get or set the Output principal stress:
        EQ.0:	no
        EQ.1 : yes
        """ # nopep8
        return self._cards[0].get_value("pstrs")

    @pstrs.setter
    def pstrs(self, value: int) -> None:
        """Set the pstrs property."""
        self._cards[0].set_value("pstrs", value)

    @property
    def pstrn(self) -> int:
        """Get or set the Output principal strain:
        EQ.0:	no
        EQ.1 : yes
        """ # nopep8
        return self._cards[0].get_value("pstrn")

    @pstrn.setter
    def pstrn(self, value: int) -> None:
        """Set the pstrn property."""
        self._cards[0].set_value("pstrn", value)

    @property
    def ifilt(self) -> int:
        """Get or set the Use filter:
        EQ.0:	no
        EQ.1 : use low pass 2nd order Butterworth filter
        """ # nopep8
        return self._cards[0].get_value("ifilt")

    @ifilt.setter
    def ifilt(self, value: int) -> None:
        """Set the ifilt property."""
        self._cards[0].set_value("ifilt", value)

    @property
    def output(self) -> int:
        """Get or set the Output format:
        EQ.0:	Write maximum stress / strain to d3max
        EQ.1 : Append the maximum stress / strain results to d3part
        EQ.2 : Write the maximum stress / strain results to d3part instead of the normal data that goes into d3part(negative time stamps are used in d3part to distinguish when this is done from the normal d3part output, which saves time history results for selected parts)
        """ # nopep8
        return self._cards[0].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        self._cards[0].set_value("output", value)

    @property
    def fcutout(self) -> float:
        """Get or set the Cutout frequency for Butterworth filter
        """ # nopep8
        return self._cards[0].get_value("fcutout")

    @fcutout.setter
    def fcutout(self, value: float) -> None:
        """Set the fcutout property."""
        self._cards[0].set_value("fcutout", value)

