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
    FieldSchema("nfail", int, 70, 10, 0),
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
            ),
        ]
    @property
    def dtcheck(self) -> typing.Optional[float]:
        """Get or set the Time step for checking and updating maximum (or minimum) values. For instance, if DTCHECK = 10-6, LS-DYNA will check and update the maximum values every 10-6 seconds (assuming for this example the time units are seconds). It will compare the current values (stress or strain) with the maximum values up to now. If the current values are larger, the maximum values will be replaced by the current values. Otherwise, the maximum values will remain unchanged. If OUTPUT > 2, the minimum values are output instead of the maximum.The same algorithm applies to the minimum values, except that the minimum values are replaced if the current values are smaller.
        """ # nopep8
        return self._cards[0].get_value("dtcheck")

    @dtcheck.setter
    def dtcheck(self, value: float) -> None:
        """Set the dtcheck property."""
        self._cards[0].set_value("dtcheck", value)

    @property
    def me(self) -> int:
        """Get or set the Method for extracting of stresses/strains:
        EQ.1: Extract maximum (or minimum)  stress / strain during transient analysis.
        EQ.2: Extract maximum (or minimum)  stress / strain after transient analysis(not used)
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
        """Get or set the Output maximum (or minimum) principal stress in place of maximum or minimum normal stresses in the global coordinate system:
        EQ.0: No
        EQ.1: Yes
        """ # nopep8
        return self._cards[0].get_value("pstrs")

    @pstrs.setter
    def pstrs(self, value: int) -> None:
        """Set the pstrs property."""
        if value not in [0, 1, None]:
            raise Exception("""pstrs must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pstrs", value)

    @property
    def pstrn(self) -> int:
        """Get or set the Output maximum (or minimum) principal strain in place of the maximum or minimum normal strains in the global coordinate system (see Remark 2):
        EQ.0: No
        EQ.1: Yes
        """ # nopep8
        return self._cards[0].get_value("pstrn")

    @pstrn.setter
    def pstrn(self, value: int) -> None:
        """Set the pstrn property."""
        if value not in [0, 1, None]:
            raise Exception("""pstrn must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pstrn", value)

    @property
    def ifilt(self) -> int:
        """Get or set the Use filter:
        EQ.0: No
        EQ.1: Use low pass 2nd order Butterworth filter
        """ # nopep8
        return self._cards[0].get_value("ifilt")

    @ifilt.setter
    def ifilt(self, value: int) -> None:
        """Set the ifilt property."""
        if value not in [0, 1, None]:
            raise Exception("""ifilt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ifilt", value)

    @property
    def output(self) -> int:
        """Get or set the Output format and flag to determine whether minimum or maximum values are output:
        EQ.0: Write maximum stress / strain to d3max
        EQ.1: Append the maximum stress / strain results to d3part
        EQ.2: Write the maximum stress / strain results to d3part instead of the normal data that goes into d3part(negative time stamps are used in d3part to distinguish when this is done from the normal d3part output, which saves time history results for selected parts)
        EQ.3: Write minimum stress / strain to d3max
        EQ.4: Append the minimum stress / strain results to d3part
        EQ.5: Write the minimum stress / strain results to d3part instead of the normal data that goes into d3part(negative time stamps are used in d3part to distinguish when this is done from the normal d3part output, which saves time history results for selected parts
        EQ.6: Same as OUTPUT = 0, but map the results onto the original geometry instead of using the deformed geometry in d3max.
        EQ.7: Same as OUTPUT = 3, but map the results onto the original geometry instead of using the deformed geometry in d3max
        EQ.8:Write the maximum stress/strain results, the minimum stress/strain results, and the absolute maximum stress/strain results to d3max (state 1: maximum stress/strain; state 2: minimum stress/strain; state 3: absolute maximum stress/strain).
        EQ.9:	Same as OUTPUT = 8, but map the results onto the original geometry instead of using the deformed geometry in d3max
        """ # nopep8
        return self._cards[0].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, None]:
            raise Exception("""output must be `None` or one of {0,1,2,3,4,5,6,7,8,9}.""")
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

    @property
    def nfail(self) -> int:
        """Get or set the Flag to determine whether failed elements are kept in d3max:
        EQ.0 : Failed elements are removed from d3max(default)
        EQ.1 : Failed elements are kept in d3max so that their maximum stress/strain are included in the fringe plot.
        """ # nopep8
        return self._cards[0].get_value("nfail")

    @nfail.setter
    def nfail(self, value: int) -> None:
        """Set the nfail property."""
        if value not in [0, 1, None]:
            raise Exception("""nfail must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nfail", value)

