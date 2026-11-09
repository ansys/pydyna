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

"""Module providing the ElementMassPart class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

class ElementMassPart(KeywordBase):
    """DYNA ELEMENT_MASS_PART keyword"""

    keyword = "ELEMENT"
    subkeyword = "MASS_PART"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementMassPart class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("pid", int, 0, 8, None),
                    Field("addmass", float, 8, 16, 0.0),
                    Field("finmass", float, 24, 16, 0.0),
                    Field("lcid", int, 40, 16, None),
                ],
                None,
                name="elements",
                **kwargs,
            ),
        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Get the table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """Set elements from the dataframe df"""
        self._cards[0].table = df

    @property
    def pid(self) -> pd.Series:
        """Get or set the Part id, a unique number must be used.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def addmass(self) -> pd.Series:
        """Get or set the Added translational mass to be distributed to nodes of PID.
        """ # nopep8
        return self._cards[0].get_column("addmass")

    @addmass.setter
    def addmass(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the addmass column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("addmass", value)

    @property
    def finmass(self) -> pd.Series:
        """Get or set the Final translational mass of the part ID or part set ID.  The total mass of the PID or SID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.
        """ # nopep8
        return self._cards[0].get_column("finmass")

    @finmass.setter
    def finmass(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the finmass column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("finmass", value)

    @property
    def lcid(self) -> pd.Series:
        """Get or set the Optional load curve ID to scale the added mass at time = 0.  This curve defines the scale factor as a function versus time.  The curve must start at unity at t=0.  This option applies to deformable bodies only.
        """ # nopep8
        return self._cards[0].get_column("lcid")

    @lcid.setter
    def lcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the lcid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("lcid", value)

    @property
    def lcid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the lcid column, keyed by lcid value."""
        return self._get_column_links(self.lcid, lambda value: self._find_keyword("DEFINE", "CURVE", "lcid", value))

    @property
    def pid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for pid, keyed by pid value."""
        return self._get_links_from_table("PART", "pid", "elements", "pid", "parts")

    def get_pid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", pid, "parts")

