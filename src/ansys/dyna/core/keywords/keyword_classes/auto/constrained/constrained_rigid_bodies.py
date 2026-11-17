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

"""Module providing the ConstrainedRigidBodies class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

class ConstrainedRigidBodies(KeywordBase):
    """DYNA CONSTRAINED_RIGID_BODIES keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "RIGID_BODIES"
    _link_fields = {
        "pidl": LinkType.PART,
        "pidc": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedRigidBodies class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("pidl", int, 0, 10, None),
                    Field("pidc", int, 10, 10, None),
                    Field("iflag", int, 20, 10, None),
                ],
                None,
                name="pairs",
                **kwargs,
            ),
        ]
    @property
    def pairs(self) -> pd.DataFrame:
        """Get the table of pairs."""
        return self._cards[0].table

    @pairs.setter
    def pairs(self, df: pd.DataFrame):
        """Set pairs from the dataframe df"""
        self._cards[0].table = df

    @property
    def pidl(self) -> pd.Series:
        """Get or set the Lead rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_column("pidl")

    @pidl.setter
    def pidl(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pidl column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pidl", value)

    @property
    def pidc(self) -> pd.Series:
        """Get or set the Constrained rigid body part ID (see *PART) or constrained rigid body part set ID for the SET keyword option (see *SET_PART)
        """ # nopep8
        return self._cards[0].get_column("pidc")

    @pidc.setter
    def pidc(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pidc column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pidc", value)

    @property
    def iflag(self) -> pd.Series:
        """Get or set the This flag is meaningful if and only if the inertia properties of the lead part, PIDL, are defined in *PART_INERTIA.  See Remark 1.
        EQ.1: Update the center - of - gravity, the translational mass,and the inertia matrix of PIDL to reflect its merging with the constrained rigid body(PIDC).
        EQ.0: The merged PIDC will not affect the properties defined in *PART_INERTIA for PIDL since the properties are assumed to already account for merged parts.If the properties are not defined in a *PART_INERTIA definition, the inertia properties of PIDC will be computed from its nodal masses.
        """ # nopep8
        return self._cards[0].get_column("iflag")

    @iflag.setter
    def iflag(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the iflag column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("iflag", value)

    @property
    def pidl_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for pidl, keyed by pidl value."""
        return self._get_links_from_table("PART", "pid", "pairs", "pidl", "parts")

    def get_pidl_link(self, pidl: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pidl."""
        return self._get_link_by_attr("PART", "pid", pidl, "parts")

    @property
    def pidc_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for pidc, keyed by pidc value."""
        return self._get_links_from_table("PART", "pid", "pairs", "pidc", "parts")

    def get_pidc_link(self, pidc: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pidc."""
        return self._get_link_by_attr("PART", "pid", pidc, "parts")

