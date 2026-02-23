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

"""Module providing the ConstrainedAdaptivity class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ConstrainedAdaptivity(KeywordBase):
    """DYNA CONSTRAINED_ADAPTIVITY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "ADAPTIVITY"
    _link_fields = {
        "dnid": LinkType.NODE,
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedAdaptivity class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("dnid", int, 0, 10, None),
                    Field("nid1", int, 10, 10, None),
                    Field("nid2", int, 20, 10, None),
                ],
                None,
                name="constrains",
                **kwargs,
            ),        ]
    @property
    def constrains(self) -> pd.DataFrame:
        """Get the table of constrains."""
        return self._cards[0].table

    @constrains.setter
    def constrains(self, df: pd.DataFrame):
        """Set constrains from the dataframe df"""
        self._cards[0].table = df

    @property
    def dnid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for dnid, keyed by dnid value."""
        return self._get_links_from_table("NODE", "nid", "constrains", "dnid", "")

    def get_dnid_link(self, dnid: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given dnid."""
        return self._get_link_by_attr("NODE", "nid", dnid, "")

    @property
    def nid1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid1, keyed by nid1 value."""
        return self._get_links_from_table("NODE", "nid", "constrains", "nid1", "")

    def get_nid1_link(self, nid1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", nid1, "")

    @property
    def nid2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid2, keyed by nid2 value."""
        return self._get_links_from_table("NODE", "nid", "constrains", "nid2", "")

    def get_nid2_link(self, nid2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", nid2, "")

