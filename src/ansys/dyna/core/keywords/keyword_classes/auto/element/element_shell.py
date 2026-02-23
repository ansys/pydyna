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

"""Module providing the ElementShell class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ElementShell(KeywordBase):
    """DYNA ELEMENT_SHELL keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementShell class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("eid", int, 0, 8, None),
                    Field("pid", int, 8, 8, None),
                    Field("n1", int, 16, 8, None),
                    Field("n2", int, 24, 8, None),
                    Field("n3", int, 32, 8, None),
                    Field("n4", int, 40, 8, None),
                    Field("n5", int, 48, 8, None),
                    Field("n6", int, 56, 8, None),
                    Field("n7", int, 64, 8, None),
                    Field("n8", int, 72, 8, None),
                ],
                None,
                name="elements",
                **kwargs,
            ),        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Get the table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """Set elements from the dataframe df"""
        self._cards[0].table = df

    @property
    def n1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n1, keyed by n1 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n1", "")

    def get_n1_link(self, n1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", n1, "")

    @property
    def n2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n2, keyed by n2 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n2", "")

    def get_n2_link(self, n2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", n2, "")

    @property
    def n3_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n3, keyed by n3 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n3", "")

    def get_n3_link(self, n3: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", n3, "")

    @property
    def n4_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n4, keyed by n4 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n4", "")

    def get_n4_link(self, n4: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", n4, "")

    @property
    def n5_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n5, keyed by n5 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n5", "")

    def get_n5_link(self, n5: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", n5, "")

    @property
    def n6_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n6, keyed by n6 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n6", "")

    def get_n6_link(self, n6: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", n6, "")

    @property
    def n7_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n7, keyed by n7 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n7", "")

    def get_n7_link(self, n7: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n7."""
        return self._get_link_by_attr("NODE", "nid", n7, "")

    @property
    def n8_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for n8, keyed by n8 value."""
        return self._get_links_from_table("NODE", "nid", "elements", "n8", "")

    def get_n8_link(self, n8: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n8."""
        return self._get_link_by_attr("NODE", "nid", n8, "")

    @property
    def pid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for pid, keyed by pid value."""
        return self._get_links_from_table("PART", "pid", "elements", "pid", "parts")

    def get_pid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", pid, "parts")

