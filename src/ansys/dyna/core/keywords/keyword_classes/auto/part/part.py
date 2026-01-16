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

"""Module providing the Part class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.hourglass.hourglass import Hourglass

class Part(KeywordBase):
    """DYNA PART keyword"""

    keyword = "PART"
    subkeyword = "PART"
    _link_fields = {
        "mid": LinkType.MAT,
        "secid": LinkType.SECTION,
        "hgid": LinkType.HOURGLASS,
    }

    def __init__(self, **kwargs):
        """Initialize the Part class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    (
                        FieldSchema("heading", str, 0, 70, None),
                    ),
                    (
                        FieldSchema("pid", int, 0, 10, None),
                        FieldSchema("secid", int, 10, 10, None),
                        FieldSchema("mid", int, 20, 10, None),
                        FieldSchema("eosid", int, 30, 10, 0),
                        FieldSchema("hgid", int, 40, 10, 0),
                        FieldSchema("grav", int, 50, 10, 0),
                        FieldSchema("adpopt", int, 60, 10, None),
                        FieldSchema("tmid", int, 70, 10, 0),
                    ),
                ],
                None,
                None,
                "parts",
                **kwargs,
            ),        ]
    @property
    def parts(self) -> pd.DataFrame:
        """Gets the full table of parts."""
        return self._cards[0].table

    @parts.setter
    def parts(self, df: pd.DataFrame):
        """sets parts from the dataframe df."""
        self._cards[0].table = df

    @property
    def mid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all MAT_* keywords for mid, keyed by pid."""
        return self._get_table_group_links("MAT", "mid", "parts", "mid", "pid")

    def get_mid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for the given pid."""
        return self._get_table_group_link("MAT", "mid", "parts", "mid", "pid", pid)

    @property
    def secid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all SECTION_* keywords for secid, keyed by pid."""
        return self._get_table_group_links("SECTION", "secid", "parts", "secid", "pid")

    def get_secid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the SECTION_* keyword for the given pid."""
        return self._get_table_group_link("SECTION", "secid", "parts", "secid", "pid", pid)

    @property
    def hgid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all HOURGLASS_* keywords for hgid, keyed by pid."""
        return self._get_table_group_links("HOURGLASS", "hgid", "parts", "hgid", "pid")

    def get_hgid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the HOURGLASS_* keyword for the given pid."""
        return self._get_table_group_link("HOURGLASS", "hgid", "parts", "hgid", "pid", pid)

