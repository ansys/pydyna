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
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Part(KeywordBase):
    """DYNA PART keyword"""

    keyword = "PART"
    subkeyword = "PART"

    def __init__(self, **kwargs):
        """Initialize the Part class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    Card(
                            [
                                Field(
                                    "heading",
                                    str,
                                    0,
                                    70,
                                ),
                            ],
                    ),
                    Card(
                            [
                                Field(
                                    "pid",
                                    int,
                                    0,
                                    10,
                                ),
                                Field(
                                    "secid",
                                    int,
                                    10,
                                    10,
                                ),
                                Field(
                                    "mid",
                                    int,
                                    20,
                                    10,
                                ),
                                Field(
                                    "eosid",
                                    int,
                                    30,
                                    10,
                                ),
                                Field(
                                    "hgid",
                                    int,
                                    40,
                                    10,
                                ),
                                Field(
                                    "grav",
                                    int,
                                    50,
                                    10,
                                ),
                                Field(
                                    "adpopt",
                                    int,
                                    60,
                                    10,
                                ),
                                Field(
                                    "tmid",
                                    int,
                                    70,
                                    10,
                                ),
                            ],
                    ),
                ],
                None,
                None,
                "parts",
                **kwargs,
            ),
        ]

    @property
    def parts(self) -> pd.DataFrame:
        """Gets the full table of parts."""
        return self._cards[0].table

    @parts.setter
    def parts(self, df: pd.DataFrame):
        """sets parts from the dataframe df."""
        self._cards[0].table = df

    def _secid_link(self, secid: int) -> typing.Optional[KeywordBase]:
        """Get the SECTION object referenced by secid (link: 15)."""
        if secid is None or secid == 0:
            return None
        for kwd in self.deck.get_kwds_by_type("SECTION"):
            if hasattr(kwd, 'secid') and kwd.secid == secid:
                return kwd
        return None

    def _mid_link(self, mid: int) -> typing.Optional[KeywordBase]:
        """Get the MAT object referenced by mid (link: 14)."""
        if mid is None or mid == 0:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if hasattr(kwd, 'mid') and kwd.mid == mid:
                return kwd
        return None

    def get_referenced_keywords(self, level: int = -1) -> typing.List[KeywordBase]:
        """Get keywords referenced by this Part with optional recursion depth control.

        Parameters
        ----------
        level : int, optional
            Recursion depth. Default is -1 (unlimited).
            - level=-1: All downstream references (default)
            - level=1: Direct references only (SECTION, MAT)
            - level=2: Direct + their references
            - level=0: No references

        Returns
        -------
        List[KeywordBase]
            List of referenced keywords.

        Examples
        --------
        >>> part = deck.keywords[0]
        >>> refs = part.get_referenced_keywords()  # All downstream
        >>> refs = part.get_referenced_keywords(level=1)  # Direct only
        """
        if self.deck is None or level == 0:
            return []

        parts_df = self.parts
        if parts_df is None or parts_df.empty:
            return []

        referenced = []
        seen_ids = set()

        # Iterate through each row in the parts DataFrame
        for idx, row in parts_df.iterrows():
            # Use private link methods
            secid = row.get('secid')
            self._add_reference(self._secid_link(secid), referenced, seen_ids)

            mid = row.get('mid')
            self._add_reference(self._mid_link(mid), referenced, seen_ids)

        # Recurse if level allows (level > 1 or level == -1 for unlimited)
        if level > 1 or level == -1:
            next_level = -1 if level == -1 else level - 1
            for ref in list(referenced):
                if hasattr(ref, 'get_referenced_keywords'):
                    sub_refs = ref.get_referenced_keywords(level=next_level)
                    for sub_ref in sub_refs:
                        self._add_reference(sub_ref, referenced, seen_ids)

        return referenced

    def _add_reference(
        self,
        ref: typing.Optional[KeywordBase],
        referenced: typing.List[KeywordBase],
        seen_ids: typing.Set[typing.Tuple[str, typing.Any]]
    ) -> None:
        """Add a reference keyword to the list if not already present.

        Parameters
        ----------
        ref : KeywordBase or None
            The referenced keyword to add
        referenced : List[KeywordBase]
            List to append reference to
        seen_ids : Set[Tuple[str, Any]]
            Set of (keyword, id) tuples already seen
        """
        if ref is None:
            return

        ref_id = self._get_id_for_keyword(ref)
        key = (ref.keyword, ref_id)

        if key in seen_ids:
            return

        referenced.append(ref)
        seen_ids.add(key)

    def _get_id_for_keyword(self, kwd: KeywordBase) -> typing.Any:
        """Get the ID value for a keyword."""
        if kwd.keyword == "SECTION":
            return getattr(kwd, 'secid', id(kwd))
        elif kwd.keyword == "MAT":
            return getattr(kwd, 'mid', id(kwd))
        return id(kwd)

