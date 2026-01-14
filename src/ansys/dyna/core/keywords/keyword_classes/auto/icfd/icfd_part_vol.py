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

"""Module providing the IcfdPartVol class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ICFDPARTVOL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("secid", int, 10, 10, None),
    FieldSchema("mid", int, 20, 10, None),
)

_ICFDPARTVOL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class IcfdPartVol(KeywordBase):
    """DYNA ICFD_PART_VOL keyword"""

    keyword = "ICFD"
    subkeyword = "PART_VOL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "spid1": LinkType.PART,
        "spid2": LinkType.PART,
        "spid3": LinkType.PART,
        "spid4": LinkType.PART,
        "spid5": LinkType.PART,
        "spid6": LinkType.PART,
        "spid7": LinkType.PART,
        "spid8": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdPartVol class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDPARTVOL_CARD0,
                **kwargs,
            ),            TableCard(
                [
                    Field("spid1", int, 0, 10, None),
                    Field("spid2", int, 10, 10, None),
                    Field("spid3", int, 20, 10, None),
                    Field("spid4", int, 30, 10, None),
                    Field("spid5", int, 40, 10, None),
                    Field("spid6", int, 50, 10, None),
                    Field("spid7", int, 60, 10, None),
                    Field("spid8", int, 70, 10, None),
                ],
                None,
                name="nodes",
                **kwargs,
            ),            OptionCardSet(
                option_spec = IcfdPartVol.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _ICFDPARTVOL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part identification for vol.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section identification defined in the *ICFD_SECTION section.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def nodes(self) -> pd.DataFrame:
        """Get the table of nodes."""
        return self._cards[1].table

    @nodes.setter
    def nodes(self, df: pd.DataFrame):
        """Set nodes from the dataframe df"""
        self._cards[1].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def spid1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid1, keyed by spid1 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid1", "parts")

    def get_spid1_link(self, spid1: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid1."""
        return self._get_link_by_attr("PART", "pid", spid1, "parts")

    @property
    def spid2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid2, keyed by spid2 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid2", "parts")

    def get_spid2_link(self, spid2: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid2."""
        return self._get_link_by_attr("PART", "pid", spid2, "parts")

    @property
    def spid3_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid3, keyed by spid3 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid3", "parts")

    def get_spid3_link(self, spid3: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid3."""
        return self._get_link_by_attr("PART", "pid", spid3, "parts")

    @property
    def spid4_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid4, keyed by spid4 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid4", "parts")

    def get_spid4_link(self, spid4: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid4."""
        return self._get_link_by_attr("PART", "pid", spid4, "parts")

    @property
    def spid5_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid5, keyed by spid5 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid5", "parts")

    def get_spid5_link(self, spid5: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid5."""
        return self._get_link_by_attr("PART", "pid", spid5, "parts")

    @property
    def spid6_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid6, keyed by spid6 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid6", "parts")

    def get_spid6_link(self, spid6: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid6."""
        return self._get_link_by_attr("PART", "pid", spid6, "parts")

    @property
    def spid7_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid7, keyed by spid7 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid7", "parts")

    def get_spid7_link(self, spid7: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid7."""
        return self._get_link_by_attr("PART", "pid", spid7, "parts")

    @property
    def spid8_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all PART keywords for spid8, keyed by spid8 value."""
        return self._get_links_from_table("PART", "pid", "nodes", "spid8", "parts")

    def get_spid8_link(self, spid8: int) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spid8."""
        return self._get_link_by_attr("PART", "pid", spid8, "parts")

