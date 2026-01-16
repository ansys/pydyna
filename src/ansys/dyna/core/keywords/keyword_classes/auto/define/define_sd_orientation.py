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

"""Module providing the DefineSdOrientation class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DEFINESDORIENTATION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSdOrientation(KeywordBase):
    """DYNA DEFINE_SD_ORIENTATION keyword"""

    keyword = "DEFINE"
    subkeyword = "SD_ORIENTATION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSdOrientation class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            TableCard(
                [
                    Field("vid", int, 0, 10, 0),
                    Field("iop", int, 10, 10, 0),
                    Field("xt", float, 20, 10, 0.0),
                    Field("yt", float, 30, 10, 0.0),
                    Field("zt", float, 40, 10, 0.0),
                    Field("nid1", int, 50, 10, 0),
                    Field("nid2", int, 60, 10, 0),
                ],
                None,
                name="vectors",
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSdOrientation.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESDORIENTATION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def vectors(self) -> pd.DataFrame:
        """Get the table of vectors."""
        return self._cards[0].table

    @vectors.setter
    def vectors(self, df: pd.DataFrame):
        """Set vectors from the dataframe df"""
        self._cards[0].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def nid1_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid1, keyed by nid1 value."""
        return self._get_links_from_table("NODE", "nid", "vectors", "nid1", "parts")

    def get_nid1_link(self, nid1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", nid1, "parts")

    @property
    def nid2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid2, keyed by nid2 value."""
        return self._get_links_from_table("NODE", "nid", "vectors", "nid2", "parts")

    def get_nid2_link(self, nid2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", nid2, "parts")

