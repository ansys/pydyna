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

"""Module providing the ElementBeamOrientation class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ElementBeamOrientation(KeywordBase):
    """DYNA ELEMENT_BEAM_ORIENTATION keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_ORIENTATION"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementBeamOrientation class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    (
                        FieldSchema("eid", int, 0, 8, None),
                        FieldSchema("pid", int, 8, 8, None),
                        FieldSchema("n1", int, 16, 8, None),
                        FieldSchema("n2", int, 24, 8, None),
                        FieldSchema("n3", int, 32, 8, None),
                        FieldSchema("rt1", int, 40, 8, 0),
                        FieldSchema("rr1", int, 48, 8, 0),
                        FieldSchema("rt2", int, 56, 8, 0),
                        FieldSchema("rr2", int, 64, 8, 0),
                        FieldSchema("local", int, 72, 8, 2),
                    ),
                    (
                        FieldSchema("vx", float, 0, 10, 0.0),
                        FieldSchema("vy", float, 10, 10, 0.0),
                        FieldSchema("vz", float, 20, 10, 0.0),
                    ),
                ],
                None,
                None,
                "elements",
                **kwargs,
            ),
        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Gets the full table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """sets elements from the dataframe df."""
        self._cards[0].table = df

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

