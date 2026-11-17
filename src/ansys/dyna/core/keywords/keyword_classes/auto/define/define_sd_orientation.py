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
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
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
            ),
            OptionCardSet(
                option_spec = DefineSdOrientation._option_spec_list[0],
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
    def vid(self) -> pd.Series:
        """Get or set the Orientation vector ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_column("vid")

    @vid.setter
    def vid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the vid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("vid", value)

    @property
    def iop(self) -> pd.Series:
        """Get or set the Option:
        EQ.0: deflections/rotations are measured and forces/moments applied along the following orientation vector,
        EQ.1: deflections/rotations are measured and forces/moments applied along the axis between the two spring/damper nodes projected onto the plane normal to the following orientation vector,
        EQ.2: deflections/rotations are measured and forces/moments applied along a vector defined by the following two nodes,
        EQ.3: deflections/rotations are measured and forces/moments applied along the axis between the two spring/damper nodes projected onto the plane normal to the vector defined by the following two nodes.
        """ # nopep8
        return self._cards[0].get_column("iop")

    @iop.setter
    def iop(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the iop column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("iop", value)

    @property
    def xt(self) -> pd.Series:
        """Get or set the x-value of orientation vector. Define if IOP=0,1.
        """ # nopep8
        return self._cards[0].get_column("xt")

    @xt.setter
    def xt(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the xt column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("xt", value)

    @property
    def yt(self) -> pd.Series:
        """Get or set the y-value of orientation vector. Define if IOP=0,1.
        """ # nopep8
        return self._cards[0].get_column("yt")

    @yt.setter
    def yt(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the yt column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("yt", value)

    @property
    def zt(self) -> pd.Series:
        """Get or set the z-value of orientation vector. Define if IOP=0,1.
        """ # nopep8
        return self._cards[0].get_column("zt")

    @zt.setter
    def zt(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the zt column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("zt", value)

    @property
    def nid1(self) -> pd.Series:
        """Get or set the Node 1 ID. Define if IOP=2,3.
        """ # nopep8
        return self._cards[0].get_column("nid1")

    @nid1.setter
    def nid1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nid1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nid1", value)

    @property
    def nid2(self) -> pd.Series:
        """Get or set the Node 2 ID. Define if IOP=2,3.
        """ # nopep8
        return self._cards[0].get_column("nid2")

    @nid2.setter
    def nid2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nid2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nid2", value)

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
        return self._get_links_from_table("NODE", "nid", "vectors", "nid1", "")

    def get_nid1_link(self, nid1: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", nid1, "")

    @property
    def nid2_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for nid2, keyed by nid2 value."""
        return self._get_links_from_table("NODE", "nid", "vectors", "nid2", "")

    def get_nid2_link(self, nid2: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", nid2, "")

