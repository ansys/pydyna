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

_ICFDPARTVOL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("secid", int, 10, 10, None),
    FieldSchema("mid", int, 20, 10, None),
    FieldSchema("mid2", int, 30, 10, None),
)

_ICFDPARTVOL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class IcfdPartVol(KeywordBase):
    """DYNA ICFD_PART_VOL keyword"""

    keyword = "ICFD"
    subkeyword = "PART_VOL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the IcfdPartVol class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDPARTVOL_CARD0,
                **kwargs,
            ),
            TableCard(
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
            ),
            OptionCardSet(
                option_spec = IcfdPartVol._option_spec_list[0],
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
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Optional material ID to specify a second material in the volume in the case of level sets. The first material (MID) fills the positive level set values.
        """ # nopep8
        return self._cards[0].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        """Set the mid2 property."""
        self._cards[0].set_value("mid2", value)

    @property
    def nodes(self) -> pd.DataFrame:
        """Get the table of nodes."""
        return self._cards[1].table

    @nodes.setter
    def nodes(self, df: pd.DataFrame):
        """Set nodes from the dataframe df"""
        self._cards[1].table = df

    @property
    def spid1(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid1")

    @spid1.setter
    def spid1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid1 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid1", value)

    @property
    def spid2(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid2")

    @spid2.setter
    def spid2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid2 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid2", value)

    @property
    def spid3(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid3")

    @spid3.setter
    def spid3(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid3 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid3", value)

    @property
    def spid4(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid4")

    @spid4.setter
    def spid4(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid4 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid4", value)

    @property
    def spid5(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid5")

    @spid5.setter
    def spid5(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid5 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid5", value)

    @property
    def spid6(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid6")

    @spid6.setter
    def spid6(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid6 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid6", value)

    @property
    def spid7(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid7")

    @spid7.setter
    def spid7(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid7 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid7", value)

    @property
    def spid8(self) -> pd.Series:
        """Get or set the Part ID for the surface elements that define the volume mesh.
        """ # nopep8
        return self._cards[1].get_column("spid8")

    @spid8.setter
    def spid8(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the spid8 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("spid8", value)

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

