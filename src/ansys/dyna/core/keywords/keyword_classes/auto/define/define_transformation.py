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

"""Module providing the DefineTransformation class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINETRANSFORMATION_CARD0 = (
    FieldSchema("tranid", int, 0, 10, None),
)

_DEFINETRANSFORMATION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTransformation(KeywordBase):
    """DYNA DEFINE_TRANSFORMATION keyword"""

    keyword = "DEFINE"
    subkeyword = "TRANSFORMATION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTransformation class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETRANSFORMATION_CARD0,
                **kwargs,
            ),            TableCard(
                [
                    Field("option", str, 0, 10, "MIRROR"),
                    Field("a1", float, 10, 10, None),
                    Field("a2", float, 20, 10, None),
                    Field("a3", float, 30, 10, None),
                    Field("a4", float, 40, 10, None),
                    Field("a5", float, 50, 10, None),
                    Field("a6", float, 60, 10, None),
                    Field("a7", float, 70, 10, None),
                ],
                None,
                name="transforms",
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineTransformation.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINETRANSFORMATION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tranid(self) -> typing.Optional[int]:
        """Get or set the Transform ID.
        """ # nopep8
        return self._cards[0].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        """Set the tranid property."""
        self._cards[0].set_value("tranid", value)

    @property
    def transforms(self) -> pd.DataFrame:
        """Get the table of transforms."""
        return self._cards[1].table

    @transforms.setter
    def transforms(self, df: pd.DataFrame):
        """Set transforms from the dataframe df"""
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

