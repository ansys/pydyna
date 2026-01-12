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

"""Module providing the DefineTableMatrix class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINETABLEMATRIX_CARD0 = (
    FieldSchema("tbid", int, 0, 10, None),
    FieldSchema("filename", str, 10, 70, None),
)

_DEFINETABLEMATRIX_CARD1 = (
    FieldSchema("nrow", int, 0, 10, None),
    FieldSchema("ncol", int, 10, 10, None),
    FieldSchema("srow", float, 20, 10, 1.0),
    FieldSchema("scol", float, 30, 10, 1.0),
    FieldSchema("sval", float, 40, 10, 1.0),
    FieldSchema("orow", float, 50, 10, None),
    FieldSchema("ocol", float, 60, 10, None),
    FieldSchema("oval", float, 70, 10, None),
)

class DefineTableMatrix(KeywordBase):
    """DYNA DEFINE_TABLE_MATRIX keyword"""

    keyword = "DEFINE"
    subkeyword = "TABLE_MATRIX"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTableMatrix class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETABLEMATRIX_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLEMATRIX_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineTableMatrix.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A negative value of TBID switches the interpretation of rows and columns in the read matrix.
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[0].set_value("tbid", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of file containing table data (stored as a matrix).
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def nrow(self) -> typing.Optional[int]:
        """Get or set the Number of rows in the matrix, same as number of rows in the file FILENAME
        """ # nopep8
        return self._cards[1].get_value("nrow")

    @nrow.setter
    def nrow(self, value: int) -> None:
        """Set the nrow property."""
        self._cards[1].set_value("nrow", value)

    @property
    def ncol(self) -> typing.Optional[int]:
        """Get or set the Number of columns in the matrix, same as number of data entries per row in the file FILENAME
        """ # nopep8
        return self._cards[1].get_value("ncol")

    @ncol.setter
    def ncol(self, value: int) -> None:
        """Set the ncol property."""
        self._cards[1].set_value("ncol", value)

    @property
    def srow(self) -> float:
        """Get or set the Scale factor for row data
        """ # nopep8
        return self._cards[1].get_value("srow")

    @srow.setter
    def srow(self, value: float) -> None:
        """Set the srow property."""
        self._cards[1].set_value("srow", value)

    @property
    def scol(self) -> float:
        """Get or set the Scale factor for column data
        """ # nopep8
        return self._cards[1].get_value("scol")

    @scol.setter
    def scol(self, value: float) -> None:
        """Set the scol property."""
        self._cards[1].set_value("scol", value)

    @property
    def sval(self) -> float:
        """Get or set the Scale factor for matrix values
        """ # nopep8
        return self._cards[1].get_value("sval")

    @sval.setter
    def sval(self, value: float) -> None:
        """Set the sval property."""
        self._cards[1].set_value("sval", value)

    @property
    def orow(self) -> typing.Optional[float]:
        """Get or set the Offset for row data
        """ # nopep8
        return self._cards[1].get_value("orow")

    @orow.setter
    def orow(self, value: float) -> None:
        """Set the orow property."""
        self._cards[1].set_value("orow", value)

    @property
    def ocol(self) -> typing.Optional[float]:
        """Get or set the Offset for column data
        """ # nopep8
        return self._cards[1].get_value("ocol")

    @ocol.setter
    def ocol(self, value: float) -> None:
        """Set the ocol property."""
        self._cards[1].set_value("ocol", value)

    @property
    def oval(self) -> typing.Optional[float]:
        """Get or set the Offset for matrix values
        """ # nopep8
        return self._cards[1].get_value("oval")

    @oval.setter
    def oval(self, value: float) -> None:
        """Set the oval property."""
        self._cards[1].set_value("oval", value)

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

