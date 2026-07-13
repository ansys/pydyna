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

_DEFINETRANSFORMATION_CARD2 = (
    FieldSchema("m11", float, 0, 10, None),
    FieldSchema("m12", float, 10, 10, None),
    FieldSchema("m13", float, 20, 10, None),
    FieldSchema("m14", float, 30, 10, None),
    FieldSchema("m21", float, 40, 10, None),
    FieldSchema("m22", float, 50, 10, None),
    FieldSchema("m23", float, 60, 10, None),
    FieldSchema("m24", float, 70, 10, None),
)

_DEFINETRANSFORMATION_CARD3 = (
    FieldSchema("m31", float, 0, 10, None),
    FieldSchema("m32", float, 10, 10, None),
    FieldSchema("m33", float, 20, 10, None),
    FieldSchema("m34", float, 30, 10, None),
    FieldSchema("m41", float, 40, 10, None),
    FieldSchema("m42", float, 50, 10, None),
    FieldSchema("m43", float, 60, 10, None),
    FieldSchema("m44", float, 70, 10, None),
)

_DEFINETRANSFORMATION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTransformation(KeywordBase):
    """DYNA DEFINE_TRANSFORMATION keyword"""

    keyword = "DEFINE"
    subkeyword = "TRANSFORMATION"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTransformation class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETRANSFORMATION_CARD0,
                **kwargs,
            ),
            TableCard(
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
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETRANSFORMATION_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETRANSFORMATION_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineTransformation._option_spec_list[0],
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
    def m11(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m11")

    @m11.setter
    def m11(self, value: float) -> None:
        """Set the m11 property."""
        self._cards[2].set_value("m11", value)

    @property
    def m12(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m12")

    @m12.setter
    def m12(self, value: float) -> None:
        """Set the m12 property."""
        self._cards[2].set_value("m12", value)

    @property
    def m13(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m13")

    @m13.setter
    def m13(self, value: float) -> None:
        """Set the m13 property."""
        self._cards[2].set_value("m13", value)

    @property
    def m14(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m14")

    @m14.setter
    def m14(self, value: float) -> None:
        """Set the m14 property."""
        self._cards[2].set_value("m14", value)

    @property
    def m21(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m21")

    @m21.setter
    def m21(self, value: float) -> None:
        """Set the m21 property."""
        self._cards[2].set_value("m21", value)

    @property
    def m22(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m22")

    @m22.setter
    def m22(self, value: float) -> None:
        """Set the m22 property."""
        self._cards[2].set_value("m22", value)

    @property
    def m23(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m23")

    @m23.setter
    def m23(self, value: float) -> None:
        """Set the m23 property."""
        self._cards[2].set_value("m23", value)

    @property
    def m24(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[2].get_value("m24")

    @m24.setter
    def m24(self, value: float) -> None:
        """Set the m24 property."""
        self._cards[2].set_value("m24", value)

    @property
    def m31(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m31")

    @m31.setter
    def m31(self, value: float) -> None:
        """Set the m31 property."""
        self._cards[3].set_value("m31", value)

    @property
    def m32(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m32")

    @m32.setter
    def m32(self, value: float) -> None:
        """Set the m32 property."""
        self._cards[3].set_value("m32", value)

    @property
    def m33(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m33")

    @m33.setter
    def m33(self, value: float) -> None:
        """Set the m33 property."""
        self._cards[3].set_value("m33", value)

    @property
    def m34(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m34")

    @m34.setter
    def m34(self, value: float) -> None:
        """Set the m34 property."""
        self._cards[3].set_value("m34", value)

    @property
    def m41(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m41")

    @m41.setter
    def m41(self, value: float) -> None:
        """Set the m41 property."""
        self._cards[3].set_value("m41", value)

    @property
    def m42(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m42")

    @m42.setter
    def m42(self, value: float) -> None:
        """Set the m42 property."""
        self._cards[3].set_value("m42", value)

    @property
    def m43(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m43")

    @m43.setter
    def m43(self, value: float) -> None:
        """Set the m43 property."""
        self._cards[3].set_value("m43", value)

    @property
    def m44(self) -> typing.Optional[float]:
        """Get or set the Parameters for OPTION = MATRIX. See Manual Table 0-1
        """ # nopep8
        return self._cards[3].get_value("m44")

    @m44.setter
    def m44(self, value: float) -> None:
        """Set the m44 property."""
        self._cards[3].set_value("m44", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

