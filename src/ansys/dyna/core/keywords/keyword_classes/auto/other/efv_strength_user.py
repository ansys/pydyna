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

"""Module providing the EfvStrengthUser class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTHUSER_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("sc2", float, 20, 10, None),
    FieldSchema("sc3", float, 30, 10, None),
    FieldSchema("sc4", float, 40, 10, None),
    FieldSchema("sc5", float, 50, 10, None),
    FieldSchema("sc6", float, 60, 10, None),
    FieldSchema("sc7", float, 70, 10, None),
)

_EFVSTRENGTHUSER_CARD1 = (
    FieldSchema("sc8", float, 0, 10, None),
    FieldSchema("sc9", float, 10, 10, None),
    FieldSchema("sc10", float, 20, 10, None),
    FieldSchema("sc11", float, 30, 10, None),
)

class EfvStrengthUser(KeywordBase):
    """DYNA EFV_STRENGTH_USER keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_USER"

    def __init__(self, **kwargs):
        """Initialize the EfvStrengthUser class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHUSER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHUSER_CARD1,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def sc2(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc2")

    @sc2.setter
    def sc2(self, value: float) -> None:
        """Set the sc2 property."""
        self._cards[0].set_value("sc2", value)

    @property
    def sc3(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc3")

    @sc3.setter
    def sc3(self, value: float) -> None:
        """Set the sc3 property."""
        self._cards[0].set_value("sc3", value)

    @property
    def sc4(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc4")

    @sc4.setter
    def sc4(self, value: float) -> None:
        """Set the sc4 property."""
        self._cards[0].set_value("sc4", value)

    @property
    def sc5(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc5")

    @sc5.setter
    def sc5(self, value: float) -> None:
        """Set the sc5 property."""
        self._cards[0].set_value("sc5", value)

    @property
    def sc6(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc6")

    @sc6.setter
    def sc6(self, value: float) -> None:
        """Set the sc6 property."""
        self._cards[0].set_value("sc6", value)

    @property
    def sc7(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[0].get_value("sc7")

    @sc7.setter
    def sc7(self, value: float) -> None:
        """Set the sc7 property."""
        self._cards[0].set_value("sc7", value)

    @property
    def sc8(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[1].get_value("sc8")

    @sc8.setter
    def sc8(self, value: float) -> None:
        """Set the sc8 property."""
        self._cards[1].set_value("sc8", value)

    @property
    def sc9(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[1].get_value("sc9")

    @sc9.setter
    def sc9(self, value: float) -> None:
        """Set the sc9 property."""
        self._cards[1].set_value("sc9", value)

    @property
    def sc10(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[1].get_value("sc10")

    @sc10.setter
    def sc10(self, value: float) -> None:
        """Set the sc10 property."""
        self._cards[1].set_value("sc10", value)

    @property
    def sc11(self) -> typing.Optional[float]:
        """Get or set the Strength parameter
        """ # nopep8
        return self._cards[1].get_value("sc11")

    @sc11.setter
    def sc11(self, value: float) -> None:
        """Set the sc11 property."""
        self._cards[1].set_value("sc11", value)

