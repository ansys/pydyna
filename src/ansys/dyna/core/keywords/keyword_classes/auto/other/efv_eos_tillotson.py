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

"""Module providing the EfvEosTillotson class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSTILLOTSON_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("au", float, 10, 10, None),
    FieldSchema("bu", float, 20, 10, None),
    FieldSchema("al", float, 30, 10, None),
    FieldSchema("bl", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_EFVEOSTILLOTSON_CARD1 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("au", float, 10, 10, None),
    FieldSchema("bu", float, 20, 10, None),
    FieldSchema("al", float, 30, 10, None),
    FieldSchema("bl", float, 40, 10, None),
)

class EfvEosTillotson(KeywordBase):
    """DYNA EFV_EOS_TILLOTSON keyword"""

    keyword = "EFV"
    subkeyword = "EOS_TILLOTSON"

    def __init__(self, **kwargs):
        """Initialize the EfvEosTillotson class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSTILLOTSON_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSTILLOTSON_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def au(self) -> typing.Optional[float]:
        """Get or set the Parameter A
        """ # nopep8
        return self._cards[0].get_value("au")

    @au.setter
    def au(self, value: float) -> None:
        """Set the au property."""
        self._cards[0].set_value("au", value)

    @property
    def bu(self) -> typing.Optional[float]:
        """Get or set the Parameter B
        """ # nopep8
        return self._cards[0].get_value("bu")

    @bu.setter
    def bu(self, value: float) -> None:
        """Set the bu property."""
        self._cards[0].set_value("bu", value)

    @property
    def al(self) -> typing.Optional[float]:
        """Get or set the Parameter a
        """ # nopep8
        return self._cards[0].get_value("al")

    @al.setter
    def al(self, value: float) -> None:
        """Set the al property."""
        self._cards[0].set_value("al", value)

    @property
    def bl(self) -> typing.Optional[float]:
        """Get or set the Parameter b
        """ # nopep8
        return self._cards[0].get_value("bl")

    @bl.setter
    def bl(self, value: float) -> None:
        """Set the bl property."""
        self._cards[0].set_value("bl", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Parameter
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Parameter
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[1].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[1].set_value("eosid", value)

    @property
    def au(self) -> typing.Optional[float]:
        """Get or set the Parameter A
        """ # nopep8
        return self._cards[1].get_value("au")

    @au.setter
    def au(self, value: float) -> None:
        """Set the au property."""
        self._cards[1].set_value("au", value)

    @property
    def bu(self) -> typing.Optional[float]:
        """Get or set the Parameter B
        """ # nopep8
        return self._cards[1].get_value("bu")

    @bu.setter
    def bu(self, value: float) -> None:
        """Set the bu property."""
        self._cards[1].set_value("bu", value)

    @property
    def al(self) -> typing.Optional[float]:
        """Get or set the Parameter a
        """ # nopep8
        return self._cards[1].get_value("al")

    @al.setter
    def al(self, value: float) -> None:
        """Set the al property."""
        self._cards[1].set_value("al", value)

    @property
    def bl(self) -> typing.Optional[float]:
        """Get or set the Parameter b
        """ # nopep8
        return self._cards[1].get_value("bl")

    @bl.setter
    def bl(self, value: float) -> None:
        """Set the bl property."""
        self._cards[1].set_value("bl", value)

