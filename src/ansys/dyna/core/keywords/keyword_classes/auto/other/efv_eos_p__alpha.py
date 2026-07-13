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

"""Module providing the EfvEosP_Alpha class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSP_ALPHA_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eosol", int, 10, 10, None),
    FieldSchema("comp", int, 20, 10, 0),
)

_EFVEOSP_ALPHA_CARD1 = (
    FieldSchema("rhopor", float, 0, 10, None),
    FieldSchema("cpor", float, 10, 10, None),
    FieldSchema("pe", float, 20, 10, None),
    FieldSchema("ps", float, 30, 10, None),
    FieldSchema("n", float, 40, 10, None),
)

class EfvEosP_Alpha(KeywordBase):
    """DYNA EFV_EOS_P-ALPHA keyword"""

    keyword = "EFV"
    subkeyword = "EOS_P-ALPHA"

    def __init__(self, **kwargs):
        """Initialize the EfvEosP_Alpha class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSP_ALPHA_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSP_ALPHA_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used (see *PART)..(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def eosol(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification for the solid (non-porous) material calling  *EFV_EOS_LINEAR or *EFV_EOS_POLYNOMIAL or *EFV_EOS_SHOCK
        """ # nopep8
        return self._cards[0].get_value("eosol")

    @eosol.setter
    def eosol(self, value: int) -> None:
        """Set the eosol property."""
        self._cards[0].set_value("eosol", value)

    @property
    def comp(self) -> int:
        """Get or set the Compaction curve formulation:
        EQ.0: Standard
        EQ.1: Alpha plastic
        """ # nopep8
        return self._cards[0].get_value("comp")

    @comp.setter
    def comp(self, value: int) -> None:
        """Set the comp property."""
        if value not in [0, 1, None]:
            raise Exception("""comp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("comp", value)

    @property
    def rhopor(self) -> typing.Optional[float]:
        """Get or set the Porous density
        """ # nopep8
        return self._cards[1].get_value("rhopor")

    @rhopor.setter
    def rhopor(self, value: float) -> None:
        """Set the rhopor property."""
        self._cards[1].set_value("rhopor", value)

    @property
    def cpor(self) -> typing.Optional[float]:
        """Get or set the Porous sound speed
        """ # nopep8
        return self._cards[1].get_value("cpor")

    @cpor.setter
    def cpor(self, value: float) -> None:
        """Set the cpor property."""
        self._cards[1].set_value("cpor", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Initial compaction pressure
        """ # nopep8
        return self._cards[1].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        """Set the pe property."""
        self._cards[1].set_value("pe", value)

    @property
    def ps(self) -> typing.Optional[float]:
        """Get or set the Solid compaction pressure
        """ # nopep8
        return self._cards[1].get_value("ps")

    @ps.setter
    def ps(self, value: float) -> None:
        """Set the ps property."""
        self._cards[1].set_value("ps", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Compaction exponent
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

