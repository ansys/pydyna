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

"""Module providing the EfvMaterialCutoff class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVMATERIALCUTOFF_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("optnum", int, 10, 10, None),
)

_EFVMATERIALCUTOFF_CARD1 = (
    FieldSchema("propnum", int, 0, 10, None),
    FieldSchema("value", float, 10, 10, None),
)

class EfvMaterialCutoff(KeywordBase):
    """DYNA EFV_MATERIAL_CUTOFF keyword"""

    keyword = "EFV"
    subkeyword = "MATERIAL_CUTOFF"

    def __init__(self, **kwargs):
        """Initialize the EfvMaterialCutoff class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVMATERIALCUTOFF_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVMATERIALCUTOFF_CARD1,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID. The ID should be the same as the material identification (MID) in *PART definition for the finite volume Euler part.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def optnum(self) -> typing.Optional[int]:
        """Get or set the Number of chosen material type associated with a given keyword OPTION in ftp.lstc.com/outgoing/aquelet/EFV/EFV_materials.htm
        """ # nopep8
        return self._cards[0].get_value("optnum")

    @optnum.setter
    def optnum(self, value: int) -> None:
        """Set the optnum property."""
        self._cards[0].set_value("optnum", value)

    @property
    def propnum(self) -> typing.Optional[int]:
        """Get or set the Material property number in the table for the specified OPTNUM provided in ftp.lstc.com/outgoing/aquelet/autodyn/autodyn_materials.htm
        """ # nopep8
        return self._cards[1].get_value("propnum")

    @propnum.setter
    def propnum(self, value: int) -> None:
        """Set the propnum property."""
        self._cards[1].set_value("propnum", value)

    @property
    def value(self) -> typing.Optional[float]:
        """Get or set the Value of the material property
        """ # nopep8
        return self._cards[1].get_value("value")

    @value.setter
    def value(self, value: float) -> None:
        """Set the value property."""
        self._cards[1].set_value("value", value)

