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

"""Module providing the InitialFieldSolid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALFIELDSOLID_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("nint", int, 10, 10, None),
    FieldSchema("nhisv", int, 20, 10, None),
)

_INITIALFIELDSOLID_CARD1 = (
    FieldSchema("fld1", int, 0, 10, None),
    FieldSchema("fld2", int, 10, 10, None),
    FieldSchema("fld3", int, 20, 10, None),
    FieldSchema("fld4", int, 30, 10, None),
    FieldSchema("fld5", int, 40, 10, None),
    FieldSchema("fld6", int, 50, 10, None),
    FieldSchema("fld7", int, 60, 10, None),
    FieldSchema("fld8", int, 70, 10, None),
)

class InitialFieldSolid(KeywordBase):
    """DYNA INITIAL_FIELD_SOLID keyword"""

    keyword = "INITIAL"
    subkeyword = "FIELD_SOLID"

    def __init__(self, **kwargs):
        """Initialize the InitialFieldSolid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALFIELDSOLID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALFIELDSOLID_CARD1,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def nint(self) -> typing.Optional[int]:
        """Get or set the Number of integration points (should correspond to the solid element formulation).
        """ # nopep8
        return self._cards[0].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        """Set the nint property."""
        self._cards[0].set_value("nint", value)

    @property
    def nhisv(self) -> typing.Optional[int]:
        """Get or set the Number of field variables. If NHISV exceeds the number of
        integration point field variables required by the constitutive model,
        only the number required is output; therefore, if in doubt, set NHISV to a large number.
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        """Set the nhisv property."""
        self._cards[0].set_value("nhisv", value)

    @property
    def fld1(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld1")

    @fld1.setter
    def fld1(self, value: int) -> None:
        """Set the fld1 property."""
        self._cards[1].set_value("fld1", value)

    @property
    def fld2(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld2")

    @fld2.setter
    def fld2(self, value: int) -> None:
        """Set the fld2 property."""
        self._cards[1].set_value("fld2", value)

    @property
    def fld3(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld3")

    @fld3.setter
    def fld3(self, value: int) -> None:
        """Set the fld3 property."""
        self._cards[1].set_value("fld3", value)

    @property
    def fld4(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld4")

    @fld4.setter
    def fld4(self, value: int) -> None:
        """Set the fld4 property."""
        self._cards[1].set_value("fld4", value)

    @property
    def fld5(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld5")

    @fld5.setter
    def fld5(self, value: int) -> None:
        """Set the fld5 property."""
        self._cards[1].set_value("fld5", value)

    @property
    def fld6(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld6")

    @fld6.setter
    def fld6(self, value: int) -> None:
        """Set the fld6 property."""
        self._cards[1].set_value("fld6", value)

    @property
    def fld7(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld7")

    @fld7.setter
    def fld7(self, value: int) -> None:
        """Set the fld7 property."""
        self._cards[1].set_value("fld7", value)

    @property
    def fld8(self) -> typing.Optional[int]:
        """Get or set the Data for the nth field (history) variable. NOTE that *MAT_TISSUE_DISPERSED only use FLD1 to FLD3 since NHISV = 3.
        """ # nopep8
        return self._cards[1].get_value("fld8")

    @fld8.setter
    def fld8(self, value: int) -> None:
        """Set the fld8 property."""
        self._cards[1].set_value("fld8", value)

