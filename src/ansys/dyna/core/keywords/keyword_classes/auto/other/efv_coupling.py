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

"""Module providing the EfvCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCOUPLING_CARD0 = (
    FieldSchema("setfld", int, 0, 10, None),
    FieldSchema("setstr", int, 10, 10, None),
    FieldSchema("setyp", int, 20, 10, 0),
    FieldSchema("covlim", float, 30, 10, 0.5),
)

_EFVCOUPLING_CARD1 = (
    FieldSchema("setfld", int, 0, 10, None),
    FieldSchema("setstr", int, 10, 10, None),
)

class EfvCoupling(KeywordBase):
    """DYNA EFV_COUPLING keyword"""

    keyword = "EFV"
    subkeyword = "COUPLING"

    def __init__(self, **kwargs):
        """Initialize the EfvCoupling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVCOUPLING_CARD1,
                **kwargs,
            ),
        ]
    @property
    def setfld(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_BOX_MESH.  A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("setfld")

    @setfld.setter
    def setfld(self, value: int) -> None:
        """Set the setfld property."""
        self._cards[0].set_value("setfld", value)

    @property
    def setstr(self) -> typing.Optional[int]:
        """Get or set the Structure set for the LS-DYNA software�s structure
        """ # nopep8
        return self._cards[0].get_value("setstr")

    @setstr.setter
    def setstr(self, value: int) -> None:
        """Set the setstr property."""
        self._cards[0].set_value("setstr", value)

    @property
    def setyp(self) -> int:
        """Get or set the Type of SETSTR:
        EQ.0: segment set ID.
        EQ.1: part set ID.
        EQ.2: solid set ID.
        EQ.3: shell set ID.
        EQ.4: thick shell set ID.
        """ # nopep8
        return self._cards[0].get_value("setyp")

    @setyp.setter
    def setyp(self, value: int) -> None:
        """Set the setyp property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""setyp must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("setyp", value)

    @property
    def covlim(self) -> float:
        """Get or set the Cover fraction limit, beyond which an Eulerian cell partially covered by a structure should be blended to a neighbor Eulerian element
        """ # nopep8
        return self._cards[0].get_value("covlim")

    @covlim.setter
    def covlim(self, value: float) -> None:
        """Set the covlim property."""
        self._cards[0].set_value("covlim", value)

    @property
    def setfld(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_BOX_MESH.  A unique number must be specified.
        """ # nopep8
        return self._cards[1].get_value("setfld")

    @setfld.setter
    def setfld(self, value: int) -> None:
        """Set the setfld property."""
        self._cards[1].set_value("setfld", value)

    @property
    def setstr(self) -> typing.Optional[int]:
        """Get or set the Structure set for the LS-DYNA software�s structure
        """ # nopep8
        return self._cards[1].get_value("setstr")

    @setstr.setter
    def setstr(self, value: int) -> None:
        """Set the setstr property."""
        self._cards[1].set_value("setstr", value)

