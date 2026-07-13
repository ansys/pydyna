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

"""Module providing the EfvSymmetry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSYMMETRY_CARD0 = (
    FieldSchema("symx", int, 0, 10, 0),
    FieldSchema("symy", int, 10, 10, 0),
    FieldSchema("symz", int, 20, 10, 0),
)

class EfvSymmetry(KeywordBase):
    """DYNA EFV_SYMMETRY keyword"""

    keyword = "EFV"
    subkeyword = "SYMMETRY"

    def __init__(self, **kwargs):
        """Initialize the EfvSymmetry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSYMMETRY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def symx(self) -> int:
        """Get or set the The symmetric plane is the YZ-plane of the global coordinate system:
        EQ.0: Not applied
        EQ.1: Activated
        """ # nopep8
        return self._cards[0].get_value("symx")

    @symx.setter
    def symx(self, value: int) -> None:
        """Set the symx property."""
        if value not in [0, 1, None]:
            raise Exception("""symx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("symx", value)

    @property
    def symy(self) -> int:
        """Get or set the The symmetric plane is the XZ-plane of the global coordinate system:
        EQ.0: Not applied
        EQ.1: Activated
        """ # nopep8
        return self._cards[0].get_value("symy")

    @symy.setter
    def symy(self, value: int) -> None:
        """Set the symy property."""
        if value not in [0, 1, None]:
            raise Exception("""symy must be `None` or one of {0,1}.""")
        self._cards[0].set_value("symy", value)

    @property
    def symz(self) -> int:
        """Get or set the The symmetric plane is the XY-plane of the global coordinate system:
        EQ.0: Not applied
        EQ.1: Activated
        """ # nopep8
        return self._cards[0].get_value("symz")

    @symz.setter
    def symz(self, value: int) -> None:
        """Set the symz property."""
        if value not in [0, 1, None]:
            raise Exception("""symz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("symz", value)

