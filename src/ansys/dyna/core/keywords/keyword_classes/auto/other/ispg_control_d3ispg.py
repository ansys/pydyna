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

"""Module providing the IspgControlD3Ispg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGCONTROLD3ISPG_CARD0 = (
    FieldSchema("stress", int, 0, 10, None),
    FieldSchema("strainr", int, 10, 10, None),
    FieldSchema("vis", int, 20, 10, None),
    FieldSchema("temp", int, 30, 10, None),
    FieldSchema("curg", int, 40, 10, None),
)

class IspgControlD3Ispg(KeywordBase):
    """DYNA ISPG_CONTROL_D3ISPG keyword"""

    keyword = "ISPG"
    subkeyword = "CONTROL_D3ISPG"

    def __init__(self, **kwargs):
        """Initialize the IspgControlD3Ispg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLD3ISPG_CARD0,
                **kwargs,
            ),
        ]
    @property
    def stress(self) -> typing.Optional[int]:
        """Get or set the Flag to output stresses into d3ispg (see Remark 1):
        LE.0: No
        GT.0: Yes
        """ # nopep8
        return self._cards[0].get_value("stress")

    @stress.setter
    def stress(self, value: int) -> None:
        """Set the stress property."""
        self._cards[0].set_value("stress", value)

    @property
    def strainr(self) -> typing.Optional[int]:
        """Get or set the Flag to output shear strain rates into d3ispg (see Remark 2):
        LE.0: No
        GT.0: Yes
        """ # nopep8
        return self._cards[0].get_value("strainr")

    @strainr.setter
    def strainr(self, value: int) -> None:
        """Set the strainr property."""
        self._cards[0].set_value("strainr", value)

    @property
    def vis(self) -> typing.Optional[int]:
        """Get or set the Flag to output viscosity into d3ispg:
        LE.0: No
        GT.0: Yes
        """ # nopep8
        return self._cards[0].get_value("vis")

    @vis.setter
    def vis(self, value: int) -> None:
        """Set the vis property."""
        self._cards[0].set_value("vis", value)

    @property
    def temp(self) -> typing.Optional[int]:
        """Get or set the Flag to output temperature into d3ispg:
        LE.0: No
        GT.0: Yes
        """ # nopep8
        return self._cards[0].get_value("temp")

    @temp.setter
    def temp(self, value: int) -> None:
        """Set the temp property."""
        self._cards[0].set_value("temp", value)

    @property
    def curg(self) -> typing.Optional[int]:
        """Get or set the Flag to output material curing into d3ispg:
        LE.0: No
        GT.0: Yes
        """ # nopep8
        return self._cards[0].get_value("curg")

    @curg.setter
    def curg(self, value: int) -> None:
        """Set the curg property."""
        self._cards[0].set_value("curg", value)

