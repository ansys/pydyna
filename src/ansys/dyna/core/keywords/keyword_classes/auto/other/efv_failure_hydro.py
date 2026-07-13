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

"""Module providing the EfvFailureHydro class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILUREHYDRO_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("hydrotl", float, 10, 10, None),
    FieldSchema("reheal", int, 20, 10, 0),
    FieldSchema("crckid", int, 30, 10, None),
    FieldSchema("stchid", int, 40, 10, None),
)

class EfvFailureHydro(KeywordBase):
    """DYNA EFV_FAILURE_HYDRO keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_HYDRO"

    def __init__(self, **kwargs):
        """Initialize the EfvFailureHydro class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREHYDRO_CARD0,
                **kwargs,
            ),
        ]
    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[0].set_value("failid", value)

    @property
    def hydrotl(self) -> typing.Optional[float]:
        """Get or set the Hydrostatic tensile limit
        """ # nopep8
        return self._cards[0].get_value("hydrotl")

    @hydrotl.setter
    def hydrotl(self, value: float) -> None:
        """Set the hydrotl property."""
        self._cards[0].set_value("hydrotl", value)

    @property
    def reheal(self) -> int:
        """Get or set the Flag for turning on/off rehealing:
        EQ.0: On
        EQ.1: Off
        """ # nopep8
        return self._cards[0].get_value("reheal")

    @reheal.setter
    def reheal(self, value: int) -> None:
        """Set the reheal property."""
        if value not in [0, 1, None]:
            raise Exception("""reheal must be `None` or one of {0,1}.""")
        self._cards[0].set_value("reheal", value)

    @property
    def crckid(self) -> typing.Optional[int]:
        """Get or set the ID for turning on/off the crack softening:
        EQ.0: Off
        GT.0: On(ID of *EFV_FAILURE_CRACK_SOFTENING)
        """ # nopep8
        return self._cards[0].get_value("crckid")

    @crckid.setter
    def crckid(self, value: int) -> None:
        """Set the crckid property."""
        self._cards[0].set_value("crckid", value)

    @property
    def stchid(self) -> typing.Optional[int]:
        """Get or set the ID for turning on/off the stochastic failure:
        EQ.0: Off
        GT.0: On(ID of *EFV_ FAILURE_STOCHASTIC)
        """ # nopep8
        return self._cards[0].get_value("stchid")

    @stchid.setter
    def stchid(self, value: int) -> None:
        """Set the stchid property."""
        self._cards[0].set_value("stchid", value)

