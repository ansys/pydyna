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

"""Module providing the EfvFailure016 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILURE016_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("fs", float, 10, 10, None),
    FieldSchema("hexp", float, 20, 10, None),
    FieldSchema("rhof", float, 30, 10, None),
    FieldSchema("f", float, 40, 10, None),
)

class EfvFailure016(KeywordBase):
    """DYNA EFV_FAILURE_016 keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_016"

    def __init__(self, **kwargs):
        """Initialize the EfvFailure016 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE016_CARD0,
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
    def fs(self) -> typing.Optional[float]:
        """Get or set the Slope, F_s, on the pressure for determining failure
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def hexp(self) -> typing.Optional[float]:
        """Get or set the Heal exponential
        """ # nopep8
        return self._cards[0].get_value("hexp")

    @hexp.setter
    def hexp(self, value: float) -> None:
        """Set the hexp property."""
        self._cards[0].set_value("hexp", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Initial failure density, p_F
        """ # nopep8
        return self._cards[0].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        """Set the rhof property."""
        self._cards[0].set_value("rhof", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Initial fraction of yield, F
        """ # nopep8
        return self._cards[0].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[0].set_value("f", value)

