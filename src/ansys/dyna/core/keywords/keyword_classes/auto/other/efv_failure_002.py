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

"""Module providing the EfvFailure002 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILURE002_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("epspfail", float, 10, 10, None),
    FieldSchema("crckid", int, 20, 10, None),
    FieldSchema("stchid", int, 30, 10, None),
)

class EfvFailure002(KeywordBase):
    """DYNA EFV_FAILURE_002 keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_002"

    def __init__(self, **kwargs):
        """Initialize the EfvFailure002 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE002_CARD0,
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
    def epspfail(self) -> typing.Optional[float]:
        """Get or set the Minimum effective plastic strain for failure
        """ # nopep8
        return self._cards[0].get_value("epspfail")

    @epspfail.setter
    def epspfail(self, value: float) -> None:
        """Set the epspfail property."""
        self._cards[0].set_value("epspfail", value)

    @property
    def crckid(self) -> typing.Optional[int]:
        """Get or set the ID for turning on/off the crack softening:
        EQ.0: Off
        GT.0: On(ID of * AUTODYN_FAILURE_ADD_CRACK_SOFTENING
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
        GT.0: On(ID of * AUTODYN_ADD_FAILURE_STOCHASTIC)
        """ # nopep8
        return self._cards[0].get_value("stchid")

    @stchid.setter
    def stchid(self, value: int) -> None:
        """Set the stchid property."""
        self._cards[0].set_value("stchid", value)

