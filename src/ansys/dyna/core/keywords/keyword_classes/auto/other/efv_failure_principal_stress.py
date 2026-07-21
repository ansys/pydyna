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

"""Module providing the EfvFailurePrincipalStress class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILUREPRINCIPALSTRESS_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("sigt", float, 10, 10, 1e+20),
    FieldSchema("sigd", float, 20, 10, 1e+20),
    FieldSchema("crckid", int, 30, 10, None),
    FieldSchema("stchid", int, 40, 10, None),
)

class EfvFailurePrincipalStress(KeywordBase):
    """DYNA EFV_FAILURE_PRINCIPAL_STRESS keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_PRINCIPAL_STRESS"

    def __init__(self, **kwargs):
        """Initialize the EfvFailurePrincipalStress class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREPRINCIPALSTRESS_CARD0,
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
    def sigt(self) -> float:
        """Get or set the Principal tensile stress failure
        """ # nopep8
        return self._cards[0].get_value("sigt")

    @sigt.setter
    def sigt(self, value: float) -> None:
        """Set the sigt property."""
        self._cards[0].set_value("sigt", value)

    @property
    def sigd(self) -> float:
        """Get or set the Maximum principal stress difference divided by 2
        """ # nopep8
        return self._cards[0].get_value("sigd")

    @sigd.setter
    def sigd(self, value: float) -> None:
        """Set the sigd property."""
        self._cards[0].set_value("sigd", value)

    @property
    def crckid(self) -> typing.Optional[int]:
        """Get or set the ID for turning on/off the crack softening:
        EQ.0: Off
        GT.0: On(ID of * AUTODYN_FAILURE_ADD_CRACK_SOFTENING)
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

