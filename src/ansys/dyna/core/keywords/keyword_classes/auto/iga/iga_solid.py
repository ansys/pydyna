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

"""Module providing the IgaSolid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_IGASOLID_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("nisr", float, 20, 10, 0.0),
    FieldSchema("niss", float, 30, 10, 0.0),
    FieldSchema("nist", float, 40, 10, 0.0),
)

class IgaSolid(KeywordBase):
    """DYNA IGA_SOLID keyword"""

    keyword = "IGA"
    subkeyword = "SOLID"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IgaSolid class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGASOLID_CARD0,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Isogeometric solid (patch) ID, see Remark 1 and Remark 2. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nisr(self) -> float:
        """Get or set the Interpolation elements in the local r-direction, see Remark 3.
        LT.0.: ABS(NISR) is the average edge length of the interpolation elements in the local r - direction.
        EQ.0. : The number of interpolation elements per isogeometric element
        is equal to the polynomial degree in the local r - direction.
        GT.0. : NINT(NISR) is the number of interpolation elements per isogeometric element in the local r - direction..
        """ # nopep8
        return self._cards[0].get_value("nisr")

    @nisr.setter
    def nisr(self, value: float) -> None:
        """Set the nisr property."""
        self._cards[0].set_value("nisr", value)

    @property
    def niss(self) -> float:
        """Get or set the Interpolation elements in the local s-direction, see Remark 3.
        LT.0.: ABS(NISS) is the average edge length of the interpolation elements in the local s - direction.
        EQ.0. : The number of interpolation elements per isogeometric element
        is equal to the polynomial degree in the local s - direction.
        GT.0. : NINT(NISS) is the number of interpolation elements per
        isogeometric element in the local s - direction.
        """ # nopep8
        return self._cards[0].get_value("niss")

    @niss.setter
    def niss(self, value: float) -> None:
        """Set the niss property."""
        self._cards[0].set_value("niss", value)

    @property
    def nist(self) -> float:
        """Get or set the Interpolation elements in the local t-direction, see Remark 3.
        LT.0.: ABS(NIST) is the average edge length of the interpolation elements in the local t - direction.
        EQ.0. : The number of interpolation elements per isogeometric element
        is equal to the polynomial degree in the local t - direction.
        GT.0. : NINT(NIST) is the number of interpolation elements per
        isogeometric element in the local t - direction.
        """ # nopep8
        return self._cards[0].get_value("nist")

    @nist.setter
    def nist(self, value: float) -> None:
        """Set the nist property."""
        self._cards[0].set_value("nist", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

