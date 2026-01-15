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

"""Module providing the IcfdBoundaryConvectionTemp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ICFDBOUNDARYCONVECTIONTEMP_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("hlcid", int, 10, 10, None),
    FieldSchema("hsf", float, 20, 10, 1.0),
    FieldSchema("tblcid", int, 30, 10, None),
    FieldSchema("tbsf", float, 40, 10, 1.0),
)

class IcfdBoundaryConvectionTemp(KeywordBase):
    """DYNA ICFD_BOUNDARY_CONVECTION_TEMP keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_CONVECTION_TEMP"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryConvectionTemp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYCONVECTIONTEMP_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the heat transfer coefficient value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        """Set the hlcid property."""
        self._cards[0].set_value("hlcid", value)

    @property
    def hsf(self) -> float:
        """Get or set the Load curve scale factor applied on the heat transfer coefficient value.  (default=1.0)
        """ # nopep8
        return self._cards[0].get_value("hsf")

    @hsf.setter
    def hsf(self, value: float) -> None:
        """Set the hsf property."""
        self._cards[0].set_value("hsf", value)

    @property
    def tblcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the environment (i.e bulk) temperature value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("tblcid")

    @tblcid.setter
    def tblcid(self, value: int) -> None:
        """Set the tblcid property."""
        self._cards[0].set_value("tblcid", value)

    @property
    def tbsf(self) -> float:
        """Get or set the Load curve scale factor applied on the environment value.  (default=1.0)
        """ # nopep8
        return self._cards[0].get_value("tbsf")

    @tbsf.setter
    def tbsf(self, value: float) -> None:
        """Set the tbsf property."""
        self._cards[0].set_value("tbsf", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

