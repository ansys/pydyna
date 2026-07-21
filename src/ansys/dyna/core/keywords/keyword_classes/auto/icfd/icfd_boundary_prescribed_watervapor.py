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

"""Module providing the IcfdBoundaryPrescribedWatervapor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDBOUNDARYPRESCRIBEDWATERVAPOR_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("mfrac", float, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("mfracu", int, 30, 10, 0),
)

class IcfdBoundaryPrescribedWatervapor(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_WATERVAPOR keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_WATERVAPOR"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPrescribedWatervapor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDWATERVAPOR_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part IDPID of the fluid surface with the water vapor mass fraction condition.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def mfrac(self) -> typing.Optional[float]:
        """Get or set the Water vapor mass fraction value. If MFRACU = 0, this value is the mass of water vapor in kg divided by the mass of dry air in kg. If MFRACU = 1, this value is the relative humidity and is in the range of [0,1]. Ignored if LCID is defined.
        """ # nopep8
        return self._cards[0].get_value("mfrac")

    @mfrac.setter
    def mfrac(self, value: float) -> None:
        """Set the mfrac property."""
        self._cards[0].set_value("mfrac", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID used to describe the water vapor mass fraction value as a function of time; see *DEFINE_?CURVE, *DEFINE_?CURVE_?FUNCTION, or *DEFINE_?FUNCTION.  If a DEFINE_?FUNCTION is used, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def mfracu(self) -> int:
        """Get or set the Water vapor mass fraction type:
        EQ.0:	The water vapor mass fraction value is the mass of the water vapor in kg divided by the mass of the dry air in kg.
        EQ.1 : The water vapor mass fraction value is the relative humidity with a range of[0,1].
        """ # nopep8
        return self._cards[0].get_value("mfracu")

    @mfracu.setter
    def mfracu(self, value: int) -> None:
        """Set the mfracu property."""
        if value not in [0, 1, None]:
            raise Exception("""mfracu must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mfracu", value)

