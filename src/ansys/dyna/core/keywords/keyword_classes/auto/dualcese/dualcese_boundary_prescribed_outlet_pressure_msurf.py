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

"""Module providing the DualceseBoundaryPrescribedOutletPressureMsurf class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD0 = (
    FieldSchema("mspid", int, 0, 10, None),
    FieldSchema("iwall", int, 10, 10, None),
    FieldSchema("dir_x", float, 20, 10, None),
    FieldSchema("dir_y", float, 30, 10, None),
    FieldSchema("dir_z", float, 40, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD1 = (
    FieldSchema("lc_totp", int, 0, 10, None),
    FieldSchema("lc_tott", int, 10, 10, None),
    FieldSchema("lc_mfrt", int, 20, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD2 = (
    FieldSchema("sf_totp", int, 0, 10, 1),
    FieldSchema("sf_tott", int, 10, 10, 1),
    FieldSchema("sf_mfrt", int, 20, 10, 1),
)

class DualceseBoundaryPrescribedOutletPressureMsurf(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_PRESCRIBED_OUTLET_PRESSURE_MSURF keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_PRESCRIBED_OUTLET_PRESSURE_MSURF"

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundaryPrescribedOutletPressureMsurf class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDOUTLETPRESSUREMSURF_CARD2,
                **kwargs,
            ),
        ]
    @property
    def mspid(self) -> typing.Optional[int]:
        """Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("mspid")

    @mspid.setter
    def mspid(self, value: int) -> None:
        """Set the mspid property."""
        self._cards[0].set_value("mspid", value)

    @property
    def iwall(self) -> typing.Optional[int]:
        """Get or set the Artificial wall flag:
        EQ.0: Do not treat as an artificial wall if there is flow recirculation at this inlet boundary.
        EQ.1: Do treat this inlet boundary as an artificial wall whenever flow recirculation exists there.
        """ # nopep8
        return self._cards[0].get_value("iwall")

    @iwall.setter
    def iwall(self, value: int) -> None:
        """Set the iwall property."""
        self._cards[0].set_value("iwall", value)

    @property
    def dir_x(self) -> typing.Optional[float]:
        """Get or set the If this vector is nonzero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dir_x")

    @dir_x.setter
    def dir_x(self, value: float) -> None:
        """Set the dir_x property."""
        self._cards[0].set_value("dir_x", value)

    @property
    def dir_y(self) -> typing.Optional[float]:
        """Get or set the If this vector is nonzero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dir_y")

    @dir_y.setter
    def dir_y(self, value: float) -> None:
        """Set the dir_y property."""
        self._cards[0].set_value("dir_y", value)

    @property
    def dir_z(self) -> typing.Optional[float]:
        """Get or set the If this vector is nonzero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dir_z")

    @dir_z.setter
    def dir_z(self, value: float) -> None:
        """Set the dir_z property."""
        self._cards[0].set_value("dir_z", value)

    @property
    def lc_totp(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) to describe the total pressure as a function of time or function ID (see *DEFINE_FUNCTION) to give the total pressure as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0: total pressure is a constant with value SF_TOTP.
        """ # nopep8
        return self._cards[1].get_value("lc_totp")

    @lc_totp.setter
    def lc_totp(self, value: int) -> None:
        """Set the lc_totp property."""
        self._cards[1].set_value("lc_totp", value)

    @property
    def lc_tott(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) to describe the total temperature as a function of time or function ID (see *DEFINE_FUNCTION) to give the total temperature as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
        EQ.0: total temperature is a constant with value SF_TOTT.
        """ # nopep8
        return self._cards[1].get_value("lc_tott")

    @lc_tott.setter
    def lc_tott(self, value: int) -> None:
        """Set the lc_tott property."""
        self._cards[1].set_value("lc_tott", value)

    @property
    def lc_mfrt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the Mach number as a function of time or function ID to give the Mach number as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).EQ.0: Mach number is a constant with value SF_MACH.
        """ # nopep8
        return self._cards[1].get_value("lc_mfrt")

    @lc_mfrt.setter
    def lc_mfrt(self, value: int) -> None:
        """Set the lc_mfrt property."""
        self._cards[1].set_value("lc_mfrt", value)

    @property
    def sf_totp(self) -> int:
        """Get or set the Scale factor for LC_TOTP
        """ # nopep8
        return self._cards[2].get_value("sf_totp")

    @sf_totp.setter
    def sf_totp(self, value: int) -> None:
        """Set the sf_totp property."""
        self._cards[2].set_value("sf_totp", value)

    @property
    def sf_tott(self) -> int:
        """Get or set the Scale factor for LC_TOTT
        """ # nopep8
        return self._cards[2].get_value("sf_tott")

    @sf_tott.setter
    def sf_tott(self, value: int) -> None:
        """Set the sf_tott property."""
        self._cards[2].set_value("sf_tott", value)

    @property
    def sf_mfrt(self) -> int:
        """Get or set the Scale factor for LC_MFRT
        """ # nopep8
        return self._cards[2].get_value("sf_mfrt")

    @sf_mfrt.setter
    def sf_mfrt(self, value: int) -> None:
        """Set the sf_mfrt property."""
        self._cards[2].set_value("sf_mfrt", value)

