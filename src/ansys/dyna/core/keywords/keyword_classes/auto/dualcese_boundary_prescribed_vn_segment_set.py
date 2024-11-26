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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DualceseBoundaryPrescribedVnSegmentSet(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_PRESCRIBED_VN_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_PRESCRIBED_VN_SEGMENT_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "idcomp",
                        int,
                        10,
                        10,
                        kwargs.get("idcomp")
                    ),
                    Field(
                        "dirx",
                        float,
                        20,
                        10,
                        kwargs.get("dirx")
                    ),
                    Field(
                        "diry",
                        float,
                        30,
                        10,
                        kwargs.get("diry")
                    ),
                    Field(
                        "dirz",
                        float,
                        40,
                        10,
                        kwargs.get("dirz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc_vn",
                        int,
                        0,
                        10,
                        kwargs.get("lc_vn")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lc_rho",
                        int,
                        30,
                        10,
                        kwargs.get("lc_rho")
                    ),
                    Field(
                        "lc_p",
                        int,
                        40,
                        10,
                        kwargs.get("lc_p")
                    ),
                    Field(
                        "lc_t",
                        int,
                        50,
                        10,
                        kwargs.get("lc_t")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sf_vn",
                        float,
                        0,
                        10,
                        kwargs.get("sf_vn", 1.0)
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "sf_rho",
                        float,
                        30,
                        10,
                        kwargs.get("sf_rho", 1.0)
                    ),
                    Field(
                        "sf_p",
                        float,
                        40,
                        10,
                        kwargs.get("sf_p", 1.0)
                    ),
                    Field(
                        "sf_t",
                        float,
                        50,
                        10,
                        kwargs.get("sf_t", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def idcomp(self) -> typing.Optional[int]:
        """Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_?COMPOSITION card with this ID
        """ # nopep8
        return self._cards[0].get_value("idcomp")

    @idcomp.setter
    def idcomp(self, value: int) -> None:
        self._cards[0].set_value("idcomp", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        self._cards[0].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        self._cards[0].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        self._cards[0].set_value("dirz", value)

    @property
    def lc_vn(self) -> typing.Optional[int]:
        """Get or set the Load curve or function ID to describe the normal velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The normal velocity is a constant with value SF_‌VN.
        EQ. - 1 : The normal velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_vn")

    @lc_vn.setter
    def lc_vn(self, value: int) -> None:
        self._cards[1].set_value("lc_vn", value)

    @property
    def lc_rho(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density versus time
        """ # nopep8
        return self._cards[1].get_value("lc_rho")

    @lc_rho.setter
    def lc_rho(self, value: int) -> None:
        self._cards[1].set_value("lc_rho", value)

    @property
    def lc_p(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the pressure versus time
        """ # nopep8
        return self._cards[1].get_value("lc_p")

    @lc_p.setter
    def lc_p(self, value: int) -> None:
        self._cards[1].set_value("lc_p", value)

    @property
    def lc_t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the temperature versus time
        """ # nopep8
        return self._cards[1].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: int) -> None:
        self._cards[1].set_value("lc_t", value)

    @property
    def sf_vn(self) -> float:
        """Get or set the Scale factor for LC_VN (default = 1.0)
        """ # nopep8
        return self._cards[2].get_value("sf_vn")

    @sf_vn.setter
    def sf_vn(self, value: float) -> None:
        self._cards[2].set_value("sf_vn", value)

    @property
    def sf_rho(self) -> float:
        """Get or set the Scale factor for LC_RHO (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_rho")

    @sf_rho.setter
    def sf_rho(self, value: float) -> None:
        self._cards[2].set_value("sf_rho", value)

    @property
    def sf_p(self) -> float:
        """Get or set the Scale factor for LC_P (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_p")

    @sf_p.setter
    def sf_p(self, value: float) -> None:
        self._cards[2].set_value("sf_p", value)

    @property
    def sf_t(self) -> float:
        """Get or set the Scale factor for LC_T (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf_t")

    @sf_t.setter
    def sf_t(self, value: float) -> None:
        self._cards[2].set_value("sf_t", value)

