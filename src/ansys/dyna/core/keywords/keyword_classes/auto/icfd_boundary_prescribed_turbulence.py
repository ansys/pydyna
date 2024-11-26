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

class IcfdBoundaryPrescribedTurbulence(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_TURBULENCE keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_TURBULENCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "vtype",
                        int,
                        10,
                        10,
                        kwargs.get("vtype", 1)
                    ),
                    Field(
                        "imp",
                        int,
                        20,
                        10,
                        kwargs.get("imp", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "ks",
                        float,
                        40,
                        10,
                        kwargs.get("ks")
                    ),
                    Field(
                        "cs",
                        float,
                        50,
                        10,
                        kwargs.get("cs")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def vtype(self) -> int:
        """Get or set the Variable type.
        EQ.1:Turbulence kineticenergy.
        EQ.2:Turbulent dissipation rate.
        EQ.3:Specific dissipation rate.
        EQ.4:Modified turbulent viscosity.
        """ # nopep8
        return self._cards[0].get_value("vtype")

    @vtype.setter
    def vtype(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""vtype must be one of {1,2,3,4}""")
        self._cards[0].set_value("vtype", value)

    @property
    def imp(self) -> int:
        """Get or set the Imposition method:
        EQ.0:Direct imposition through value specified by LCID.
        EQ.1:Using turbulent Intensity specified by LCID if VTYPE = 1.Using turbulence length scale specified by LCID if VTYPE = 2,3 and 4.
        EQ.2:Using turbulence viscosity ratio specified by LCID.Only available for VTYPE = 2 and VTYPE = 3.
        """ # nopep8
        return self._cards[0].get_value("imp")

    @imp.setter
    def imp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""imp must be one of {0,1,2}""")
        self._cards[0].set_value("imp", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the variable value as a function of time; see *DEFINE_‌CURVE, *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION. If a *DEFINE_‌FUNCTION is used, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time, k, e, mut).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def ks(self) -> typing.Optional[float]:
        """Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.
        """ # nopep8
        return self._cards[0].get_value("ks")

    @ks.setter
    def ks(self, value: float) -> None:
        self._cards[0].set_value("ks", value)

    @property
    def cs(self) -> typing.Optional[float]:
        """Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.
        """ # nopep8
        return self._cards[0].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        self._cards[0].set_value("cs", value)

