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

class DualceseBoundaryPrescribedHybridMsurf(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_PRESCRIBED_HYBRID_MSURF keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_PRESCRIBED_HYBRID_MSURF"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mspid",
                        int,
                        0,
                        10,
                        kwargs.get("mspid")
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
                        "lc_z1",
                        int,
                        0,
                        10,
                        kwargs.get("lc_z1")
                    ),
                    Field(
                        "lc_ra",
                        int,
                        10,
                        10,
                        kwargs.get("lc_ra")
                    ),
                    Field(
                        "lc_u",
                        int,
                        20,
                        10,
                        kwargs.get("lc_u")
                    ),
                    Field(
                        "lc_v",
                        int,
                        30,
                        10,
                        kwargs.get("lc_v")
                    ),
                    Field(
                        "lc_w",
                        int,
                        40,
                        10,
                        kwargs.get("lc_w")
                    ),
                    Field(
                        "lc_d1",
                        int,
                        50,
                        10,
                        kwargs.get("lc_d1")
                    ),
                    Field(
                        "lc_da",
                        int,
                        60,
                        10,
                        kwargs.get("lc_da")
                    ),
                    Field(
                        "lc_db",
                        int,
                        70,
                        10,
                        kwargs.get("lc_db")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc_p",
                        float,
                        0,
                        10,
                        kwargs.get("lc_p")
                    ),
                    Field(
                        "lc_t",
                        float,
                        10,
                        10,
                        kwargs.get("lc_t")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sf_z1",
                        float,
                        0,
                        10,
                        kwargs.get("sf_z1", 1.0)
                    ),
                    Field(
                        "sf_ra",
                        float,
                        10,
                        10,
                        kwargs.get("sf_ra", 1.0)
                    ),
                    Field(
                        "sf_u",
                        float,
                        20,
                        10,
                        kwargs.get("sf_u", 1.0)
                    ),
                    Field(
                        "sf_v",
                        float,
                        30,
                        10,
                        kwargs.get("sf_v", 1.0)
                    ),
                    Field(
                        "sf_w",
                        float,
                        40,
                        10,
                        kwargs.get("sf_w", 1.0)
                    ),
                    Field(
                        "sf_d1",
                        float,
                        50,
                        10,
                        kwargs.get("sf_d1", 1.0)
                    ),
                    Field(
                        "sf_da",
                        float,
                        60,
                        10,
                        kwargs.get("sf_da", 1.0)
                    ),
                    Field(
                        "sf_db",
                        float,
                        70,
                        10,
                        kwargs.get("sf_db", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc_p",
                        float,
                        0,
                        10,
                        kwargs.get("lc_p")
                    ),
                    Field(
                        "lc_t",
                        float,
                        10,
                        10,
                        kwargs.get("lc_t")
                    ),
                ],
            ),
        ]

    @property
    def mspid(self) -> typing.Optional[int]:
        """Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("mspid")

    @mspid.setter
    def mspid(self, value: int) -> None:
        self._cards[0].set_value("mspid", value)

    @property
    def idcomp(self) -> typing.Optional[int]:
        """Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_‌COMPOSITION card with this ID [Not yet available
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
    def lc_z1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or function ID to describe the volume fraction of material 1 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The volume fraction is a constant with value SF_?Z1.
        EQ. - 1 : The volume fraction is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_z1")

    @lc_z1.setter
    def lc_z1(self, value: int) -> None:
        self._cards[1].set_value("lc_z1", value)

    @property
    def lc_ra(self) -> typing.Optional[int]:
        """Get or set the Load curve or function ID to describe the mass fraction of reactant (material  ) with respect to the explosive mixture (material 2) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The mass fraction is a constant with value SF_RA.
        EQ.-1:	The mass fraction is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_ra")

    @lc_ra.setter
    def lc_ra(self, value: int) -> None:
        self._cards[1].set_value("lc_ra", value)

    @property
    def lc_u(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The x-component of velocity is a constant with value SF_U.
        EQ.-1:The x-component of velocity is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_u")

    @lc_u.setter
    def lc_u(self, value: int) -> None:
        self._cards[1].set_value("lc_u", value)

    @property
    def lc_v(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.EQ.0:The y-component of velocity is a constant with value SF_V.

        EQ. - 1:	The  y - component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_v")

    @lc_v.setter
    def lc_v(self, value: int) -> None:
        self._cards[1].set_value("lc_v", value)

    @property
    def lc_w(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The  z-component of velocity is a constant with value SF_W.
        EQ.-1:The  z-component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_w")

    @lc_w.setter
    def lc_w(self, value: int) -> None:
        self._cards[1].set_value("lc_w", value)

    @property
    def lc_d1(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the density of the first multiphase material as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The density of the first multiphase material is a constant with value SF_D1
        EQ.-1:The density of the first multiphase material is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lc_d1")

    @lc_d1.setter
    def lc_d1(self, value: int) -> None:
        self._cards[1].set_value("lc_d1", value)

    @property
    def lc_da(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the density of the reactant (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The density of the reactant is a constant with value SF_DA.
        EQ.-1:	The density of the reactant is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_da")

    @lc_da.setter
    def lc_da(self, value: int) -> None:
        self._cards[1].set_value("lc_da", value)

    @property
    def lc_db(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the density of the product (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The density of the product is a constant with value SF_DB.
        EQ.-1:	The density of the product is computed by the solver
        """ # nopep8
        return self._cards[1].get_value("lc_db")

    @lc_db.setter
    def lc_db(self, value: int) -> None:
        self._cards[1].set_value("lc_db", value)

    @property
    def lc_p(self) -> typing.Optional[float]:
        """Get or set the Load curve or defined function ID to describe the pressure as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The pressure is a constant with value SF_P
        EQ.-1:	The pressure is computed by the solver
        """ # nopep8
        return self._cards[2].get_value("lc_p")

    @lc_p.setter
    def lc_p(self, value: float) -> None:
        self._cards[2].set_value("lc_p", value)

    @property
    def lc_t(self) -> typing.Optional[float]:
        """Get or set the Load curve or defined function ID to describe the temperature as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0:	The temperature is a constant with value SF_T.
        EQ.-1:	The temperature is computed by the solver
        """ # nopep8
        return self._cards[2].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: float) -> None:
        self._cards[2].set_value("lc_t", value)

    @property
    def sf_z1(self) -> float:
        """Get or set the Scale factor for LC_Z1
        """ # nopep8
        return self._cards[3].get_value("sf_z1")

    @sf_z1.setter
    def sf_z1(self, value: float) -> None:
        self._cards[3].set_value("sf_z1", value)

    @property
    def sf_ra(self) -> float:
        """Get or set the Scale factor for LC_RA
        """ # nopep8
        return self._cards[3].get_value("sf_ra")

    @sf_ra.setter
    def sf_ra(self, value: float) -> None:
        self._cards[3].set_value("sf_ra", value)

    @property
    def sf_u(self) -> float:
        """Get or set the Scale factor for LC_U
        """ # nopep8
        return self._cards[3].get_value("sf_u")

    @sf_u.setter
    def sf_u(self, value: float) -> None:
        self._cards[3].set_value("sf_u", value)

    @property
    def sf_v(self) -> float:
        """Get or set the Scale factor for LC_V
        """ # nopep8
        return self._cards[3].get_value("sf_v")

    @sf_v.setter
    def sf_v(self, value: float) -> None:
        self._cards[3].set_value("sf_v", value)

    @property
    def sf_w(self) -> float:
        """Get or set the Scale factor for LC_W
        """ # nopep8
        return self._cards[3].get_value("sf_w")

    @sf_w.setter
    def sf_w(self, value: float) -> None:
        self._cards[3].set_value("sf_w", value)

    @property
    def sf_d1(self) -> float:
        """Get or set the Scale factor for LC_D1
        """ # nopep8
        return self._cards[3].get_value("sf_d1")

    @sf_d1.setter
    def sf_d1(self, value: float) -> None:
        self._cards[3].set_value("sf_d1", value)

    @property
    def sf_da(self) -> float:
        """Get or set the Scale factor for LC_DA
        """ # nopep8
        return self._cards[3].get_value("sf_da")

    @sf_da.setter
    def sf_da(self, value: float) -> None:
        self._cards[3].set_value("sf_da", value)

    @property
    def sf_db(self) -> float:
        """Get or set the Scale factor for LC_DB
        """ # nopep8
        return self._cards[3].get_value("sf_db")

    @sf_db.setter
    def sf_db(self, value: float) -> None:
        self._cards[3].set_value("sf_db", value)

    @property
    def lc_p(self) -> typing.Optional[float]:
        """Get or set the Scale factor for LC_P
        """ # nopep8
        return self._cards[4].get_value("lc_p")

    @lc_p.setter
    def lc_p(self, value: float) -> None:
        self._cards[4].set_value("lc_p", value)

    @property
    def lc_t(self) -> typing.Optional[float]:
        """Get or set the Scale factor for LC_T
        """ # nopep8
        return self._cards[4].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: float) -> None:
        self._cards[4].set_value("lc_t", value)

