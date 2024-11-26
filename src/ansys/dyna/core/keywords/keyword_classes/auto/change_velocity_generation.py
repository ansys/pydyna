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

class ChangeVelocityGeneration(KeywordBase):
    """DYNA CHANGE_VELOCITY_GENERATION keyword"""

    keyword = "CHANGE"
    subkeyword = "VELOCITY_GENERATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid/pid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid/pid")
                    ),
                    Field(
                        "styp",
                        int,
                        10,
                        10,
                        kwargs.get("styp", 1)
                    ),
                    Field(
                        "omega",
                        float,
                        20,
                        10,
                        kwargs.get("omega", 0.0)
                    ),
                    Field(
                        "vx",
                        float,
                        30,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        40,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        50,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "ivatn",
                        int,
                        60,
                        10,
                        kwargs.get("ivatn", 0)
                    ),
                    Field(
                        "icid",
                        int,
                        70,
                        10,
                        kwargs.get("icid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        10,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        20,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                    Field(
                        "nx",
                        float,
                        30,
                        10,
                        kwargs.get("nx", 0.0)
                    ),
                    Field(
                        "ny",
                        float,
                        40,
                        10,
                        kwargs.get("ny", 0.0)
                    ),
                    Field(
                        "nz",
                        float,
                        50,
                        10,
                        kwargs.get("nz", 0.0)
                    ),
                    Field(
                        "phase",
                        int,
                        60,
                        10,
                        kwargs.get("phase", 0)
                    ),
                    Field(
                        "irigid",
                        int,
                        70,
                        10,
                        kwargs.get("irigid", 0)
                    ),
                ],
            ),
        ]

    @property
    def nsid_pid(self) -> typing.Optional[int]:
        """Get or set the Node set ID or part set ID.
        EQ.0: STYP is ignored and all velocities are set.
        """ # nopep8
        return self._cards[0].get_value("nsid/pid")

    @nsid_pid.setter
    def nsid_pid(self, value: int) -> None:
        self._cards[0].set_value("nsid/pid", value)

    @property
    def styp(self) -> int:
        """Get or set the Set type:
        EQ.1: part set ID, see *SET_PART,
        EQ.2: part ID, see *PART,
        EQ.3: nodal set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""styp must be one of {1,2,3}""")
        self._cards[0].set_value("styp", value)

    @property
    def omega(self) -> float:
        """Get or set the Angular velocity about rotational axis.
        """ # nopep8
        return self._cards[0].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[0].set_value("omega", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial translational velocity in global x-direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial translational velocity in global y-direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial translational velocity in global z-direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def ivatn(self) -> int:
        """Get or set the EQ.0:  slaved parts are ignored.
        EQ.1:  slaved parts and slaved nodes of the master parts will be assigned initial velocities like the master part.
        """ # nopep8
        return self._cards[0].get_value("ivatn")

    @ivatn.setter
    def ivatn(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ivatn must be one of {0,1}""")
        self._cards[0].set_value("ivatn", value)

    @property
    def icid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID. The specified translational velocities (VX, VY, VZ) and the direction cosines of the rotation axis (NX, NY, NZ) are in the global system if ICID=0 and are in the local system if ICID is defined. Therefore, if ICID is defined, *INCLUDE_TRANSFORM does not transform (VX, VY, VZ) and (NX, NY, NZ).
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        self._cards[0].set_value("icid", value)

    @property
    def xc(self) -> float:
        """Get or set the x-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[1].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the y-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[1].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the z-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[1].set_value("zc", value)

    @property
    def nx(self) -> float:
        """Get or set the x-direction cosine.  If set to -999, NY and NZ are interpreted as the 1st and 2nd nodes defining the rotational axis, in which case the coordinates of node NY are used as XC, YC, ZC.  If ICID is defined, the direction cosine, (NX, NY, NZ), is projected along coordinate system ICID to yield the direction cosines of the rotation axis only if NX .NE. -999..
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> float:
        """Get or set the y-direction cosine or the 1st node of the rotational axis when NX = -999.
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        self._cards[1].set_value("ny", value)

    @property
    def nz(self) -> float:
        """Get or set the z-direction cosine or the 2nd node of the rotational axis when NX = -999..
        """ # nopep8
        return self._cards[1].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        self._cards[1].set_value("nz", value)

    @property
    def phase(self) -> int:
        """Get or set the Flag specifying phase of the analysis the velocities apply to:
        EQ.0. Velocities applied immediately,
        EQ.1. Velocities applied after dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("phase")

    @phase.setter
    def phase(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""phase must be one of {0,1}""")
        self._cards[1].set_value("phase", value)

    @property
    def irigid(self) -> int:
        """Get or set the Option to overwrite or automatically set rigid body velocities defined on the *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY _INERTIA cards.
        EQ.1:  Reset the rigid body velocites for *PART ID or all parts in *SET_PART ID.    This option does not apply for STYP=3.
        """ # nopep8
        return self._cards[1].get_value("irigid")

    @irigid.setter
    def irigid(self, value: int) -> None:
        self._cards[1].set_value("irigid", value)

