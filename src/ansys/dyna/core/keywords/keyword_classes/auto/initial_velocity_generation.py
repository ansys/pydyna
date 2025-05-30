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

"""Module providing the InitialVelocityGeneration class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialVelocityGeneration(KeywordBase):
    """DYNA INITIAL_VELOCITY_GENERATION keyword"""

    keyword = "INITIAL"
    subkeyword = "VELOCITY_GENERATION"

    def __init__(self, **kwargs):
        """Initialize the InitialVelocityGeneration class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "styp",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "omega",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vx",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vy",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vz",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ivatn",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "icid",
                        int,
                        70,
                        10,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yc",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zc",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nx",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ny",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nz",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "phase",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "irigid",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part ID, part set ID, or node set ID.If zero, STYP is ignored,and all velocities are set.
        WARNING for if IVATN = 0: If a part ID of a rigid body is specified, only the nodes that belong to elements of the rigid body are initialized.Nodes added with* CONSTRAINED_EXTRA_NODES are not initialized.Set IVATN = 1 to initialize velocities of constrained nodes and parts..
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

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
        """Set the styp property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""styp must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("styp", value)

    @property
    def omega(self) -> float:
        """Get or set the Angular velocity about rotational axis.
        """ # nopep8
        return self._cards[0].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[0].set_value("omega", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial translational velocity in global x-direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial translational velocity in global y-direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial translational velocity in global z-direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def ivatn(self) -> int:
        """Get or set the Flag for setting the initial velocities of constrained nodes and parts:
        EQ.0 : Constrained parts are ignored.
        EQ.1 : Constrained parts and nodes constrained to parts will be assigned initial velocities like the part to which they are constrained..
        """ # nopep8
        return self._cards[0].get_value("ivatn")

    @ivatn.setter
    def ivatn(self, value: int) -> None:
        """Set the ivatn property."""
        if value not in [0, 1, None]:
            raise Exception("""ivatn must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ivatn", value)

    @property
    def icid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID. The specified translational velocities (VX, VY, VZ) and the direction cosines of the rotation axis (NX, NY, NZ) are in the global system if ICID=0 and are in the local system if ICID is defined. Therefore, if ICID is defined, *INCLUDE_TRANSFORM does not transform (VX, VY, VZ) and (NX, NY, NZ).
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        """Set the icid property."""
        self._cards[0].set_value("icid", value)

    @property
    def xc(self) -> float:
        """Get or set the x-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[1].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the y-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[1].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the z-coordinate on rotational axis.
        """ # nopep8
        return self._cards[1].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[1].set_value("zc", value)

    @property
    def nx(self) -> float:
        """Get or set the x-direction cosine.  If set to -999, NY and NZ are interpreted as the 1st and 2nd nodes defining the rotational axis, in which case the coordinates of node NY are used as XC, YC, ZC.  If ICID is defined, the direction cosine, (NX, NY, NZ), is projected along coordinate system ICID to yield the direction cosines of the rotation axis only if NX .NE. -999..
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> float:
        """Get or set the y-direction cosine or the 1st node of the rotational axis when NX = -999.
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[1].set_value("ny", value)

    @property
    def nz(self) -> float:
        """Get or set the z-direction cosine or the 2nd node of the rotational axis when NX = -999..
        """ # nopep8
        return self._cards[1].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[1].set_value("nz", value)

    @property
    def phase(self) -> int:
        """Get or set the Flag determining basis for initialization of velocity.
        EQ.0:	Initial velocities are applied at t = 0 of the regular transient phase of the analysis and are based on the undeformed geometry.Rigid bodies whose velocities are initialized using this keyword should always use PHASE = 0.
        EQ.1 : Initial velocities of deformable bodies are based on geometry that includes deformation incurred prior to the application of the initial velocities.That deformation could be due to a dynamic relaxation phase or due to a nonzero start time specified with *INITIAL_VELOCITY_GENERATION_START_TIME.
        """ # nopep8
        return self._cards[1].get_value("phase")

    @phase.setter
    def phase(self, value: int) -> None:
        """Set the phase property."""
        if value not in [0, 1, None]:
            raise Exception("""phase must be `None` or one of {0,1}.""")
        self._cards[1].set_value("phase", value)

    @property
    def irigid(self) -> int:
        """Get or set the Option to overwrite or automatically set rigid body velocities defined on the *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY _INERTIA cards.
        EQ.1:  Reset the rigid body velocites for *PART ID or all parts in *SET_PART ID.    This option does not apply for STYP=3.
        """ # nopep8
        return self._cards[1].get_value("irigid")

    @irigid.setter
    def irigid(self, value: int) -> None:
        """Set the irigid property."""
        self._cards[1].set_value("irigid", value)

