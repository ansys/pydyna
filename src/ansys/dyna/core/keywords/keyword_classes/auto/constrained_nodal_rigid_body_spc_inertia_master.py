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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ConstrainedNodalRigidBodySpcInertiaMaster(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY_SPC_INERTIA_MASTER keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY_SPC_INERTIA_MASTER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
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
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "nsid",
                        int,
                        20,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "pnode",
                        int,
                        30,
                        10,
                        kwargs.get("pnode", 0)
                    ),
                    Field(
                        "iprt",
                        int,
                        40,
                        10,
                        kwargs.get("iprt", 0)
                    ),
                    Field(
                        "drflag",
                        int,
                        50,
                        10,
                        kwargs.get("drflag", 0)
                    ),
                    Field(
                        "rrflag",
                        int,
                        60,
                        10,
                        kwargs.get("rrflag", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cmo",
                        float,
                        0,
                        10,
                        kwargs.get("cmo", 0.0)
                    ),
                    Field(
                        "con1",
                        float,
                        10,
                        10,
                        kwargs.get("con1", 0)
                    ),
                    Field(
                        "con2",
                        float,
                        20,
                        10,
                        kwargs.get("con2", 0)
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
                        "tm",
                        float,
                        30,
                        10,
                        kwargs.get("tm", 0.0)
                    ),
                    Field(
                        "ircs",
                        int,
                        40,
                        10,
                        kwargs.get("ircs", 0)
                    ),
                    Field(
                        "nodeid",
                        int,
                        50,
                        10,
                        kwargs.get("nodeid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ixx",
                        float,
                        0,
                        10,
                        kwargs.get("ixx")
                    ),
                    Field(
                        "ixy",
                        float,
                        10,
                        10,
                        kwargs.get("ixy", 0.0)
                    ),
                    Field(
                        "ixz",
                        float,
                        20,
                        10,
                        kwargs.get("ixz", 0.0)
                    ),
                    Field(
                        "iyy",
                        float,
                        30,
                        10,
                        kwargs.get("iyy")
                    ),
                    Field(
                        "iyz",
                        float,
                        40,
                        10,
                        kwargs.get("iyz", 0.0)
                    ),
                    Field(
                        "izz",
                        float,
                        50,
                        10,
                        kwargs.get("izz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vtx",
                        float,
                        0,
                        10,
                        kwargs.get("vtx", 0.0)
                    ),
                    Field(
                        "vty",
                        float,
                        10,
                        10,
                        kwargs.get("vty", 0.0)
                    ),
                    Field(
                        "vtz",
                        float,
                        20,
                        10,
                        kwargs.get("vtz", 0.0)
                    ),
                    Field(
                        "vrx",
                        float,
                        30,
                        10,
                        kwargs.get("vrx", 0.0)
                    ),
                    Field(
                        "vry",
                        float,
                        40,
                        10,
                        kwargs.get("vry", 0.0)
                    ),
                    Field(
                        "vrz",
                        float,
                        50,
                        10,
                        kwargs.get("vrz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xl",
                        float,
                        0,
                        10,
                        kwargs.get("xl")
                    ),
                    Field(
                        "yl",
                        float,
                        10,
                        10,
                        kwargs.get("yl")
                    ),
                    Field(
                        "zl",
                        float,
                        20,
                        10,
                        kwargs.get("zl")
                    ),
                    Field(
                        "xlip",
                        float,
                        30,
                        10,
                        kwargs.get("xlip")
                    ),
                    Field(
                        "ylip",
                        float,
                        40,
                        10,
                        kwargs.get("ylip")
                    ),
                    Field(
                        "zlip",
                        float,
                        50,
                        10,
                        kwargs.get("zlip")
                    ),
                    Field(
                        "cid2",
                        int,
                        60,
                        10,
                        kwargs.get("cid2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ConstrainedNodalRigidBodySpcInertiaMaster.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the nodal rigid body.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. Only necessary if no local system is defined below.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE. This nodal set defines the rigid body.If NSID=0, then NSID=PID, i.e., the node set ID and the part ID are assumed to be identical.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def pnode(self) -> int:
        """Get or set the An optional, possibly massless, nodal point located at the mass center of the nodal rigid body. The initial nodal coordinates will be reset if necessary to ensure that they lie at the mass center. In the output files, the coordinates, accelerations, velocites, and displacements of this node will coorespond to the mass center of the nodal rigid body. If CID is defined, the velocities and accelerations of PNODE will be output in the local system in the D3PLOT and D3THDT files unless PNODE is specified as a negative number in which case the global system is used.
        """ # nopep8
        return self._cards[0].get_value("pnode")

    @pnode.setter
    def pnode(self, value: int) -> None:
        self._cards[0].set_value("pnode", value)

    @property
    def iprt(self) -> int:
        """Get or set the Print flag.  For nodal rigid bodies the following values apply:
        EQ.1:	Write data into rbdout.
        EQ.2 : Do not write data into rbdout.
        Except for in the case of two - noded rigid bodies, IPRT(if 0 or unset) defaults to the value of IPRTF in* CONTROL_OUTPUT.For two - noded rigid bodies, printing is suppressed(IPRT = 2) unless IPRT is set to 1.  This is to avoid excessively large rbdout files when the model contains many two - noded welds.
        """ # nopep8
        return self._cards[0].get_value("iprt")

    @iprt.setter
    def iprt(self, value: int) -> None:
        self._cards[0].set_value("iprt", value)

    @property
    def drflag(self) -> int:
        """Get or set the Displacement release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z displacement in global system,
        EQ.-6: release z and x displacement in global system,
        EQ.-5: release y and z displacement in global system,
        EQ.-4: release x and y displacement in global system,
        EQ.-3: release z displacement in global system,
        EQ.-2: release y displacement in global system,
        EQ.-1: release x displacement in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x displacement in rigid body local system,
        EQ. 2: release y displacement in rigid body local system,
        EQ. 3: release z displacement in rigid body local system,
        EQ. 4: release x and y displacement in rigid body local system,
        EQ. 5: release y and z displacement in rigid body local system,
        EQ. 6: release z and x displacement in rigid body local system,
        EQ. 7: release x, y, and z displacement in rigid body local system
        """ # nopep8
        return self._cards[0].get_value("drflag")

    @drflag.setter
    def drflag(self, value: int) -> None:
        if value not in [0, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""drflag must be one of {0,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("drflag", value)

    @property
    def rrflag(self) -> int:
        """Get or set the Rotation release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z rotations in global system,
        EQ.-6: release z and x rotations in global system,
        EQ.-5: release y and z rotations in global system,
        EQ.-4: release x and y rotations in global system,
        EQ.-3: release z rotation in global system,
        EQ.-2: release y rotation in global system,
        EQ.-1: release x rotation in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x rotation in rigid body local system,
        EQ. 2: release y rotation in rigid body local system,
        EQ. 3: release z rotation in rigid body local system,
        EQ. 4: release x and y rotations in rigid body local system,
        EQ. 5: release y and z rotations in rigid body local system,
        EQ. 6: release z and x rotations in rigid body local system,
        EQ. 7: release x, y, and z rotations in rigid body local system,
        """ # nopep8
        return self._cards[0].get_value("rrflag")

    @rrflag.setter
    def rrflag(self, value: int) -> None:
        if value not in [0, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rrflag must be one of {0,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rrflag", value)

    @property
    def cmo(self) -> float:
        """Get or set the Center of mass constraint option, CMO:
        EQ.+1.0: constraints applied in global directions,
        EQ. 0.0: no constraints,
        EQ. -1.0: constraints applied in local directions (SPC constraint).
        """ # nopep8
        return self._cards[1].get_value("cmo")

    @cmo.setter
    def cmo(self, value: float) -> None:
        if value not in [0.0, -1.0, 1.0]:
            raise Exception("""cmo must be one of {0.0,-1.0,1.0}""")
        self._cards[1].set_value("cmo", value)

    @property
    def con1(self) -> float:
        """Get or set the First constraint parameter:
        If CMO=+1.0, then specify global translational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x displacement,
        EQ.2: constrained y displacement,
        EQ.3: constrained z displacement,
        EQ.4: constrained x and y displacements,
        EQ.5: constrained y and z displacements,
        EQ.6: constrained z and x displacements,
        EQ.7: constrained x, y, and z displacements.
        If CM0=-1.0, then specify local coordinate system ID. See *DEFINE_ COORDINATE_OPTION: This coordinate system is fixed in time.
        """ # nopep8
        return self._cards[1].get_value("con1")

    @con1.setter
    def con1(self, value: float) -> None:
        self._cards[1].set_value("con1", value)

    @property
    def con2(self) -> float:
        """Get or set the If CMO=+1.0, then specify global rotational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x, y, and z rotations.
        If CM0=-1.0, then specify local (SPC) constraint:
        EQ.000000 no constraint,
        EQ.100000 constrained x translation,
        EQ.010000 constrained y translation,
        EQ.001000 constrained z translation,
        EQ.000100 constrained x rotation,
        EQ.000010 constrained y rotation,
        EQ.000001 constrained z rotation.
        Any combination of local constraints can be achieved by adding the number 1 into the corresponding column.
        """ # nopep8
        return self._cards[1].get_value("con2")

    @con2.setter
    def con2(self, value: float) -> None:
        self._cards[1].set_value("con2", value)

    @property
    def xc(self) -> float:
        """Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the coordinates of the nodal point, NODEID, are taken as the center of mass.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the y-coordinate of center of mass.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the z-coordinate of center of mass.
        """ # nopep8
        return self._cards[2].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[2].set_value("zc", value)

    @property
    def tm(self) -> float:
        """Get or set the Translational mass.
        """ # nopep8
        return self._cards[2].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        self._cards[2].set_value("tm", value)

    @property
    def ircs(self) -> int:
        """Get or set the Flag for inertia tensor reference coordinate system:
        EQ.0: global inertia tensor,
        EQ.1: principal moments of inertias with orientation vectors as given below.
        """ # nopep8
        return self._cards[2].get_value("ircs")

    @ircs.setter
    def ircs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ircs must be one of {0,1}""")
        self._cards[2].set_value("ircs", value)

    @property
    def nodeid(self) -> int:
        """Get or set the Optional nodal point defining the CG of the rigid body. If this node is not a member of the set NSID above, its motion will not be updated to correspond with the nodal rigid body after the calculation begins. PNODE and NODEID can be identical if and only if PNODE physically lies at the mass center at time zero.
        """ # nopep8
        return self._cards[2].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[2].set_value("nodeid", value)

    @property
    def ixx(self) -> typing.Optional[float]:
        """Get or set the XX component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("ixx")

    @ixx.setter
    def ixx(self, value: float) -> None:
        self._cards[3].set_value("ixx", value)

    @property
    def ixy(self) -> float:
        """Get or set the XY component of inertia tesor (set to zero if IRCS=1).
        """ # nopep8
        return self._cards[3].get_value("ixy")

    @ixy.setter
    def ixy(self, value: float) -> None:
        self._cards[3].set_value("ixy", value)

    @property
    def ixz(self) -> float:
        """Get or set the XZ component of inertia tesor (set to zero if IRCS=1).
        """ # nopep8
        return self._cards[3].get_value("ixz")

    @ixz.setter
    def ixz(self, value: float) -> None:
        self._cards[3].set_value("ixz", value)

    @property
    def iyy(self) -> typing.Optional[float]:
        """Get or set the YY component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("iyy")

    @iyy.setter
    def iyy(self, value: float) -> None:
        self._cards[3].set_value("iyy", value)

    @property
    def iyz(self) -> float:
        """Get or set the YZ component of inertia tesor (set to zero if IRCS=1).
        """ # nopep8
        return self._cards[3].get_value("iyz")

    @iyz.setter
    def iyz(self, value: float) -> None:
        self._cards[3].set_value("iyz", value)

    @property
    def izz(self) -> float:
        """Get or set the ZZ component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("izz")

    @izz.setter
    def izz(self, value: float) -> None:
        self._cards[3].set_value("izz", value)

    @property
    def vtx(self) -> float:
        """Get or set the x-rigid body initial translational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vtx")

    @vtx.setter
    def vtx(self, value: float) -> None:
        self._cards[4].set_value("vtx", value)

    @property
    def vty(self) -> float:
        """Get or set the y-rigid body initial translational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vty")

    @vty.setter
    def vty(self, value: float) -> None:
        self._cards[4].set_value("vty", value)

    @property
    def vtz(self) -> float:
        """Get or set the z-rigid body initial translational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vtz")

    @vtz.setter
    def vtz(self, value: float) -> None:
        self._cards[4].set_value("vtz", value)

    @property
    def vrx(self) -> float:
        """Get or set the x-rigid body initial rotational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vrx")

    @vrx.setter
    def vrx(self, value: float) -> None:
        self._cards[4].set_value("vrx", value)

    @property
    def vry(self) -> float:
        """Get or set the y-rigid body initial rotational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vry")

    @vry.setter
    def vry(self, value: float) -> None:
        self._cards[4].set_value("vry", value)

    @property
    def vrz(self) -> float:
        """Get or set the z-rigid body initial rotational velocity in global coordinate system.
        """ # nopep8
        return self._cards[4].get_value("vrz")

    @vrz.setter
    def vrz(self, value: float) -> None:
        self._cards[4].set_value("vrz", value)

    @property
    def xl(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0)
        """ # nopep8
        return self._cards[5].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        self._cards[5].set_value("xl", value)

    @property
    def yl(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of local x-axis.
        """ # nopep8
        return self._cards[5].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        self._cards[5].set_value("yl", value)

    @property
    def zl(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of local x-axis.
        """ # nopep8
        return self._cards[5].get_value("zl")

    @zl.setter
    def zl(self, value: float) -> None:
        self._cards[5].set_value("zl", value)

    @property
    def xlip(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of local in-plane vector
        """ # nopep8
        return self._cards[5].get_value("xlip")

    @xlip.setter
    def xlip(self, value: float) -> None:
        self._cards[5].set_value("xlip", value)

    @property
    def ylip(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of local in-plane vector
        """ # nopep8
        return self._cards[5].get_value("ylip")

    @ylip.setter
    def ylip(self, value: float) -> None:
        self._cards[5].set_value("ylip", value)

    @property
    def zlip(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of local in-plane vector
        """ # nopep8
        return self._cards[5].get_value("zlip")

    @zlip.setter
    def zlip(self, value: float) -> None:
        self._cards[5].set_value("zlip", value)

    @property
    def cid2(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID, see *DEFINE_COORDINATE, with this option leave fields 1-6 blank.
        """ # nopep8
        return self._cards[5].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        self._cards[5].set_value("cid2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

