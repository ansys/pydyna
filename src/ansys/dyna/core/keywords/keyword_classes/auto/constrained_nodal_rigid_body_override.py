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

class ConstrainedNodalRigidBodyOverride(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY_OVERRIDE keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY_OVERRIDE"
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
                        "icnt",
                        int,
                        0,
                        10,
                        kwargs.get("icnt", 0)
                    ),
                    Field(
                        "ibag",
                        int,
                        10,
                        10,
                        kwargs.get("ibag", 0)
                    ),
                    Field(
                        "ipsm",
                        int,
                        20,
                        10,
                        kwargs.get("ipsm", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ConstrainedNodalRigidBodyOverride.option_specs[0],
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
    def icnt(self) -> int:
        """Get or set the Flag for contact synchronization:
        EQ.0:	No synchronization,
        EQ.1 : Since there exists no contact when both slave and master sides belong to the same rigid body,
        setting ICNT = 1 will turn off / on all contact definitions of which the slave and master sides belong to
        the same nodal rigid body PID when PID is turned on / off by * SENSOR_CONTROL.
        """ # nopep8
        return self._cards[1].get_value("icnt")

    @icnt.setter
    def icnt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icnt must be one of {0,1}""")
        self._cards[1].set_value("icnt", value)

    @property
    def ibag(self) -> int:
        """Get or set the Flag for control volume airbag synchronization:
        EQ.0:	No synchronization,
        EQ.1 : Since airbag pressure will not change when all segments constituting the airbag belong to
        the same rigid body, setting IBAG = 1 will skip calculation of control volume airbags of
        which all the segments belong to the same nodal rigid body PID when PID is on.The airbag calculation will be resumed,
        with time offset to related airbag time - dependent curves, when PID is turned off by* SENSOR_CONTROL.
        """ # nopep8
        return self._cards[1].get_value("ibag")

    @ibag.setter
    def ibag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibag must be one of {0,1}""")
        self._cards[1].set_value("ibag", value)

    @property
    def ipsm(self) -> int:
        """Get or set the Flag for prescribed-motion synchronization:
        EQ.0:	No synchronization,
        EQ.1 : Prescribed boundary conditions,* BOUNDARY_PRESCRIBED_MOTION, for PID will be turned off
        automatically when PID is turned off by* SENSOR_CONTROL.Prescribed boundary condition not for PIDand of
        which or all related nodes belong to PID will be turned off when PID is active to avoid boundary
        condition conflict.Those boundary conditions will be turned on, with time
        offset to related time - dependent curves, when PID is turned off by* SENSOR_CONTROL.
        EQ.2 : Same as IPSM = 1, however, without time offset when those boundary conditions not for PID are turned on..
        """ # nopep8
        return self._cards[1].get_value("ipsm")

    @ipsm.setter
    def ipsm(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ipsm must be one of {0,1,2}""")
        self._cards[1].set_value("ipsm", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)
