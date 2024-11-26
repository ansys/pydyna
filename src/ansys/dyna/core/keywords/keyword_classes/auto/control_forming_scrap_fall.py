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

class ControlFormingScrapFall(KeywordBase):
    """DYNA CONTROL_FORMING_SCRAP_FALL keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_SCRAP_FALL"

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
                        "vectid",
                        int,
                        10,
                        10,
                        kwargs.get("vectid")
                    ),
                    Field(
                        "ndset",
                        int,
                        20,
                        10,
                        kwargs.get("ndset")
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "depth",
                        float,
                        40,
                        10,
                        kwargs.get("depth")
                    ),
                    Field(
                        "dist",
                        float,
                        50,
                        10,
                        kwargs.get("dist")
                    ),
                    Field(
                        "idrgd",
                        int,
                        60,
                        10,
                        kwargs.get("idrgd")
                    ),
                    Field(
                        "ifseed",
                        int,
                        70,
                        10,
                        kwargs.get("ifseed")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nobead",
                        int,
                        0,
                        10,
                        kwargs.get("nobead")
                    ),
                    Field(
                        "seedx",
                        float,
                        10,
                        10,
                        kwargs.get("seedx")
                    ),
                    Field(
                        "seedy",
                        float,
                        20,
                        10,
                        kwargs.get("seedy")
                    ),
                    Field(
                        "seedz",
                        float,
                        30,
                        10,
                        kwargs.get("seedz")
                    ),
                    Field(
                        "effset",
                        float,
                        40,
                        10,
                        kwargs.get("effset")
                    ),
                    Field(
                        "gap",
                        float,
                        50,
                        10,
                        kwargs.get("gap")
                    ),
                    Field(
                        "ipset",
                        int,
                        60,
                        10,
                        kwargs.get("ipset")
                    ),
                    Field(
                        "extend",
                        int,
                        70,
                        10,
                        kwargs.get("extend")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "newid",
                        int,
                        0,
                        10,
                        kwargs.get("newid")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of a scrap piece.  This part ID becomes a dummy ID if all trimmed scrap pieces are defined by NEWID. See definition for NEWID and Figure 0-3.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def vectid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for a trim steel movement, as defined by *DEFINE_VECTOR. If left undefined (blank), global z-direction is assumed.
        """ # nopep8
        return self._cards[0].get_value("vectid")

    @vectid.setter
    def vectid(self, value: int) -> None:
        self._cards[0].set_value("vectid", value)

    @property
    def ndset(self) -> typing.Optional[int]:
        """Get or set the A node set consists of all nodes along the cutting edge of the trim steel.  Note that prior to Revision 90339 the nodes in the set must be defined in consecutive order.  See Remarks (LS-PrePost) below on how to define a node set along a path in LS-PrePost.  This node set, together with VECTID, is projected to the sheet metal to form a trim curve.  To trim a scrap out of a parent piece involving a neighboring trim steel, which also serves as a scrap cutter, the node set needs to be defined for the scrap cutter portion only for the scrap, see Figure 0-3.
        """ # nopep8
        return self._cards[0].get_value("ndset")

    @ndset.setter
    def ndset(self, value: int) -> None:
        self._cards[0].set_value("ndset", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID governing the trim steel kinematics, as defined by *DEFINE_CURVE.
        GT.0:	velocity-controlled kinematics
        LT.0:	displacement-controlled kinematics
        An example input deck is provided below.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def depth(self) -> typing.Optional[float]:
        """Get or set the A small penetrating distance between the cutting edge of the trim steel and the scrap piece, as shown in Figure 0-2.  Nodes along the scrap edge are released from automatically added constraints at the simulation start and are free to move after this distance is reached.
        """ # nopep8
        return self._cards[0].get_value("depth")

    @depth.setter
    def depth(self, value: float) -> None:
        self._cards[0].set_value("depth", value)

    @property
    def dist(self) -> typing.Optional[float]:
        """Get or set the A distance tolerance measured in the plane normal to the trim steel moving direction, between nodes along the cutting edge of the trim steel defined by NDSET and nodes along an edge of the scrap, as shown in Figure 0-1.  This tolerance is used to determine if the constraints need to be added at the simulation start to the nodes along the trim edge of the scrap piece.
        """ # nopep8
        return self._cards[0].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[0].set_value("dist", value)

    @property
    def idrgd(self) -> typing.Optional[int]:
        """Get or set the Part ID of a parent piece, which is the remaining sheet metal after the scrap is successfully trimmed out of a large sheet metal.  Note the usual *PART needs to be defined somewhere in the input deck, along with *MAT_20 and totally fixed translational and rotational DOFs.  See Figure 0-3.
        """ # nopep8
        return self._cards[0].get_value("idrgd")

    @idrgd.setter
    def idrgd(self, value: int) -> None:
        self._cards[0].set_value("idrgd", value)

    @property
    def ifseed(self) -> typing.Optional[int]:
        """Get or set the A flag to indicate the location of the scrap piece.
        EQ.0:	automatically determined.  The trim steel defined will be responsible to trim as well as to push (have contact with) the scrap piece.
        EQ.1:	automatically determined, however, the trim steel in definition will only be used to trim out the scrap, not to push (have contact with) the scrap piece.
        EQ.-1:	user specified by defining SEEDX, SEEDY, and SEEDZ.
        """ # nopep8
        return self._cards[0].get_value("ifseed")

    @ifseed.setter
    def ifseed(self, value: int) -> None:
        self._cards[0].set_value("ifseed", value)

    @property
    def nobead(self) -> typing.Optional[int]:
        """Get or set the A node set to be excluded from initially imposed constraints after trimming.  This node set typically consists of nodes in the scrap draw bead region where due to modeling problems the beads on the scrap initially interfere with the beads on the rigid tooling; it causes scrap to get stuck later in the simulation if left as is.  See Figure 0-4.
        """ # nopep8
        return self._cards[1].get_value("nobead")

    @nobead.setter
    def nobead(self, value: int) -> None:
        self._cards[1].set_value("nobead", value)

    @property
    def seedx(self) -> typing.Optional[float]:
        """Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
        """ # nopep8
        return self._cards[1].get_value("seedx")

    @seedx.setter
    def seedx(self, value: float) -> None:
        self._cards[1].set_value("seedx", value)

    @property
    def seedy(self) -> typing.Optional[float]:
        """Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
        """ # nopep8
        return self._cards[1].get_value("seedy")

    @seedy.setter
    def seedy(self, value: float) -> None:
        self._cards[1].set_value("seedy", value)

    @property
    def seedz(self) -> typing.Optional[float]:
        """Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
        """ # nopep8
        return self._cards[1].get_value("seedz")

    @seedz.setter
    def seedz(self, value: float) -> None:
        self._cards[1].set_value("seedz", value)

    @property
    def effset(self) -> typing.Optional[float]:
        """Get or set the Scrap edge offset amount away from the trim steel edge, towards the scrap seed node side.  This is useful to remove initial interference between the trimmed scrap (because of poorly modeled trim steel) and coarsely modeled lower trim post.  See Figure 0-3..
        """ # nopep8
        return self._cards[1].get_value("effset")

    @effset.setter
    def effset(self, value: float) -> None:
        self._cards[1].set_value("effset", value)

    @property
    def gap(self) -> typing.Optional[float]:
        """Get or set the Scrap piece offset amount from the part set defined by IPSET (e.g. top surfaces of the scrap cutters), in the direction of the element normals of the IPSET.  This parameter makes it easier to remove initial interference between the scrap and other die components.  See Figure 0-5.
        """ # nopep8
        return self._cards[1].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        self._cards[1].set_value("gap", value)

    @property
    def ipset(self) -> typing.Optional[int]:
        """Get or set the A part set ID from which the scrap will be offset to remove the initial interference, works together only with GAP.  The part set ID should only include portions of tool parts that are directly underneath the scrap (top surface portion of the tools).  The normals of the IPSET must point toward the scrap.  The parts that should belong to IPSET are typically of those elements on the top surface of the scrap cutter, see Figure 0-5.
        """ # nopep8
        return self._cards[1].get_value("ipset")

    @ipset.setter
    def ipset(self, value: int) -> None:
        self._cards[1].set_value("ipset", value)

    @property
    def extend(self) -> typing.Optional[int]:
        """Get or set the An amount to extend a trim steel’s edge based on the NDSET defined, so it can form a continuous trim line together with a neighboring trim steel, whose edge may also be extended, to trim out the scrap piece.  See Figure 0-3..
        """ # nopep8
        return self._cards[1].get_value("extend")

    @extend.setter
    def extend(self, value: int) -> None:
        self._cards[1].set_value("extend", value)

    @property
    def newid(self) -> typing.Optional[int]:
        """Get or set the New part ID of a scrap piece for the scrap area defined by the seed location.  If this is not defined (left blank) or input as “0”, the scrap piece will retain original PID as its part ID.  See Figure 0-3.  This is useful in case where one original scrap is trimmed into multiple smaller pieces, and contacts between these smaller pieces need to be defined..
        """ # nopep8
        return self._cards[2].get_value("newid")

    @newid.setter
    def newid(self, value: int) -> None:
        self._cards[2].set_value("newid", value)

