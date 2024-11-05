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

class PartInertiaContactPrint(KeywordBase):
    """DYNA PART_INERTIA_CONTACT_PRINT keyword"""

    keyword = "PART"
    subkeyword = "INERTIA_CONTACT_PRINT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "secid",
                        int,
                        10,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "mid",
                        int,
                        20,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "eosid",
                        int,
                        30,
                        10,
                        kwargs.get("eosid", 0)
                    ),
                    Field(
                        "hgid",
                        int,
                        40,
                        10,
                        kwargs.get("hgid", 0)
                    ),
                    Field(
                        "grav",
                        int,
                        50,
                        10,
                        kwargs.get("grav", 0)
                    ),
                    Field(
                        "adpopt",
                        int,
                        60,
                        10,
                        kwargs.get("adpopt")
                    ),
                    Field(
                        "tmid",
                        int,
                        70,
                        10,
                        kwargs.get("tmid", 0)
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
                        kwargs.get("xc")
                    ),
                    Field(
                        "yc",
                        float,
                        10,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "zc",
                        float,
                        20,
                        10,
                        kwargs.get("zc")
                    ),
                    Field(
                        "tm",
                        float,
                        30,
                        10,
                        kwargs.get("tm")
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
                        kwargs.get("nodeid")
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
                        kwargs.get("ixy")
                    ),
                    Field(
                        "ixz",
                        float,
                        20,
                        10,
                        kwargs.get("ixz")
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
                        kwargs.get("iyz")
                    ),
                    Field(
                        "izz",
                        float,
                        50,
                        10,
                        kwargs.get("izz")
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
                        kwargs.get("vtx")
                    ),
                    Field(
                        "vty",
                        float,
                        10,
                        10,
                        kwargs.get("vty")
                    ),
                    Field(
                        "vtz",
                        float,
                        20,
                        10,
                        kwargs.get("vtz")
                    ),
                    Field(
                        "vrx",
                        float,
                        30,
                        10,
                        kwargs.get("vrx")
                    ),
                    Field(
                        "vry",
                        float,
                        40,
                        10,
                        kwargs.get("vry")
                    ),
                    Field(
                        "vrz",
                        float,
                        50,
                        10,
                        kwargs.get("vrz")
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
                        "cid",
                        int,
                        60,
                        10,
                        kwargs.get("cid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fs",
                        float,
                        0,
                        10,
                        kwargs.get("fs")
                    ),
                    Field(
                        "fd",
                        float,
                        10,
                        10,
                        kwargs.get("fd")
                    ),
                    Field(
                        "dc",
                        float,
                        20,
                        10,
                        kwargs.get("dc")
                    ),
                    Field(
                        "vc",
                        float,
                        30,
                        10,
                        kwargs.get("vc")
                    ),
                    Field(
                        "optt",
                        float,
                        40,
                        10,
                        kwargs.get("optt")
                    ),
                    Field(
                        "sft",
                        float,
                        50,
                        10,
                        kwargs.get("sft")
                    ),
                    Field(
                        "ssf",
                        float,
                        60,
                        10,
                        kwargs.get("ssf")
                    ),
                    Field(
                        "cparm8",
                        float,
                        70,
                        10,
                        kwargs.get("cparm8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "prbf",
                        int,
                        0,
                        10,
                        kwargs.get("prbf", 0)
                    ),
                ],
            ),
        ]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID defined in *SECTION section.
        """ # nopep8
        return self._cards[1].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[1].set_value("secid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT section.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[1].set_value("mid", value)

    @property
    def eosid(self) -> int:
        """Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
        """ # nopep8
        return self._cards[1].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[1].set_value("eosid", value)

    @property
    def hgid(self) -> int:
        """Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
        EQ.0: default values are used.
        """ # nopep8
        return self._cards[1].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        self._cards[1].set_value("hgid", value)

    @property
    def grav(self) -> int:
        """Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
        EQ.0: all parts initialized,
        EQ.1: only current material initialized.
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""grav must be one of {0,1}""")
        self._cards[1].set_value("grav", value)

    @property
    def adpopt(self) -> typing.Optional[int]:
        """Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
        LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
        EQ.0:Adaptive remeshing is inactive for this part ID.
        EQ.1:	h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
        EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
        EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
        EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        self._cards[1].set_value("adpopt", value)

    @property
    def tmid(self) -> int:
        """Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
        EQ.0: defaults to MID.
        """ # nopep8
        return self._cards[1].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[1].set_value("tmid", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the corrdinates of the nodal point, NODID, are taken as the center of mass.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of center of mass.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of center of mass.
        """ # nopep8
        return self._cards[2].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[2].set_value("zc", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Translational mass.
        """ # nopep8
        return self._cards[2].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        self._cards[2].set_value("tm", value)

    @property
    def ircs(self) -> int:
        """Get or set the Flag for inertia tensor reference coordinate system:
        EQ.0: global inertia tensor (default),
        EQ.1: principal moments of inertia with orientation vectors.
        """ # nopep8
        return self._cards[2].get_value("ircs")

    @ircs.setter
    def ircs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ircs must be one of {0,1}""")
        self._cards[2].set_value("ircs", value)

    @property
    def nodeid(self) -> typing.Optional[int]:
        """Get or set the Nodal point defining the CG of the rigid body. This node should be included as an extra node for the rigid body; however, this is not a requirement. If this node is free, its motion will not be updated to correspond with the rigid body after the calculation begins.
        """ # nopep8
        return self._cards[2].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[2].set_value("nodeid", value)

    @property
    def ixx(self) -> typing.Optional[float]:
        """Get or set the Ixx, xx component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("ixx")

    @ixx.setter
    def ixx(self, value: float) -> None:
        self._cards[3].set_value("ixx", value)

    @property
    def ixy(self) -> typing.Optional[float]:
        """Get or set the Ixy, xy component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("ixy")

    @ixy.setter
    def ixy(self, value: float) -> None:
        self._cards[3].set_value("ixy", value)

    @property
    def ixz(self) -> typing.Optional[float]:
        """Get or set the Ixz, xz component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("ixz")

    @ixz.setter
    def ixz(self, value: float) -> None:
        self._cards[3].set_value("ixz", value)

    @property
    def iyy(self) -> typing.Optional[float]:
        """Get or set the Iyy, yy component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("iyy")

    @iyy.setter
    def iyy(self, value: float) -> None:
        self._cards[3].set_value("iyy", value)

    @property
    def iyz(self) -> typing.Optional[float]:
        """Get or set the Iyz, xy component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("iyz")

    @iyz.setter
    def iyz(self, value: float) -> None:
        self._cards[3].set_value("iyz", value)

    @property
    def izz(self) -> typing.Optional[float]:
        """Get or set the Izz , zz component of inertia tensor.
        """ # nopep8
        return self._cards[3].get_value("izz")

    @izz.setter
    def izz(self, value: float) -> None:
        self._cards[3].set_value("izz", value)

    @property
    def vtx(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocity of rigid body in x-direction.
        """ # nopep8
        return self._cards[4].get_value("vtx")

    @vtx.setter
    def vtx(self, value: float) -> None:
        self._cards[4].set_value("vtx", value)

    @property
    def vty(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocity of rigid body in y-direction.
        """ # nopep8
        return self._cards[4].get_value("vty")

    @vty.setter
    def vty(self, value: float) -> None:
        self._cards[4].set_value("vty", value)

    @property
    def vtz(self) -> typing.Optional[float]:
        """Get or set the Initial translational velocity of rigid body in z-direction.
        """ # nopep8
        return self._cards[4].get_value("vtz")

    @vtz.setter
    def vtz(self, value: float) -> None:
        self._cards[4].set_value("vtz", value)

    @property
    def vrx(self) -> typing.Optional[float]:
        """Get or set the Initial rotational velocity of rigid body about x-axis.
        """ # nopep8
        return self._cards[4].get_value("vrx")

    @vrx.setter
    def vrx(self, value: float) -> None:
        self._cards[4].set_value("vrx", value)

    @property
    def vry(self) -> typing.Optional[float]:
        """Get or set the Initial rotational velocity of rigid body about y-axis.
        """ # nopep8
        return self._cards[4].get_value("vry")

    @vry.setter
    def vry(self, value: float) -> None:
        self._cards[4].set_value("vry", value)

    @property
    def vrz(self) -> typing.Optional[float]:
        """Get or set the Initial rotational velocity of rigid body about z-axis.
        """ # nopep8
        return self._cards[4].get_value("vrz")

    @vrz.setter
    def vrz(self, value: float) -> None:
        self._cards[4].set_value("vrz", value)

    @property
    def xl(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0).
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
        """Get or set the x-coordinate of vector in local x-y plane.
        """ # nopep8
        return self._cards[5].get_value("xlip")

    @xlip.setter
    def xlip(self, value: float) -> None:
        self._cards[5].set_value("xlip", value)

    @property
    def ylip(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of vector in local x-y plane.
        """ # nopep8
        return self._cards[5].get_value("ylip")

    @ylip.setter
    def ylip(self, value: float) -> None:
        self._cards[5].set_value("ylip", value)

    @property
    def zlip(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of vecotr in local x-y plane.
        """ # nopep8
        return self._cards[5].get_value("zlip")

    @zlip.setter
    def zlip(self, value: float) -> None:
        self._cards[5].set_value("zlip", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_...
        If defined, leave fields 1-6 blank.
        """ # nopep8
        return self._cards[5].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[5].set_value("cid", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction.
        """ # nopep8
        return self._cards[6].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[6].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction.
        """ # nopep8
        return self._cards[6].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[6].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay coefficient.
        """ # nopep8
        return self._cards[6].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[6].set_value("dc", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Viscous friction coefficient.
        """ # nopep8
        return self._cards[6].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[6].set_value("vc", value)

    @property
    def optt(self) -> typing.Optional[float]:
        """Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
        """ # nopep8
        return self._cards[6].get_value("optt")

    @optt.setter
    def optt(self, value: float) -> None:
        self._cards[6].set_value("optt", value)

    @property
    def sft(self) -> typing.Optional[float]:
        """Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
        """ # nopep8
        return self._cards[6].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        self._cards[6].set_value("sft", value)

    @property
    def ssf(self) -> typing.Optional[float]:
        """Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition. If zero, SSF is taken as unity.
        """ # nopep8
        return self._cards[6].get_value("ssf")

    @ssf.setter
    def ssf(self, value: float) -> None:
        self._cards[6].set_value("ssf", value)

    @property
    def cparm8(self) -> typing.Optional[float]:
        """Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_AUTOMATIC_GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_..._MPP Optional Card.
        EQ.0 : Flag is not set(default).
        EQ.1 : Flag is set.
        EQ.2 : Flag is set.CPARM8 = 2 has the additional effect of permitting contact treatment of spot weld(type 9) beams in AUTOMATIC_GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_GENERAL contacts.
        """ # nopep8
        return self._cards[6].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: float) -> None:
        self._cards[6].set_value("cparm8", value)

    @property
    def prbf(self) -> int:
        """Get or set the Print flag for RBDOUT and MATSUM files
        EQ.0: default is taken from the keyword *CONTROL_OUTPUT
        EQ.1: write data into RDBOUT file only
        EQ.2: write data into MATSUM file only
        EQ.3: do not write data into RBDOUT AND MATSUM files
        """ # nopep8
        return self._cards[7].get_value("prbf")

    @prbf.setter
    def prbf(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""prbf must be one of {0,1,2,3}""")
        self._cards[7].set_value("prbf", value)

