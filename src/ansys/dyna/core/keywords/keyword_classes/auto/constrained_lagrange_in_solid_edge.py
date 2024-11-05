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

class ConstrainedLagrangeInSolidEdge(KeywordBase):
    """DYNA CONSTRAINED_LAGRANGE_IN_SOLID_EDGE keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "LAGRANGE_IN_SOLID_EDGE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "coupid",
                        int,
                        0,
                        10,
                        kwargs.get("coupid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "slave",
                        int,
                        0,
                        10,
                        kwargs.get("slave")
                    ),
                    Field(
                        "master",
                        int,
                        10,
                        10,
                        kwargs.get("master")
                    ),
                    Field(
                        "sstyp",
                        int,
                        20,
                        10,
                        kwargs.get("sstyp", 0)
                    ),
                    Field(
                        "mstyp",
                        int,
                        30,
                        10,
                        kwargs.get("mstyp", 0)
                    ),
                    Field(
                        "nquad",
                        int,
                        40,
                        10,
                        kwargs.get("nquad", 0)
                    ),
                    Field(
                        "ctype",
                        int,
                        50,
                        10,
                        kwargs.get("ctype", 2)
                    ),
                    Field(
                        "direc",
                        int,
                        60,
                        10,
                        kwargs.get("direc", 1)
                    ),
                    Field(
                        "mcoup",
                        int,
                        70,
                        10,
                        kwargs.get("mcoup", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "start",
                        float,
                        0,
                        10,
                        kwargs.get("start", 0.0)
                    ),
                    Field(
                        "end",
                        float,
                        10,
                        10,
                        kwargs.get("end", 1.0E+10)
                    ),
                    Field(
                        "pfac",
                        float,
                        20,
                        10,
                        kwargs.get("pfac", 0.1)
                    ),
                    Field(
                        "fric",
                        float,
                        30,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                    Field(
                        "frcmin",
                        float,
                        40,
                        10,
                        kwargs.get("frcmin", 0.5)
                    ),
                    Field(
                        "norm",
                        int,
                        50,
                        10,
                        kwargs.get("norm", 0)
                    ),
                    Field(
                        "normtyp",
                        int,
                        60,
                        10,
                        kwargs.get("normtyp", 0)
                    ),
                    Field(
                        "damp",
                        float,
                        70,
                        10,
                        kwargs.get("damp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cq",
                        float,
                        0,
                        10,
                        kwargs.get("cq", 0.0)
                    ),
                    Field(
                        "hmin",
                        float,
                        10,
                        10,
                        kwargs.get("hmin")
                    ),
                    Field(
                        "hmax",
                        float,
                        20,
                        10,
                        kwargs.get("hmax")
                    ),
                    Field(
                        "ileak",
                        int,
                        30,
                        10,
                        kwargs.get("ileak", 0)
                    ),
                    Field(
                        "pleak",
                        float,
                        40,
                        10,
                        kwargs.get("pleak", 0.1)
                    ),
                    Field(
                        "lcidpor",
                        int,
                        50,
                        10,
                        kwargs.get("lcidpor")
                    ),
                    Field(
                        "nvent",
                        int,
                        60,
                        10,
                        kwargs.get("nvent", 0)
                    ),
                    Field(
                        "blockage",
                        int,
                        70,
                        10,
                        kwargs.get("blockage", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iboxid",
                        int,
                        0,
                        10,
                        kwargs.get("iboxid", 0)
                    ),
                    Field(
                        "ipenchk",
                        int,
                        10,
                        10,
                        kwargs.get("ipenchk", 0)
                    ),
                    Field(
                        "intforc",
                        int,
                        20,
                        10,
                        kwargs.get("intforc", 0)
                    ),
                    Field(
                        "ialesof",
                        int,
                        30,
                        10,
                        kwargs.get("ialesof", 0)
                    ),
                    Field(
                        "lagmul",
                        float,
                        40,
                        10,
                        kwargs.get("lagmul", 0.0)
                    ),
                    Field(
                        "pfacmm",
                        int,
                        50,
                        10,
                        kwargs.get("pfacmm", 0)
                    ),
                    Field(
                        "thkf",
                        float,
                        60,
                        10,
                        kwargs.get("thkf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a1",
                        float,
                        0,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        10,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "a2",
                        float,
                        20,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "b2",
                        float,
                        30,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "a3",
                        float,
                        40,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "b3",
                        float,
                        50,
                        10,
                        kwargs.get("b3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ventsid",
                        int,
                        0,
                        10,
                        kwargs.get("ventsid")
                    ),
                    Field(
                        "ventyp",
                        int,
                        10,
                        10,
                        kwargs.get("ventyp", 0)
                    ),
                    Field(
                        "vtcoef",
                        int,
                        20,
                        10,
                        kwargs.get("vtcoef", 0)
                    ),
                    Field(
                        "poppres",
                        float,
                        30,
                        10,
                        kwargs.get("poppres", 0.0)
                    ),
                    Field(
                        "coeflc",
                        int,
                        40,
                        10,
                        kwargs.get("coeflc", 0)
                    ),
                ],
            ),
        ]

    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the ID.
        """ # nopep8
        return self._cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        self._cards[0].set_value("coupid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Title
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def slave(self) -> typing.Optional[int]:
        """Get or set the Part, part set ID or Segment set ID of slaves see *PART, *SET_PART or *SET_SEGMENT.
        """ # nopep8
        return self._cards[1].get_value("slave")

    @slave.setter
    def slave(self, value: int) -> None:
        self._cards[1].set_value("slave", value)

    @property
    def master(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of master solid elements, see *PART or *SET_PART.
        """ # nopep8
        return self._cards[1].get_value("master")

    @master.setter
    def master(self, value: int) -> None:
        self._cards[1].set_value("master", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Slave type:
        EQ.0: part set ID,
        EQ.1: part ID,
        EQ.2: segment set ID.
        """ # nopep8
        return self._cards[1].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sstyp must be one of {0,1,2}""")
        self._cards[1].set_value("sstyp", value)

    @property
    def mstyp(self) -> int:
        """Get or set the Master type:
        EQ.0: part set ID,
        EQ.1: part ID.
        """ # nopep8
        return self._cards[1].get_value("mstyp")

    @mstyp.setter
    def mstyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mstyp must be one of {0,1}""")
        self._cards[1].set_value("mstyp", value)

    @property
    def nquad(self) -> int:
        """Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
        EQ.0: at nodes only,
        EQ.n: use a rectangular grid of n*n points,
        EQ.-n: at nodes and a rectangular grid of n*n points.
        """ # nopep8
        return self._cards[1].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[1].set_value("nquad", value)

    @property
    def ctype(self) -> int:
        """Get or set the Coupling type
        EQ.1: constrained acceleration,
        EQ.2: constrained acceleration and velocity (default),
        EQ.3: constrained acceleration and velocity, normal direction only,
        EQ.4: penalty coupling (Shell and solid Elements),
        EQ.5: penalty coupling allowing erosion in the lagrangian entities (Solid Elements).
        EQ.6: Penalty coupling designed for airbag modeling(testing).DIREC is automatically reset to DIREC=1.
        """ # nopep8
        return self._cards[1].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5, 6, 11, 12]:
            raise Exception("""ctype must be one of {2,1,3,4,5,6,11,12}""")
        self._cards[1].set_value("ctype", value)

    @property
    def direc(self) -> int:
        """Get or set the Coupling direction (CTYPE 4 and 5).
        EQ.1: normal direction, compression and tension (default),
        EQ.2: normal direction, compression only,
        EQ.3: all directions.
        """ # nopep8
        return self._cards[1].get_value("direc")

    @direc.setter
    def direc(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""direc must be one of {1,2,3}""")
        self._cards[1].set_value("direc", value)

    @property
    def mcoup(self) -> int:
        """Get or set the Multi-material option (CTYPE 4 and 5).
        EQ.0: couple with all multi-material groups,
        EQ.1: couple with material with highest density.
        """ # nopep8
        return self._cards[1].get_value("mcoup")

    @mcoup.setter
    def mcoup(self, value: int) -> None:
        self._cards[1].set_value("mcoup", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for coupling (default=0.0).
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time for coupling (default=1.0E+10).
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        self._cards[2].set_value("end", value)

    @property
    def pfac(self) -> float:
        """Get or set the Penalty factor (CTYPE 4 and 5 only).
        """ # nopep8
        return self._cards[2].get_value("pfac")

    @pfac.setter
    def pfac(self, value: float) -> None:
        self._cards[2].set_value("pfac", value)

    @property
    def fric(self) -> float:
        """Get or set the Coefficient of friction (DIREC 2 only).
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[2].set_value("fric", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
        """ # nopep8
        return self._cards[2].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        self._cards[2].set_value("frcmin", value)

    @property
    def norm(self) -> int:
        """Get or set the Shell and segment normal orientation:
        EQ.0: right hand rule (default)
        EQ.1: left hand rule.
        """ # nopep8
        return self._cards[2].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        self._cards[2].set_value("norm", value)

    @property
    def normtyp(self) -> int:
        """Get or set the Penality spring direction(DIREC 1 and 2 ):
        EQ.0: interpolated from node normals(default),
        EQ.1: segment normal.
        """ # nopep8
        return self._cards[2].get_value("normtyp")

    @normtyp.setter
    def normtyp(self, value: int) -> None:
        self._cards[2].set_value("normtyp", value)

    @property
    def damp(self) -> float:
        """Get or set the Damping factor for penalty coupling. This is a coupling-damping
        scaling factor. Typically it may be between 0 and 1 (see Remark 7).
        """ # nopep8
        return self._cards[2].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[2].set_value("damp", value)

    @property
    def cq(self) -> float:
        """Get or set the Heat transfer coefficient.
        """ # nopep8
        return self._cards[3].get_value("cq")

    @cq.setter
    def cq(self, value: float) -> None:
        self._cards[3].set_value("cq", value)

    @property
    def hmin(self) -> typing.Optional[float]:
        """Get or set the Minmum air gap in heat transfer
        """ # nopep8
        return self._cards[3].get_value("hmin")

    @hmin.setter
    def hmin(self, value: float) -> None:
        self._cards[3].set_value("hmin", value)

    @property
    def hmax(self) -> typing.Optional[float]:
        """Get or set the Maximum air gap in heat transfer. there is no heat transfer above this value.
        """ # nopep8
        return self._cards[3].get_value("hmax")

    @hmax.setter
    def hmax(self, value: float) -> None:
        self._cards[3].set_value("hmax", value)

    @property
    def ileak(self) -> int:
        """Get or set the Leakage control:
        EQ.0: none(default),
        EQ.1: weak,
        EQ.2: strong.
        """ # nopep8
        return self._cards[3].get_value("ileak")

    @ileak.setter
    def ileak(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ileak must be one of {0,1,2}""")
        self._cards[3].set_value("ileak", value)

    @property
    def pleak(self) -> float:
        """Get or set the Leakage control penalty factor
        """ # nopep8
        return self._cards[3].get_value("pleak")

    @pleak.setter
    def pleak(self, value: float) -> None:
        self._cards[3].set_value("pleak", value)

    @property
    def lcidpor(self) -> typing.Optional[int]:
        """Get or set the A load curve ID(LCID) defining porours flow through coupling segment.
        """ # nopep8
        return self._cards[3].get_value("lcidpor")

    @lcidpor.setter
    def lcidpor(self, value: int) -> None:
        self._cards[3].set_value("lcidpor", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vents defined below
        """ # nopep8
        return self._cards[3].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        self._cards[3].set_value("nvent", value)

    @property
    def blockage(self) -> int:
        """Get or set the Blockage consideration flag.
        EQ.0 blockage is not considered.
        EQ blockage is considered for venting and porosity
        """ # nopep8
        return self._cards[3].get_value("blockage")

    @blockage.setter
    def blockage(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""blockage must be one of {0,1}""")
        self._cards[3].set_value("blockage", value)

    @property
    def iboxid(self) -> int:
        """Get or set the A box ID defining a box region in space in which ALE coupling is activated.  At time=0.0, the number of Lagrangian segments inside this box is remembered. In subsequent coupling computation steps, there is no need to search for the Lagrangian segments again.
        """ # nopep8
        return self._cards[4].get_value("iboxid")

    @iboxid.setter
    def iboxid(self, value: int) -> None:
        self._cards[4].set_value("iboxid", value)

    @property
    def ipenchk(self) -> int:
        """Get or set the Initial penetration check flag (only for CTYPE=4, Remark 13): 	EQ.0: Do not check for initial penetration.EQ.1: Check and save initial ALE material penetration across a Lagrangian surface (d0), but do not activate coupling at t=0.  In subsequent steps (t>0) the actual penetration is computed as follows actual penetration 	= total penetration ¨C initial penetration da=dT ¨C d0
        """ # nopep8
        return self._cards[4].get_value("ipenchk")

    @ipenchk.setter
    def ipenchk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ipenchk must be one of {0,1}""")
        self._cards[4].set_value("ipenchk", value)

    @property
    def intforc(self) -> int:
        """Get or set the A flag to turn on or off (0=OFF or 1=ON) the output of ALE coupling pressure and forces on the slave Lagrangian segments (or surfaces).  Note that the coupling pressures and forces are computed based on the ALE fluid penetrations and coupling stiffness of the system.  When (1) INTFORC=1 and (2) a *DATABASE_BINARY_FSIFOR (DBF) card is defined, LS-DYNA writes out the segment coupling pressure and forces to the binary interface force file for contour plotting.  This interface force file is activated by executing ls971 as follows (3):	 ls971 i=inputfilename.k   h=interfaceforcefilename The time interval between output is defined by  dt  in the DBF card.  To plot the binary data in this file: lsprepost interfaceforcefilename.
        """ # nopep8
        return self._cards[4].get_value("intforc")

    @intforc.setter
    def intforc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""intforc must be one of {0,1}""")
        self._cards[4].set_value("intforc", value)

    @property
    def ialesof(self) -> int:
        """Get or set the An integer flag to turn ON/OFF a supplemental Lagrange multiplier FSI constraint which provides a coupling force in addition to the basic penalty coupling contribution.  This is a hybrid coupling method.EQ.0: OFF (default).EQ.1: Turn ON the hybrid Lagrange-multiplier method.  LAGMUL multiplier factor is read.
        """ # nopep8
        return self._cards[4].get_value("ialesof")

    @ialesof.setter
    def ialesof(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ialesof must be one of {0,1}""")
        self._cards[4].set_value("ialesof", value)

    @property
    def lagmul(self) -> float:
        """Get or set the A Lagrange multiplier factor with a range between 0.0 and 0.05 may be defined.  A typical value may be 0.01.  This should never be greater than 0.1. 	EQ.0: OFF (default).GT.0: Turn ON the Lagrange-multiplier method and use LAGMUL as a coefficient for scaling the penalty factor
        """ # nopep8
        return self._cards[4].get_value("lagmul")

    @lagmul.setter
    def lagmul(self, value: float) -> None:
        self._cards[4].set_value("lagmul", value)

    @property
    def pfacmm(self) -> int:
        """Get or set the Mass-based penalty stiffness factor computational options.  This works in conjunction with PFAC=constant (not a load curve).  The coupling penalty stiffness (CPS) is computed based on an estimated effective coupling mass.
        """ # nopep8
        return self._cards[4].get_value("pfacmm")

    @pfacmm.setter
    def pfacmm(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""pfacmm must be one of {0,1,2,3}""")
        self._cards[4].set_value("pfacmm", value)

    @property
    def thkf(self) -> float:
        """Get or set the (For all CTYPE choices except 11) A flag to account for the coupling thickness of the Lagrangian shell (slave) part.  LT.0: Use positive value of |THKF| for coupling segment thickness.EQ.0: Do not consider coupling segment thickness.GT.0: Coupling segment thickness scale factor.		For CTYPE=11 case (see Remark 14):  This thickness is required for volume calculation.GT.0: (Fabric) Thickness scale factor.  The base shell thickness is taken from the *PART definition.LT.0: User-defined (Fabric) thickness.  The fabric thickness is set to |THKF|.
        """ # nopep8
        return self._cards[4].get_value("thkf")

    @thkf.setter
    def thkf(self, value: float) -> None:
        self._cards[4].set_value("thkf", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=11, A1 = An = coefficient for normal-to-segment direction.For CTYPE=12: A1 = Ax = coefficient for global X-direction
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14).	For CTYPE=11, B1 = Bn = coefficient for normal-to-segment direction.		For CTYPE=12: B1 = Bx = coefficient for global X-direction
        """ # nopep8
        return self._cards[5].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[5].set_value("b1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A2 = Ay = coefficient for global Y-direction
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: B2 = By = coefficient for global Y-direction
        """ # nopep8
        return self._cards[5].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[5].set_value("b2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A3 = Az = coefficient for global Z-direction
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14).	For CTYPE=12: B3 = Bz = coefficient for global Z-direction
        """ # nopep8
        return self._cards[5].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[5].set_value("b3", value)

    @property
    def ventsid(self) -> typing.Optional[int]:
        """Get or set the sid
        """ # nopep8
        return self._cards[6].get_value("ventsid")

    @ventsid.setter
    def ventsid(self, value: int) -> None:
        self._cards[6].set_value("ventsid", value)

    @property
    def ventyp(self) -> int:
        """Get or set the EQ.0 partset
        EQ .1 part
        EQ.2 segmentset
        """ # nopep8
        return self._cards[6].get_value("ventyp")

    @ventyp.setter
    def ventyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ventyp must be one of {0,1,2}""")
        self._cards[6].set_value("ventyp", value)

    @property
    def vtcoef(self) -> int:
        """Get or set the Flow coefficient for each vent surface area
        """ # nopep8
        return self._cards[6].get_value("vtcoef")

    @vtcoef.setter
    def vtcoef(self, value: int) -> None:
        self._cards[6].set_value("vtcoef", value)

    @property
    def poppres(self) -> float:
        """Get or set the sid
        """ # nopep8
        return self._cards[6].get_value("poppres")

    @poppres.setter
    def poppres(self, value: float) -> None:
        self._cards[6].set_value("poppres", value)

    @property
    def coeflc(self) -> int:
        """Get or set the EQ.0 partset
        EQ .1 part
        EQ.2 segmentset
        """ # nopep8
        return self._cards[6].get_value("coeflc")

    @coeflc.setter
    def coeflc(self, value: int) -> None:
        self._cards[6].set_value("coeflc", value)

