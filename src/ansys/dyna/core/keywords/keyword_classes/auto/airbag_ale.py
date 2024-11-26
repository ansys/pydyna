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

class AirbagAle(KeywordBase):
    """DYNA AIRBAG_ALE keyword"""

    keyword = "AIRBAG"
    subkeyword = "ALE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "atmost",
                        float,
                        0,
                        10,
                        kwargs.get("atmost", 0.0)
                    ),
                    Field(
                        "atmosp",
                        float,
                        10,
                        10,
                        kwargs.get("atmosp", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "gc",
                        float,
                        30,
                        10,
                        kwargs.get("gc")
                    ),
                    Field(
                        "cc",
                        float,
                        40,
                        10,
                        kwargs.get("cc", 1.0)
                    ),
                    Field(
                        "tnkvol",
                        float,
                        50,
                        10,
                        kwargs.get("tnkvol", 0.0)
                    ),
                    Field(
                        "tnkfinp",
                        float,
                        60,
                        10,
                        kwargs.get("tnkfinp", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nquad",
                        int,
                        0,
                        10,
                        kwargs.get("nquad", 4)
                    ),
                    Field(
                        "ctype",
                        int,
                        10,
                        10,
                        kwargs.get("ctype", 4)
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
                        kwargs.get("frcmin", 0.3)
                    ),
                    Field(
                        "normtyp",
                        int,
                        50,
                        10,
                        kwargs.get("normtyp", 0)
                    ),
                    Field(
                        "ileak",
                        int,
                        60,
                        10,
                        kwargs.get("ileak", 2)
                    ),
                    Field(
                        "pleak",
                        float,
                        70,
                        10,
                        kwargs.get("pleak", 0.1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ivsetid",
                        int,
                        0,
                        10,
                        kwargs.get("ivsetid", 0)
                    ),
                    Field(
                        "ivtype",
                        int,
                        10,
                        10,
                        kwargs.get("ivtype", 0)
                    ),
                    Field(
                        "iblock",
                        int,
                        20,
                        10,
                        kwargs.get("iblock", 0)
                    ),
                    Field(
                        "vntcof",
                        float,
                        30,
                        10,
                        kwargs.get("vntcof", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nx/ida",
                        int,
                        0,
                        10,
                        kwargs.get("nx/ida")
                    ),
                    Field(
                        "ny/idg",
                        int,
                        10,
                        10,
                        kwargs.get("ny/idg")
                    ),
                    Field(
                        "nz",
                        int,
                        20,
                        10,
                        kwargs.get("nz", 1)
                    ),
                    Field(
                        "movern",
                        int,
                        30,
                        10,
                        kwargs.get("movern", 0)
                    ),
                    Field(
                        "zoom",
                        int,
                        40,
                        10,
                        kwargs.get("zoom", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x0",
                        float,
                        0,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "y0",
                        float,
                        10,
                        10,
                        kwargs.get("y0")
                    ),
                    Field(
                        "z0",
                        float,
                        20,
                        10,
                        kwargs.get("z0")
                    ),
                    Field(
                        "x1",
                        float,
                        30,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "y1",
                        float,
                        40,
                        10,
                        kwargs.get("y1")
                    ),
                    Field(
                        "z1",
                        float,
                        50,
                        10,
                        kwargs.get("z1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x2",
                        float,
                        0,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "y2",
                        float,
                        10,
                        10,
                        kwargs.get("y2")
                    ),
                    Field(
                        "z2",
                        float,
                        20,
                        10,
                        kwargs.get("z2")
                    ),
                    Field(
                        "x3",
                        float,
                        30,
                        10,
                        kwargs.get("x3")
                    ),
                    Field(
                        "y3",
                        float,
                        40,
                        10,
                        kwargs.get("y3")
                    ),
                    Field(
                        "z3",
                        float,
                        50,
                        10,
                        kwargs.get("z3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "swtime",
                        float,
                        0,
                        10,
                        kwargs.get("swtime", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "hg",
                        float,
                        20,
                        10,
                        kwargs.get("hg", 0.0)
                    ),
                    Field(
                        "nair",
                        int,
                        30,
                        10,
                        kwargs.get("nair", 0)
                    ),
                    Field(
                        "ngas",
                        int,
                        40,
                        10,
                        kwargs.get("ngas", 0)
                    ),
                    Field(
                        "norif",
                        int,
                        50,
                        10,
                        kwargs.get("norif", 0)
                    ),
                    Field(
                        "lcvel",
                        int,
                        60,
                        10,
                        kwargs.get("lcvel")
                    ),
                    Field(
                        "lct",
                        int,
                        70,
                        10,
                        kwargs.get("lct")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
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
                        "mwair",
                        float,
                        30,
                        10,
                        kwargs.get("mwair", 0)
                    ),
                    Field(
                        "initm",
                        float,
                        40,
                        10,
                        kwargs.get("initm", 0)
                    ),
                    Field(
                        "aira",
                        float,
                        50,
                        10,
                        kwargs.get("aira", 0)
                    ),
                    Field(
                        "airb",
                        float,
                        60,
                        10,
                        kwargs.get("airb", 0)
                    ),
                    Field(
                        "airc",
                        float,
                        70,
                        10,
                        kwargs.get("airc", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcmf",
                        int,
                        0,
                        10,
                        kwargs.get("lcmf")
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
                        "mwgas",
                        float,
                        30,
                        10,
                        kwargs.get("mwgas", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "gasa",
                        float,
                        50,
                        10,
                        kwargs.get("gasa", 0)
                    ),
                    Field(
                        "gasb",
                        float,
                        60,
                        10,
                        kwargs.get("gasb", 0)
                    ),
                    Field(
                        "gasc",
                        float,
                        70,
                        10,
                        kwargs.get("gasc", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodeid",
                        int,
                        0,
                        10,
                        kwargs.get("nodeid", 0)
                    ),
                    Field(
                        "vecid",
                        int,
                        10,
                        10,
                        kwargs.get("vecid", 0)
                    ),
                    Field(
                        "orifare",
                        float,
                        20,
                        10,
                        kwargs.get("orifare", 0)
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.  This set ID contains the Lagrangian elements (segments) which make up the airbag and possibly the airbag canister/compartment and/or a simple representation of the inflator.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: Segment set
        EQ.1: Part set
        """ # nopep8
        return self._cards[0].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D. Used during the CV phase
        """ # nopep8
        return self._cards[0].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[0].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0≤γ≤1.  SPSF is needed during the CV phase.
        """ # nopep8
        return self._cards[0].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[0].set_value("spsf", value)

    @property
    def atmost(self) -> float:
        """Get or set the Atmospheric ambient temperature.  See Remark 2
        """ # nopep8
        return self._cards[1].get_value("atmost")

    @atmost.setter
    def atmost(self, value: float) -> None:
        self._cards[1].set_value("atmost", value)

    @property
    def atmosp(self) -> float:
        """Get or set the Atmospheric ambient pressure.  See Remark 2
        """ # nopep8
        return self._cards[1].get_value("atmosp")

    @atmosp.setter
    def atmosp(self, value: float) -> None:
        self._cards[1].set_value("atmosp", value)

    @property
    def gc(self) -> typing.Optional[float]:
        """Get or set the Universal molar gas constant
        """ # nopep8
        return self._cards[1].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        self._cards[1].set_value("gc", value)

    @property
    def cc(self) -> float:
        """Get or set the Conversion constant. EQ.0:	Set to 1.0
        """ # nopep8
        return self._cards[1].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        self._cards[1].set_value("cc", value)

    @property
    def tnkvol(self) -> float:
        """Get or set the Tank volume from the inflator tank test or inflator canister volume.
        LCVEL = 0 and TNKFINP is defined:
        TNKVOL is the defined tank.  Inlet gas velocity is estimated by LS-DYNA method (testing).
        LCVEL = 0 and TNKFINP is not defined:
        TNKVOL is the estimated inflator canister volume inlet gas velocity is estimated automatically by the Lian-Bhalsod-Olovssonmethod.
        LCVEL ≠ 0:
        This must be left blank
        """ # nopep8
        return self._cards[1].get_value("tnkvol")

    @tnkvol.setter
    def tnkvol(self, value: float) -> None:
        self._cards[1].set_value("tnkvol", value)

    @property
    def tnkfinp(self) -> float:
        """Get or set the Tank final pressure from the inflator tank test data. Only define this parameter for option 1 of TNKVOL definition above.  See Remark 10
        """ # nopep8
        return self._cards[1].get_value("tnkfinp")

    @tnkfinp.setter
    def tnkfinp(self, value: float) -> None:
        self._cards[1].set_value("tnkfinp", value)

    @property
    def nquad(self) -> int:
        """Get or set the Number of (quadrature) coupling points for coupling Lagrangian parts to ALE master solid parts.
        If NQUAD = n, then nxn coupling points will be parametrically distributed over the surface of each Lagrangian segment.
        """ # nopep8
        return self._cards[2].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[2].set_value("nquad", value)

    @property
    def ctype(self) -> int:
        """Get or set the Coupling type (see Remark 12):
        EQ.4:	Penalty coupling with coupling in the normal direction under compression only(default).
        EQ.6 : Penalty coupling in which coupling is under both tension and compression in the normal direction for the unfolded regionand under only compression in the normal direction for folded region.
        """ # nopep8
        return self._cards[2].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        self._cards[2].set_value("ctype", value)

    @property
    def pfac(self) -> float:
        """Get or set the Penalty factor.  PFAC is a scale factor for scaling the estimated stiffness of the interacting (coupling) system.  It is used to compute the coupling forces to be distributed on lagrangian and ALE parts.  See Remark 13.
        GT.0:	Fraction of estimated critical stiffness.
        LT.0:	-PFAC is a load curve ID.  The curve defines the relative coupling pressure (y-axis) as a function of the tolerable fluid penetration distance (x-axis)
        """ # nopep8
        return self._cards[2].get_value("pfac")

    @pfac.setter
    def pfac(self, value: float) -> None:
        self._cards[2].set_value("pfac", value)

    @property
    def fric(self) -> float:
        """Get or set the Coupling coefficient of friction.
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[2].set_value("fric", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Minimum fluid volume fraction in an ALE element to activate coupling.
        """ # nopep8
        return self._cards[2].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        self._cards[2].set_value("frcmin", value)

    @property
    def normtyp(self) -> int:
        """Get or set the Penalty coupling spring direction:
        EQ.0:	Normal vectors are interpolated from nodal normals
        EQ.1:	Normal vectors are interpolated from segment normals.
        """ # nopep8
        return self._cards[2].get_value("normtyp")

    @normtyp.setter
    def normtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""normtyp must be one of {0,1}""")
        self._cards[2].set_value("normtyp", value)

    @property
    def ileak(self) -> int:
        """Get or set the Leakage control flag. Default = 2 (with energy compensation)
        """ # nopep8
        return self._cards[2].get_value("ileak")

    @ileak.setter
    def ileak(self, value: int) -> None:
        self._cards[2].set_value("ileak", value)

    @property
    def pleak(self) -> float:
        """Get or set the Leakage control penalty factor (default = 0.1)
        """ # nopep8
        return self._cards[2].get_value("pleak")

    @pleak.setter
    def pleak(self, value: float) -> None:
        self._cards[2].set_value("pleak", value)

    @property
    def ivsetid(self) -> int:
        """Get or set the Set ID defining the venting hole surface(s).  See Remark 4
        """ # nopep8
        return self._cards[3].get_value("ivsetid")

    @ivsetid.setter
    def ivsetid(self, value: int) -> None:
        self._cards[3].set_value("ivsetid", value)

    @property
    def ivtype(self) -> int:
        """Get or set the Type of IVSET:
        EQ.0: Part Set
        EQ.1: Part ID
        EQ.2: Segment Set
        """ # nopep8
        return self._cards[3].get_value("ivtype")

    @ivtype.setter
    def ivtype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ivtype must be one of {0,1,2}""")
        self._cards[3].set_value("ivtype", value)

    @property
    def iblock(self) -> int:
        """Get or set the Flag for considering blockage effects for porosity and vents (see Remark 5):
        EQ.0:	no (blockage is NOT considered, default).
        EQ.1:	yes (blockage is considered)
        """ # nopep8
        return self._cards[3].get_value("iblock")

    @iblock.setter
    def iblock(self, value: int) -> None:
        self._cards[3].set_value("iblock", value)

    @property
    def vntcof(self) -> float:
        """Get or set the Vent Coefficient for scaling the flow.  See Remark 6
        """ # nopep8
        return self._cards[3].get_value("vntcof")

    @vntcof.setter
    def vntcof(self, value: float) -> None:
        self._cards[3].set_value("vntcof", value)

    @property
    def nx_ida(self) -> typing.Optional[int]:
        """Get or set the NX is the number of ALE elements to be generated in the x-direction.
        IDA is the Part ID of the initial air mesh
        """ # nopep8
        return self._cards[4].get_value("nx/ida")

    @nx_ida.setter
    def nx_ida(self, value: int) -> None:
        self._cards[4].set_value("nx/ida", value)

    @property
    def ny_idg(self) -> typing.Optional[int]:
        """Get or set the NY is the number of ALE elements to be generated in the y-direction
        IDG is the Part ID of the initial air mesh
        """ # nopep8
        return self._cards[4].get_value("ny/idg")

    @ny_idg.setter
    def ny_idg(self, value: int) -> None:
        self._cards[4].set_value("ny/idg", value)

    @property
    def nz(self) -> int:
        """Get or set the NZ is the number of ALE elements to be generated in the z-direction
        Leave blank to activate
        """ # nopep8
        return self._cards[4].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        self._cards[4].set_value("nz", value)

    @property
    def movern(self) -> int:
        """Get or set the ALE mesh automatic motion option.
        EQ.0:	ALE mesh is fixed in space.
        GT.0:	Node group ID.  See *ALE_REFERENCE_SYSTEM_NODE ALE mesh can be moved with PRTYP = 5,
        mesh motion follows a coordinate system defined by 3 reference nodes.  See Remark 7
        """ # nopep8
        return self._cards[4].get_value("movern")

    @movern.setter
    def movern(self, value: int) -> None:
        self._cards[4].set_value("movern", value)

    @property
    def zoom(self) -> int:
        """Get or set the ALE mesh automatic expansion option:
        EQ.0:	Do not expand ALE mesh
        EQ.1:	Expand/contract ALE mesh by keeping all airbag parts contained within the ALE mesh (equivalent to PRTYP = 9).  See Remark 8.
        """ # nopep8
        return self._cards[4].get_value("zoom")

    @zoom.setter
    def zoom(self, value: int) -> None:
        self._cards[4].set_value("zoom", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[5].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[5].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[5].set_value("z0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[5].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[5].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[5].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[6].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[6].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[6].set_value("z2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        self._cards[6].set_value("x3", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        self._cards[6].set_value("y3", value)

    @property
    def z3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("z3")

    @z3.setter
    def z3(self, value: float) -> None:
        self._cards[6].set_value("z3", value)

    @property
    def swtime(self) -> float:
        """Get or set the Time to switch from ALE bag to control volume (AIRBAG_HYBRID). EQ:0.0 switch to control volume will take place at time equal 0.0. If this field is not defined (blnak) switch time will be set to 1.0e16.
        """ # nopep8
        return self._cards[7].get_value("swtime")

    @swtime.setter
    def swtime(self, value: float) -> None:
        self._cards[7].set_value("swtime", value)

    @property
    def hg(self) -> float:
        """Get or set the Hourglass coefficient for ALE fluid mesh
        """ # nopep8
        return self._cards[7].get_value("hg")

    @hg.setter
    def hg(self, value: float) -> None:
        self._cards[7].set_value("hg", value)

    @property
    def nair(self) -> int:
        """Get or set the Number of air components.  For example, this equals 2 when air contains 80% of N2 and 20% of O2.  If air is defined as a single gas, then NAIR = 1
        """ # nopep8
        return self._cards[7].get_value("nair")

    @nair.setter
    def nair(self, value: int) -> None:
        self._cards[7].set_value("nair", value)

    @property
    def ngas(self) -> int:
        """Get or set the Number of inflator gas components
        """ # nopep8
        return self._cards[7].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        self._cards[7].set_value("ngas", value)

    @property
    def norif(self) -> int:
        """Get or set the Number of point sources or orifices.  This determines the number of point source cards to be read
        """ # nopep8
        return self._cards[7].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        self._cards[7].set_value("norif", value)

    @property
    def lcvel(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for inlet velocity (see also TNKVOL & TNKFINP of Card 2 above).
        This is the same estimated velocity curve used in *SECTION_POINT_SOURCE_MIXTURE card.
        """ # nopep8
        return self._cards[7].get_value("lcvel")

    @lcvel.setter
    def lcvel(self, value: int) -> None:
        self._cards[7].set_value("lcvel", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for inlet gas temperature (see *AIRBAG_HYBRID)
        """ # nopep8
        return self._cards[7].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        self._cards[7].set_value("lct", value)

    @property
    def mwair(self) -> float:
        """Get or set the Molecular weight of air component
        """ # nopep8
        return self._cards[8].get_value("mwair")

    @mwair.setter
    def mwair(self, value: float) -> None:
        self._cards[8].set_value("mwair", value)

    @property
    def initm(self) -> float:
        """Get or set the Initial Mass Fraction of air component(s)
        """ # nopep8
        return self._cards[8].get_value("initm")

    @initm.setter
    def initm(self, value: float) -> None:
        self._cards[8].set_value("initm", value)

    @property
    def aira(self) -> float:
        """Get or set the First Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("aira")

    @aira.setter
    def aira(self, value: float) -> None:
        self._cards[8].set_value("aira", value)

    @property
    def airb(self) -> float:
        """Get or set the Second Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("airb")

    @airb.setter
    def airb(self, value: float) -> None:
        self._cards[8].set_value("airb", value)

    @property
    def airc(self) -> float:
        """Get or set the Third Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("airc")

    @airc.setter
    def airc(self, value: float) -> None:
        self._cards[8].set_value("airc", value)

    @property
    def lcmf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for mass flow rate
        """ # nopep8
        return self._cards[9].get_value("lcmf")

    @lcmf.setter
    def lcmf(self, value: int) -> None:
        self._cards[9].set_value("lcmf", value)

    @property
    def mwgas(self) -> float:
        """Get or set the Molecular weight of inflator gas components
        """ # nopep8
        return self._cards[9].get_value("mwgas")

    @mwgas.setter
    def mwgas(self, value: float) -> None:
        self._cards[9].set_value("mwgas", value)

    @property
    def gasa(self) -> float:
        """Get or set the First Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasa")

    @gasa.setter
    def gasa(self, value: float) -> None:
        self._cards[9].set_value("gasa", value)

    @property
    def gasb(self) -> float:
        """Get or set the Second Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasb")

    @gasb.setter
    def gasb(self, value: float) -> None:
        self._cards[9].set_value("gasb", value)

    @property
    def gasc(self) -> float:
        """Get or set the Third Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        self._cards[9].set_value("gasc", value)

    @property
    def nodeid(self) -> int:
        """Get or set the Node ID defining the point source
        """ # nopep8
        return self._cards[10].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[10].set_value("nodeid", value)

    @property
    def vecid(self) -> int:
        """Get or set the Vector Id defining the direction of flow at the point source
        """ # nopep8
        return self._cards[10].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[10].set_value("vecid", value)

    @property
    def orifare(self) -> float:
        """Get or set the Orifice area at the point source
        """ # nopep8
        return self._cards[10].get_value("orifare")

    @orifare.setter
    def orifare(self, value: float) -> None:
        self._cards[10].set_value("orifare", value)

