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

"""Module providing the AirbagAle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGALE_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("sidtyp", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("mwd", float, 60, 10, 0.0),
    FieldSchema("spsf", float, 70, 10, 0.0),
)

_AIRBAGALE_CARD1 = (
    FieldSchema("atmost", float, 0, 10, 0.0),
    FieldSchema("atmosp", float, 10, 10, 0.0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("gc", float, 30, 10, None),
    FieldSchema("cc", float, 40, 10, 1.0),
    FieldSchema("tnkvol", float, 50, 10, 0.0),
    FieldSchema("tnkfinp", float, 60, 10, 0.0),
)

_AIRBAGALE_CARD2 = (
    FieldSchema("nquad", int, 0, 10, 4),
    FieldSchema("ctype", int, 10, 10, 4),
    FieldSchema("pfac", float, 20, 10, 0.1),
    FieldSchema("fric", float, 30, 10, 0.0),
    FieldSchema("frcmin", float, 40, 10, 0.3),
    FieldSchema("normtyp", int, 50, 10, 0),
    FieldSchema("ileak", int, 60, 10, 2),
    FieldSchema("pleak", float, 70, 10, 0.1),
)

_AIRBAGALE_CARD3 = (
    FieldSchema("ivsetid", int, 0, 10, 0),
    FieldSchema("ivtype", int, 10, 10, 0),
    FieldSchema("iblock", int, 20, 10, 0),
    FieldSchema("vntcof", float, 30, 10, 0.0),
)

_AIRBAGALE_CARD4 = (
    FieldSchema("nx_ida", int, 0, 10, None, "nx/ida"),
    FieldSchema("ny_idg", int, 10, 10, None, "ny/idg"),
    FieldSchema("nz", int, 20, 10, 1),
    FieldSchema("movern", int, 30, 10, 0),
    FieldSchema("zoom", int, 40, 10, 0),
)

_AIRBAGALE_CARD5 = (
    FieldSchema("x0", float, 0, 10, None),
    FieldSchema("y0", float, 10, 10, None),
    FieldSchema("z0", float, 20, 10, None),
    FieldSchema("x1", float, 30, 10, None),
    FieldSchema("y1", float, 40, 10, None),
    FieldSchema("z1", float, 50, 10, None),
)

_AIRBAGALE_CARD6 = (
    FieldSchema("x2", float, 0, 10, None),
    FieldSchema("y2", float, 10, 10, None),
    FieldSchema("z2", float, 20, 10, None),
    FieldSchema("x3", float, 30, 10, None),
    FieldSchema("y3", float, 40, 10, None),
    FieldSchema("z3", float, 50, 10, None),
)

_AIRBAGALE_CARD7 = (
    FieldSchema("swtime", float, 0, 10, 0.0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("hg", float, 20, 10, 0.0),
    FieldSchema("nair", int, 30, 10, 0),
    FieldSchema("ngas", int, 40, 10, 0),
    FieldSchema("norif", int, 50, 10, 0),
    FieldSchema("lcvel", int, 60, 10, None),
    FieldSchema("lct", int, 70, 10, None),
)

_AIRBAGALE_CARD8 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("mwair", float, 30, 10, 0.0),
    FieldSchema("initm", float, 40, 10, 0.0),
    FieldSchema("aira", float, 50, 10, 0.0),
    FieldSchema("airb", float, 60, 10, 0.0),
    FieldSchema("airc", float, 70, 10, 0.0),
)

_AIRBAGALE_CARD9 = (
    FieldSchema("lcmf", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("mwgas", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("gasa", float, 50, 10, 0.0),
    FieldSchema("gasb", float, 60, 10, 0.0),
    FieldSchema("gasc", float, 70, 10, 0.0),
)

_AIRBAGALE_CARD10 = (
    FieldSchema("nodeid", int, 0, 10, 0),
    FieldSchema("vecid", int, 10, 10, 0),
    FieldSchema("orifare", float, 20, 10, 0.0),
)

class AirbagAle(KeywordBase):
    """DYNA AIRBAG_ALE keyword"""

    keyword = "AIRBAG"
    subkeyword = "ALE"
    _link_fields = {
        "lcvel": LinkType.DEFINE_CURVE,
        "lct": LinkType.DEFINE_CURVE,
        "lcmf": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagAle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGALE_CARD10,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.  This set ID contains the Lagrangian elements (segments) which make up the airbag and possibly the airbag canister/compartment and/or a simple representation of the inflator.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
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
        """Set the sidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sidtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D. Used during the CV phase
        """ # nopep8
        return self._cards[0].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        """Set the mwd property."""
        self._cards[0].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0≤γ≤1.  SPSF is needed during the CV phase.
        """ # nopep8
        return self._cards[0].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        """Set the spsf property."""
        self._cards[0].set_value("spsf", value)

    @property
    def atmost(self) -> float:
        """Get or set the Atmospheric ambient temperature.  See Remark 2
        """ # nopep8
        return self._cards[1].get_value("atmost")

    @atmost.setter
    def atmost(self, value: float) -> None:
        """Set the atmost property."""
        self._cards[1].set_value("atmost", value)

    @property
    def atmosp(self) -> float:
        """Get or set the Atmospheric ambient pressure.  See Remark 2
        """ # nopep8
        return self._cards[1].get_value("atmosp")

    @atmosp.setter
    def atmosp(self, value: float) -> None:
        """Set the atmosp property."""
        self._cards[1].set_value("atmosp", value)

    @property
    def gc(self) -> typing.Optional[float]:
        """Get or set the Universal molar gas constant
        """ # nopep8
        return self._cards[1].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        """Set the gc property."""
        self._cards[1].set_value("gc", value)

    @property
    def cc(self) -> float:
        """Get or set the Conversion constant. EQ.0:	Set to 1.0
        """ # nopep8
        return self._cards[1].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        """Set the cc property."""
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
        """Set the tnkvol property."""
        self._cards[1].set_value("tnkvol", value)

    @property
    def tnkfinp(self) -> float:
        """Get or set the Tank final pressure from the inflator tank test data. Only define this parameter for option 1 of TNKVOL definition above.  See Remark 10
        """ # nopep8
        return self._cards[1].get_value("tnkfinp")

    @tnkfinp.setter
    def tnkfinp(self, value: float) -> None:
        """Set the tnkfinp property."""
        self._cards[1].set_value("tnkfinp", value)

    @property
    def nquad(self) -> int:
        """Get or set the Number of (quadrature) coupling points for coupling Lagrangian parts to ALE master solid parts.
        If NQUAD = n, then nxn coupling points will be parametrically distributed over the surface of each Lagrangian segment.
        """ # nopep8
        return self._cards[2].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        """Set the nquad property."""
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
        """Set the ctype property."""
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
        """Set the pfac property."""
        self._cards[2].set_value("pfac", value)

    @property
    def fric(self) -> float:
        """Get or set the Coupling coefficient of friction.
        """ # nopep8
        return self._cards[2].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[2].set_value("fric", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Minimum fluid volume fraction in an ALE element to activate coupling.
        """ # nopep8
        return self._cards[2].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        """Set the frcmin property."""
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
        """Set the normtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""normtyp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("normtyp", value)

    @property
    def ileak(self) -> int:
        """Get or set the Leakage control flag. Default = 2 (with energy compensation)
        """ # nopep8
        return self._cards[2].get_value("ileak")

    @ileak.setter
    def ileak(self, value: int) -> None:
        """Set the ileak property."""
        self._cards[2].set_value("ileak", value)

    @property
    def pleak(self) -> float:
        """Get or set the Leakage control penalty factor (default = 0.1)
        """ # nopep8
        return self._cards[2].get_value("pleak")

    @pleak.setter
    def pleak(self, value: float) -> None:
        """Set the pleak property."""
        self._cards[2].set_value("pleak", value)

    @property
    def ivsetid(self) -> int:
        """Get or set the Set ID defining the venting hole surface(s).  See Remark 4
        """ # nopep8
        return self._cards[3].get_value("ivsetid")

    @ivsetid.setter
    def ivsetid(self, value: int) -> None:
        """Set the ivsetid property."""
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
        """Set the ivtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ivtype must be `None` or one of {0,1,2}.""")
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
        """Set the iblock property."""
        self._cards[3].set_value("iblock", value)

    @property
    def vntcof(self) -> float:
        """Get or set the Vent Coefficient for scaling the flow.  See Remark 6
        """ # nopep8
        return self._cards[3].get_value("vntcof")

    @vntcof.setter
    def vntcof(self, value: float) -> None:
        """Set the vntcof property."""
        self._cards[3].set_value("vntcof", value)

    @property
    def nx_ida(self) -> typing.Optional[int]:
        """Get or set the NX is the number of ALE elements to be generated in the x-direction.
        IDA is the Part ID of the initial air mesh
        """ # nopep8
        return self._cards[4].get_value("nx_ida")

    @nx_ida.setter
    def nx_ida(self, value: int) -> None:
        """Set the nx_ida property."""
        self._cards[4].set_value("nx_ida", value)

    @property
    def ny_idg(self) -> typing.Optional[int]:
        """Get or set the NY is the number of ALE elements to be generated in the y-direction
        IDG is the Part ID of the initial air mesh
        """ # nopep8
        return self._cards[4].get_value("ny_idg")

    @ny_idg.setter
    def ny_idg(self, value: int) -> None:
        """Set the ny_idg property."""
        self._cards[4].set_value("ny_idg", value)

    @property
    def nz(self) -> int:
        """Get or set the NZ is the number of ALE elements to be generated in the z-direction
        Leave blank to activate
        """ # nopep8
        return self._cards[4].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        """Set the nz property."""
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
        """Set the movern property."""
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
        """Set the zoom property."""
        self._cards[4].set_value("zoom", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[5].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[5].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[5].set_value("z0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[5].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[5].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[5].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[5].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[6].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[6].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[6].set_value("z2", value)

    @property
    def x3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("x3")

    @x3.setter
    def x3(self, value: float) -> None:
        """Set the x3 property."""
        self._cards[6].set_value("x3", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        """Set the y3 property."""
        self._cards[6].set_value("y3", value)

    @property
    def z3(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
        """ # nopep8
        return self._cards[6].get_value("z3")

    @z3.setter
    def z3(self, value: float) -> None:
        """Set the z3 property."""
        self._cards[6].set_value("z3", value)

    @property
    def swtime(self) -> float:
        """Get or set the Time to switch from ALE bag to control volume (AIRBAG_HYBRID). EQ:0.0 switch to control volume will take place at time equal 0.0. If this field is not defined (blnak) switch time will be set to 1.0e16.
        """ # nopep8
        return self._cards[7].get_value("swtime")

    @swtime.setter
    def swtime(self, value: float) -> None:
        """Set the swtime property."""
        self._cards[7].set_value("swtime", value)

    @property
    def hg(self) -> float:
        """Get or set the Hourglass coefficient for ALE fluid mesh
        """ # nopep8
        return self._cards[7].get_value("hg")

    @hg.setter
    def hg(self, value: float) -> None:
        """Set the hg property."""
        self._cards[7].set_value("hg", value)

    @property
    def nair(self) -> int:
        """Get or set the Number of air components.  For example, this equals 2 when air contains 80% of N2 and 20% of O2.  If air is defined as a single gas, then NAIR = 1
        """ # nopep8
        return self._cards[7].get_value("nair")

    @nair.setter
    def nair(self, value: int) -> None:
        """Set the nair property."""
        self._cards[7].set_value("nair", value)

    @property
    def ngas(self) -> int:
        """Get or set the Number of inflator gas components
        """ # nopep8
        return self._cards[7].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        """Set the ngas property."""
        self._cards[7].set_value("ngas", value)

    @property
    def norif(self) -> int:
        """Get or set the Number of point sources or orifices.  This determines the number of point source cards to be read
        """ # nopep8
        return self._cards[7].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[7].set_value("norif", value)

    @property
    def lcvel(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for inlet velocity (see also TNKVOL & TNKFINP of Card 2 above).
        This is the same estimated velocity curve used in *SECTION_POINT_SOURCE_MIXTURE card.
        """ # nopep8
        return self._cards[7].get_value("lcvel")

    @lcvel.setter
    def lcvel(self, value: int) -> None:
        """Set the lcvel property."""
        self._cards[7].set_value("lcvel", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for inlet gas temperature (see *AIRBAG_HYBRID)
        """ # nopep8
        return self._cards[7].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        """Set the lct property."""
        self._cards[7].set_value("lct", value)

    @property
    def mwair(self) -> float:
        """Get or set the Molecular weight of air component
        """ # nopep8
        return self._cards[8].get_value("mwair")

    @mwair.setter
    def mwair(self, value: float) -> None:
        """Set the mwair property."""
        self._cards[8].set_value("mwair", value)

    @property
    def initm(self) -> float:
        """Get or set the Initial Mass Fraction of air component(s)
        """ # nopep8
        return self._cards[8].get_value("initm")

    @initm.setter
    def initm(self, value: float) -> None:
        """Set the initm property."""
        self._cards[8].set_value("initm", value)

    @property
    def aira(self) -> float:
        """Get or set the First Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("aira")

    @aira.setter
    def aira(self, value: float) -> None:
        """Set the aira property."""
        self._cards[8].set_value("aira", value)

    @property
    def airb(self) -> float:
        """Get or set the Second Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("airb")

    @airb.setter
    def airb(self, value: float) -> None:
        """Set the airb property."""
        self._cards[8].set_value("airb", value)

    @property
    def airc(self) -> float:
        """Get or set the Third Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[8].get_value("airc")

    @airc.setter
    def airc(self, value: float) -> None:
        """Set the airc property."""
        self._cards[8].set_value("airc", value)

    @property
    def lcmf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for mass flow rate
        """ # nopep8
        return self._cards[9].get_value("lcmf")

    @lcmf.setter
    def lcmf(self, value: int) -> None:
        """Set the lcmf property."""
        self._cards[9].set_value("lcmf", value)

    @property
    def mwgas(self) -> float:
        """Get or set the Molecular weight of inflator gas components
        """ # nopep8
        return self._cards[9].get_value("mwgas")

    @mwgas.setter
    def mwgas(self, value: float) -> None:
        """Set the mwgas property."""
        self._cards[9].set_value("mwgas", value)

    @property
    def gasa(self) -> float:
        """Get or set the First Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasa")

    @gasa.setter
    def gasa(self, value: float) -> None:
        """Set the gasa property."""
        self._cards[9].set_value("gasa", value)

    @property
    def gasb(self) -> float:
        """Get or set the Second Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasb")

    @gasb.setter
    def gasb(self, value: float) -> None:
        """Set the gasb property."""
        self._cards[9].set_value("gasb", value)

    @property
    def gasc(self) -> float:
        """Get or set the Third Coefficient of molar heat capacity at constant pressure
        """ # nopep8
        return self._cards[9].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        """Set the gasc property."""
        self._cards[9].set_value("gasc", value)

    @property
    def nodeid(self) -> int:
        """Get or set the Node ID defining the point source
        """ # nopep8
        return self._cards[10].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        """Set the nodeid property."""
        self._cards[10].set_value("nodeid", value)

    @property
    def vecid(self) -> int:
        """Get or set the Vector Id defining the direction of flow at the point source
        """ # nopep8
        return self._cards[10].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[10].set_value("vecid", value)

    @property
    def orifare(self) -> float:
        """Get or set the Orifice area at the point source
        """ # nopep8
        return self._cards[10].get_value("orifare")

    @orifare.setter
    def orifare(self, value: float) -> None:
        """Set the orifare property."""
        self._cards[10].set_value("orifare", value)

    @property
    def lcvel_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvel."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvel:
                return kwd
        return None

    @lcvel_link.setter
    def lcvel_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvel."""
        self.lcvel = value.lcid

    @property
    def lct_link(self) -> DefineCurve:
        """Get the DefineCurve object for lct."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lct:
                return kwd
        return None

    @lct_link.setter
    def lct_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lct."""
        self.lct = value.lcid

    @property
    def lcmf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcmf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcmf:
                return kwd
        return None

    @lcmf_link.setter
    def lcmf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcmf."""
        self.lcmf = value.lcid

