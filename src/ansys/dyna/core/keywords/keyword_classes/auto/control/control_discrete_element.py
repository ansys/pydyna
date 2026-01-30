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

"""Module providing the ControlDiscreteElement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLDISCRETEELEMENT_CARD0 = (
    FieldSchema("ndamp", float, 0, 10, 0.0),
    FieldSchema("tdamp", float, 10, 10, 0.0),
    FieldSchema("frics", float, 20, 10, 0.0),
    FieldSchema("fricr", float, 30, 10, 0.0),
    FieldSchema("normk", float, 40, 10, 0.01),
    FieldSchema("sheark", float, 50, 10, 0.0),
    FieldSchema("cap", int, 60, 10, 0),
    FieldSchema("vtk", int, 70, 10, 0),
)

_CONTROLDISCRETEELEMENT_CARD1 = (
    FieldSchema("gamma", float, 0, 10, 0.0),
    FieldSchema("vol", float, 10, 10, 0.0),
    FieldSchema("ang", float, 20, 10, 0.0),
    FieldSchema("gap", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("ignore", int, 50, 10, 0),
    FieldSchema("nbuf", int, 60, 10, 6),
    FieldSchema("parallel", int, 70, 10, 0),
)

_CONTROLDISCRETEELEMENT_CARD2 = (
    FieldSchema("lnorm", int, 0, 10, 0),
    FieldSchema("lshear", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("fricd", float, 30, 10, 0.0),
    FieldSchema("dc", float, 40, 10, 0.0),
    FieldSchema("ncrb", int, 50, 10, 0),
    FieldSchema("bt", float, 60, 10, 0.0),
    FieldSchema("dt", float, 70, 10, 1e+20),
)

_CONTROLDISCRETEELEMENT_CARD3 = (
    FieldSchema("cp", float, 0, 10, 0.0),
    FieldSchema("tc", float, 10, 10, 0.0),
    FieldSchema("tfac", float, 20, 10, 0.0),
)

_CONTROLDISCRETEELEMENT_CARD4 = (
    FieldSchema("idesoft", int, 0, 10, 0),
    FieldSchema("sofscl", float, 10, 10, 0.1),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("iskip", int, 30, 10, 0),
    FieldSchema("maxnei", int, 40, 10, 20),
)

class ControlDiscreteElement(KeywordBase):
    """DYNA CONTROL_DISCRETE_ELEMENT keyword"""

    keyword = "CONTROL"
    subkeyword = "DISCRETE_ELEMENT"
    _link_fields = {
        "lnorm": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlDiscreteElement class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLDISCRETEELEMENT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDISCRETEELEMENT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDISCRETEELEMENT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDISCRETEELEMENT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDISCRETEELEMENT_CARD4,
                **kwargs,
            ),        ]
    @property
    def ndamp(self) -> float:
        """Get or set the Normal damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("ndamp")

    @ndamp.setter
    def ndamp(self, value: float) -> None:
        """Set the ndamp property."""
        self._cards[0].set_value("ndamp", value)

    @property
    def tdamp(self) -> float:
        """Get or set the Tangential damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("tdamp")

    @tdamp.setter
    def tdamp(self, value: float) -> None:
        """Set the tdamp property."""
        self._cards[0].set_value("tdamp", value)

    @property
    def frics(self) -> float:
        """Get or set the Friction coefficient
        EQ.0: 3 DOF
        NE.0: 6 DOF (consider rotational DOF)
        """ # nopep8
        return self._cards[0].get_value("frics")

    @frics.setter
    def frics(self, value: float) -> None:
        """Set the frics property."""
        self._cards[0].set_value("frics", value)

    @property
    def fricr(self) -> float:
        """Get or set the Rolling friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fricr")

    @fricr.setter
    def fricr(self, value: float) -> None:
        """Set the fricr property."""
        self._cards[0].set_value("fricr", value)

    @property
    def normk(self) -> float:
        """Get or set the Optional: user defined normal spring constant
        """ # nopep8
        return self._cards[0].get_value("normk")

    @normk.setter
    def normk(self, value: float) -> None:
        """Set the normk property."""
        self._cards[0].set_value("normk", value)

    @property
    def sheark(self) -> float:
        """Get or set the Optional: user defined shear spring constant
        """ # nopep8
        return self._cards[0].get_value("sheark")

    @sheark.setter
    def sheark(self, value: float) -> None:
        """Set the sheark property."""
        self._cards[0].set_value("sheark", value)

    @property
    def cap(self) -> int:
        """Get or set the EQ.0: dry particles
        NE.0: wet particles, consider capillary force and need additional
        input card
        """ # nopep8
        return self._cards[0].get_value("cap")

    @cap.setter
    def cap(self, value: int) -> None:
        """Set the cap property."""
        self._cards[0].set_value("cap", value)

    @property
    def vtk(self) -> int:
        """Get or set the Maximum number of subcycling cycles
        """ # nopep8
        return self._cards[0].get_value("vtk")

    @vtk.setter
    def vtk(self, value: int) -> None:
        """Set the vtk property."""
        self._cards[0].set_value("vtk", value)

    @property
    def gamma(self) -> float:
        """Get or set the Liquid surface tension
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[1].set_value("gamma", value)

    @property
    def vol(self) -> float:
        """Get or set the Volume fraction
        """ # nopep8
        return self._cards[1].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        """Set the vol property."""
        self._cards[1].set_value("vol", value)

    @property
    def ang(self) -> float:
        """Get or set the Contact angle
        """ # nopep8
        return self._cards[1].get_value("ang")

    @ang.setter
    def ang(self, value: float) -> None:
        """Set the ang property."""
        self._cards[1].set_value("ang", value)

    @property
    def gap(self) -> float:
        """Get or set the Optional parameter affecting the spatial limit of the liquid bridge.
        CAP.EQ.0:	GAP is ignored, if the CAP field is 0 and the simulation is modeling dry particles.
        CAP.NE.0 : A liquid bridge exists when δ, as illustrated in Figure 0 - 2, is less or equal to min⁡(GAP ,d_rup) where d_rup is the rupture distance of the bridge automatically calculated by LS - DYNA
        """ # nopep8
        return self._cards[1].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        """Set the gap property."""
        self._cards[1].set_value("gap", value)

    @property
    def ignore(self) -> int:
        """Get or set the Ignore initial penetration option
        EQ.0:	Calculate the contact force for DES with initial penetration
        GT.0 : Ignore the contact force calculation for DES with initial penetration
        """ # nopep8
        return self._cards[1].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        """Set the ignore property."""
        self._cards[1].set_value("ignore", value)

    @property
    def nbuf(self) -> int:
        """Get or set the GE.0:	Factor of memory use for asynchronous message buffer (Default = 6)
        LT.0:	Disable asynchronous scheme and use minimum memory for data transfer
        """ # nopep8
        return self._cards[1].get_value("nbuf")

    @nbuf.setter
    def nbuf(self, value: int) -> None:
        """Set the nbuf property."""
        self._cards[1].set_value("nbuf", value)

    @property
    def parallel(self) -> int:
        """Get or set the Flag for calculating contact force between bonded DES:
        EQ.0:	skip contact force calculation for bonded DES(Default)
        EQ.1 : consider contact force calculation for bonded DES
        """ # nopep8
        return self._cards[1].get_value("parallel")

    @parallel.setter
    def parallel(self, value: int) -> None:
        """Set the parallel property."""
        if value not in [0, 1, None]:
            raise Exception("""parallel must be `None` or one of {0,1}.""")
        self._cards[1].set_value("parallel", value)

    @property
    def lnorm(self) -> int:
        """Get or set the Load curve ID of a curve that defines function for normal stiffness with respect to norm penetration ratio
        """ # nopep8
        return self._cards[2].get_value("lnorm")

    @lnorm.setter
    def lnorm(self, value: int) -> None:
        """Set the lnorm property."""
        self._cards[2].set_value("lnorm", value)

    @property
    def lshear(self) -> int:
        """Get or set the Load curve ID of a curve that defines function for shear stiffness with respect to norm penetration ratio
        """ # nopep8
        return self._cards[2].get_value("lshear")

    @lshear.setter
    def lshear(self, value: int) -> None:
        """Set the lshear property."""
        self._cards[2].set_value("lshear", value)

    @property
    def fricd(self) -> float:
        """Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS
        """ # nopep8
        return self._cards[2].get_value("fricd")

    @fricd.setter
    def fricd(self, value: float) -> None:
        """Set the fricd property."""
        self._cards[2].set_value("fricd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[2].set_value("dc", value)

    @property
    def ncrb(self) -> int:
        """Get or set the Rebalancing frequency, that is, the number of cycles between each rebalancing.  This parameter only applies to MPP.
        EQ.0:	no rebalancing is performed
        """ # nopep8
        return self._cards[2].get_value("ncrb")

    @ncrb.setter
    def ncrb(self, value: int) -> None:
        """Set the ncrb property."""
        self._cards[2].set_value("ncrb", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[2].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[2].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time
        """ # nopep8
        return self._cards[2].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[2].set_value("dt", value)

    @property
    def cp(self) -> float:
        """Get or set the DES thermal properties
        """ # nopep8
        return self._cards[3].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[3].set_value("cp", value)

    @property
    def tc(self) -> float:
        """Get or set the DES thermal properties
        """ # nopep8
        return self._cards[3].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[3].set_value("tc", value)

    @property
    def tfac(self) -> float:
        """Get or set the DES thermal properties
        """ # nopep8
        return self._cards[3].get_value("tfac")

    @tfac.setter
    def tfac(self, value: float) -> None:
        """Set the tfac property."""
        self._cards[3].set_value("tfac", value)

    @property
    def idesoft(self) -> int:
        """Get or set the Flag for soft constraint formulation:
        EQ.1:	Soft constraint formulation.The contact stiffness is based on the nodal mass and the global time step size.
        This input provides a different way for calculating NORMK.NORMK is ignored if IDESOFT = 1. IDESOFT is ignored if LNORM ≠ 0.
        """ # nopep8
        return self._cards[4].get_value("idesoft")

    @idesoft.setter
    def idesoft(self, value: int) -> None:
        """Set the idesoft property."""
        self._cards[4].set_value("idesoft", value)

    @property
    def sofscl(self) -> float:
        """Get or set the Scale factor applied to the contact stiffness in the soft constrain formulation.
        """ # nopep8
        return self._cards[4].get_value("sofscl")

    @sofscl.setter
    def sofscl(self, value: float) -> None:
        """Set the sofscl property."""
        self._cards[4].set_value("sofscl", value)

    @property
    def iskip(self) -> int:
        """Get or set the Flag for skipping the calculation of contact force between DES:
        EQ.0:	Consider the particle - particle contact calculation(default).
        EQ.1 : Skip the particle - particle contact calculation.

        """ # nopep8
        return self._cards[4].get_value("iskip")

    @iskip.setter
    def iskip(self, value: int) -> None:
        """Set the iskip property."""
        self._cards[4].set_value("iskip", value)

    @property
    def maxnei(self) -> int:
        """Get or set the Number of neighbors to be tracked for DES contact and capillary force calculation (default = 20).
        If particle sizes are very different, MAXNEI needs to be increased to capture more neighbors
        """ # nopep8
        return self._cards[4].get_value("maxnei")

    @maxnei.setter
    def maxnei(self, value: int) -> None:
        """Set the maxnei property."""
        self._cards[4].set_value("maxnei", value)

    @property
    def lnorm_link(self) -> DefineCurve:
        """Get the DefineCurve object for lnorm."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lnorm:
                return kwd
        return None

    @lnorm_link.setter
    def lnorm_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lnorm."""
        self.lnorm = value.lcid

