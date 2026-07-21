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

"""Module providing the ConstrainedInterpolationSpotweld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD1 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("nsid", int, 20, 10, None),
    FieldSchema("thick", float, 30, 10, None),
    FieldSchema("r", float, 40, 10, None),
    FieldSchema("stiff", float, 50, 10, None),
    FieldSchema("alpha1", float, 60, 10, None),
    FieldSchema("model", int, 70, 10, 1),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD2 = (
    FieldSchema("rn", float, 0, 10, None),
    FieldSchema("rs", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("lcf", int, 30, 10, None),
    FieldSchema("lcupf", int, 40, 10, None),
    FieldSchema("lcupr", int, 50, 10, None),
    FieldSchema("dens", float, 60, 10, None),
    FieldSchema("intp", int, 70, 10, 0),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD3 = (
    FieldSchema("upfn", float, 0, 10, None),
    FieldSchema("upfs", float, 10, 10, None),
    FieldSchema("alpha2", float, 20, 10, None),
    FieldSchema("beta2", float, 30, 10, None),
    FieldSchema("uprn", float, 40, 10, None),
    FieldSchema("uprs", float, 50, 10, None),
    FieldSchema("alpha3", float, 60, 10, None),
    FieldSchema("beta3", float, 70, 10, None),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD4 = (
    FieldSchema("mrn", float, 0, 10, None),
    FieldSchema("mrs", float, 10, 10, None),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD5 = (
    FieldSchema("stiff2", float, 0, 10, None),
    FieldSchema("stiff3", float, 10, 10, None),
    FieldSchema("stiff4", float, 20, 10, None),
    FieldSchema("lcdexp", int, 30, 10, 0),
    FieldSchema("gamma", float, 40, 10, 0.0),
    FieldSchema("sropt", int, 50, 10, 0),
    FieldSchema("pidvb", float, 60, 10, 0.0),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD6 = (
    FieldSchema("scarn", float, 0, 10, 1.0),
    FieldSchema("scars", float, 10, 10, 1.0),
    FieldSchema("damp", float, 20, 10, None),
    FieldSchema("sproff", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("failbm", float, 50, 10, 0.0),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD7 = (
    FieldSchema("ffn", float, 0, 10, None),
    FieldSchema("ffb", float, 10, 10, None),
    FieldSchema("ffs", float, 20, 10, None),
    FieldSchema("exfc", float, 30, 10, None),
    FieldSchema("stifp", float, 40, 10, 0.0),
    FieldSchema("mfsfc", float, 50, 10, 0.0),
    FieldSchema("defc", float, 60, 10, 0.0),
    FieldSchema("npfc", float, 70, 10, 0.0),
)

class ConstrainedInterpolationSpotweld(KeywordBase):
    """DYNA CONSTRAINED_INTERPOLATION_SPOTWELD keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "INTERPOLATION_SPOTWELD"
    _link_fields = {
        "lcf": LinkType.DEFINE_CURVE,
        "lcupf": LinkType.DEFINE_CURVE,
        "lcupr": LinkType.DEFINE_CURVE,
        "lcdexp": LinkType.DEFINE_CURVE,
        "nsid": LinkType.SET_NODE,
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedInterpolationSpotweld class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD7,
                **kwargs,
            ),
        ]
    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Name or description of the SPR3 defined in this keyword. This name will be used as part title for the visualization beams, such as in the d3plot database. If undefined, that part title is 'SPR3_NSID_...'.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of the first sheet
        GT.0: Part ID
        LT.0: | PID1 | is part set ID(for in - plane composed sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[1].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of the second sheet. PID2 can be identical to PID1 if the spot weld location nodes from NSID lie in between the shell elements that should be self-connected.
        GT.0: Part ID
        LT.0: |PID2 | is part set ID(for in - plane composed sheets sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[1].set_value("pid2", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of spot weld location nodes.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Total thickness of both sheets.
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[1].set_value("thick", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Spotweld radius.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[1].set_value("r", value)

    @property
    def stiff(self) -> typing.Optional[float]:
        """Get or set the Elastic stiffness. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        """Set the stiff property."""
        self._cards[1].set_value("stiff", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Scaling factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[1].set_value("alpha1", value)

    @property
    def model(self) -> int:
        """Get or set the Material behavior and damage model, see remarks.
        EQ. 1: SPR3 (default),
        EQ. 2: SPR4,
        EQ. 3: SPR3 with strongly objective formulation
        EQ.11: same as 1 with selected material parameters as functions,
        EQ.12: same as 2 with selected material parameters as functions,
        EQ.21: same as 11 with slight modification, see remarks,
        EQ.22: same as 12 with slight modification, see remarks.
        EQ.31: Same as 11 but with 12 more material parameters as functions
        EQ.41: Same as 31 with slight modification
        """ # nopep8
        return self._cards[1].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        """Set the model property."""
        if value not in [1, 2, 3, 11, 12, 21, 22, 31, 41, None]:
            raise Exception("""model must be `None` or one of {1,2,3,11,12,21,22,31,41}.""")
        self._cards[1].set_value("model", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Tensile strength factor.
        GT.0.0: Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remark 2).
        LT.0.0: Load curve with ID | RN | giving R_n as a function of peel ratio(see Remark 5)
        """ # nopep8
        return self._cards[2].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        """Set the rn property."""
        self._cards[2].set_value("rn", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the Shear strength factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[2].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        """Set the rs property."""
        self._cards[2].set_value("rs", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic potential _1. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

    @property
    def lcf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing force versus plastic displacement.
        """ # nopep8
        return self._cards[2].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        """Set the lcf property."""
        self._cards[2].set_value("lcf", value)

    @property
    def lcupf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[2].get_value("lcupf")

    @lcupf.setter
    def lcupf(self, value: int) -> None:
        """Set the lcupf property."""
        self._cards[2].set_value("lcupf", value)

    @property
    def lcupr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[2].get_value("lcupr")

    @lcupr.setter
    def lcupr(self, value: int) -> None:
        """Set the lcupr property."""
        self._cards[2].set_value("lcupr", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Spotweld density (necessary for time step calculation).
        """ # nopep8
        return self._cards[2].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        """Set the dens property."""
        self._cards[2].set_value("dens", value)

    @property
    def intp(self) -> int:
        """Get or set the Flag for interpolation. INTP is interpreted digit-wise, namely as,
        INTP=[LK]=K+10�L
        The first digit, K, controls the interpolation method:
        EQ.0: linear (default),
        EQ.1: uniform  (not recommended for asymmetrical arrangement of the upper and lower nodes),
        EQ.2: inverse distance weighting.
        EQ.3: Quadratic.
        The second digit, L, controls if the nodal area is considered when computing the weight:
        L.EQ.0: Nodal area is not considered (default),
        L.EQ.1: Nodal area is considered.
        """ # nopep8
        return self._cards[2].get_value("intp")

    @intp.setter
    def intp(self, value: int) -> None:
        """Set the intp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""intp must be `None` or one of {0,1,2,3}.""")
        self._cards[2].set_value("intp", value)

    @property
    def upfn(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in the normal direction.
        """ # nopep8
        return self._cards[3].get_value("upfn")

    @upfn.setter
    def upfn(self, value: float) -> None:
        """Set the upfn property."""
        self._cards[3].set_value("upfn", value)

    @property
    def upfs(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in the shear direction.
        """ # nopep8
        return self._cards[3].get_value("upfs")

    @upfs.setter
    def upfs(self, value: float) -> None:
        """Set the upfs property."""
        self._cards[3].set_value("upfs", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement scaling factor.
        """ # nopep8
        return self._cards[3].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[3].set_value("alpha2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic initiation displacement.
        """ # nopep8
        return self._cards[3].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[3].set_value("beta2", value)

    @property
    def uprn(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in the normal direction.
        """ # nopep8
        return self._cards[3].get_value("uprn")

    @uprn.setter
    def uprn(self, value: float) -> None:
        """Set the uprn property."""
        self._cards[3].set_value("uprn", value)

    @property
    def uprs(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in the shear direction.
        """ # nopep8
        return self._cards[3].get_value("uprs")

    @uprs.setter
    def uprs(self, value: float) -> None:
        """Set the uprs property."""
        self._cards[3].set_value("uprs", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement scaling factor.
        """ # nopep8
        return self._cards[3].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[3].set_value("alpha3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic rupture displacement.
        """ # nopep8
        return self._cards[3].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        """Set the beta3 property."""
        self._cards[3].set_value("beta3", value)

    @property
    def mrn(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RN.
        """ # nopep8
        return self._cards[4].get_value("mrn")

    @mrn.setter
    def mrn(self, value: float) -> None:
        """Set the mrn property."""
        self._cards[4].set_value("mrn", value)

    @property
    def mrs(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RS.
        """ # nopep8
        return self._cards[4].get_value("mrs")

    @mrs.setter
    def mrs(self, value: float) -> None:
        """Set the mrs property."""
        self._cards[4].set_value("mrs", value)

    @property
    def stiff2(self) -> typing.Optional[float]:
        """Get or set the Elastic shear stiffness.Function ID if MODEL > 30 (see Remark 2).
        GT.0: Constant value or function ID.
        LT.0: | STIFF2 | is load curve ID for the curve giving shear stiffness as a function of load angle(ranging from 0 for pure shear to t / 2 for pure tension).
        """ # nopep8
        return self._cards[5].get_value("stiff2")

    @stiff2.setter
    def stiff2(self, value: float) -> None:
        """Set the stiff2 property."""
        self._cards[5].set_value("stiff2", value)

    @property
    def stiff3(self) -> typing.Optional[float]:
        """Get or set the Elastic bending stiffness. . Function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("stiff3")

    @stiff3.setter
    def stiff3(self, value: float) -> None:
        """Set the stiff3 property."""
        self._cards[5].set_value("stiff3", value)

    @property
    def stiff4(self) -> typing.Optional[float]:
        """Get or set the Elastic torsional stiffness.. Function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("stiff4")

    @stiff4.setter
    def stiff4(self, value: float) -> None:
        """Set the stiff4 property."""
        self._cards[5].set_value("stiff4", value)

    @property
    def lcdexp(self) -> int:
        """Get or set the Load curve for damage exponent as a function of mode mixity.
        """ # nopep8
        return self._cards[5].get_value("lcdexp")

    @lcdexp.setter
    def lcdexp(self, value: int) -> None:
        """Set the lcdexp property."""
        self._cards[5].set_value("lcdexp", value)

    @property
    def gamma(self) -> float:
        """Get or set the Scaling factor, r_1. Function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[5].set_value("gamma", value)

    @property
    def sropt(self) -> int:
        """Get or set the Shear rotation option that defines local kinematics system:
        EQ.0: pure shear does not create a normal component (default).
        EQ.1: pure shear creates a normal component.
        """ # nopep8
        return self._cards[5].get_value("sropt")

    @sropt.setter
    def sropt(self, value: int) -> None:
        """Set the sropt property."""
        if value not in [0, 1, None]:
            raise Exception("""sropt must be `None` or one of {0,1}.""")
        self._cards[5].set_value("sropt", value)

    @property
    def pidvb(self) -> float:
        """Get or set the Part ID for beams used to represent SPR3 in post-processing.
        EQ.0: Part ID automatically set(default).
        GT.0: PIDVB defines the part ID.
        LT.0: | PIDVB | defines the part ID, but an alternative type of beam representation(deformation) is invoked.Also, beams are deleted after failure.
        """ # nopep8
        return self._cards[5].get_value("pidvb")

    @pidvb.setter
    def pidvb(self, value: float) -> None:
        """Set the pidvb property."""
        self._cards[5].set_value("pidvb", value)

    @property
    def scarn(self) -> float:
        """Get or set the Scale factor for tensile strength factor RN. This option also scales the displacements in LCF, LCUPF, and LCUPR so that the shape of the force-displacement curve stays similar.
        LT.0.0: |SCARN| refers to a load curve ID, giving the scale factor as a function of the state of the neighboring SPR3
        """ # nopep8
        return self._cards[6].get_value("scarn")

    @scarn.setter
    def scarn(self, value: float) -> None:
        """Set the scarn property."""
        self._cards[6].set_value("scarn", value)

    @property
    def scars(self) -> float:
        """Get or set the Scale factor for tensile strength factor RS. This option also scales the displacements in LCF, LCUPF, and LCUPR so that the shape of the force-displacement curve stays similar.
        LT.0.0: |SCARS| refers to a load curve ID, giving the scale factor as a function of the state of the neighboring SPR3.
        """ # nopep8
        return self._cards[6].get_value("scars")

    @scars.setter
    def scars(self, value: float) -> None:
        """Set the scars property."""
        self._cards[6].set_value("scars", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient to model stiffness-proportional damping effects.We recommend setting DAMP to a value between 0.05 and 0.15.
        """ # nopep8
        return self._cards[6].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[6].set_value("damp", value)

    @property
    def sproff(self) -> float:
        """Get or set the Specifies the relative location of the spot weld between PID1 and PID2. The value must be between -1.0 and 1.0.
        A value of -1.0 places the spot weld on PID1, while a value of 1.0 places it on PID2. By default (SPROFF = 0.0),
        the spot weld is located at the geometric center between PID1 and PID2.
        """ # nopep8
        return self._cards[6].get_value("sproff")

    @sproff.setter
    def sproff(self, value: float) -> None:
        """Set the sproff property."""
        self._cards[6].set_value("sproff", value)

    @property
    def failbm(self) -> float:
        """Get or set the Erosion of the base material if the spot weld fails flag:
        EQ.0.0:	If the spot weld fails, no erosion occurs in either sheet.
        EQ.1.0:	If the spot weld fails, erosion occurs in the first sheet�s nearest shell or solid element to the spot weld.
        EQ.2.0:	If the spot weld fails, erosion occurs in the second sheet�s nearest shell or solid element to the spot weld.
        EQ.3.0:	If the spot weld fails, erosion occurs in the nearest shell or solid element in each sheet.
        This flag is for depicting nugget pullout and potential crack initiation.
        """ # nopep8
        return self._cards[6].get_value("failbm")

    @failbm.setter
    def failbm(self, value: float) -> None:
        """Set the failbm property."""
        if value not in [0.0, 1.0, 2.0, 3.0, None]:
            raise Exception("""failbm must be `None` or one of {0.0,1.0,2.0,3.0}.""")
        self._cards[6].set_value("failbm", value)

    @property
    def ffn(self) -> typing.Optional[float]:
        """Get or set the Resultant normal force at failure. FFN is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("ffn")

    @ffn.setter
    def ffn(self, value: float) -> None:
        """Set the ffn property."""
        self._cards[7].set_value("ffn", value)

    @property
    def ffb(self) -> typing.Optional[float]:
        """Get or set the Resultant bending force at failure. FFB is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("ffb")

    @ffb.setter
    def ffb(self, value: float) -> None:
        """Set the ffb property."""
        self._cards[7].set_value("ffb", value)

    @property
    def ffs(self) -> typing.Optional[float]:
        """Get or set the Resultant shear force at failure. FFS is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("ffs")

    @ffs.setter
    def ffs(self, value: float) -> None:
        """Set the ffs property."""
        self._cards[7].set_value("ffs", value)

    @property
    def exfc(self) -> typing.Optional[float]:
        """Get or set the Failure function exponent. EXFC is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("exfc")

    @exfc.setter
    def exfc(self, value: float) -> None:
        """Set the exfc property."""
        self._cards[7].set_value("exfc", value)

    @property
    def stifp(self) -> float:
        """Get or set the Plastic stiffness. If greater than zero, this replaces LCF by a simple linear hardening law.
        """ # nopep8
        return self._cards[7].get_value("stifp")

    @stifp.setter
    def stifp(self, value: float) -> None:
        """Set the stifp property."""
        self._cards[7].set_value("stifp", value)

    @property
    def mfsfc(self) -> float:
        """Get or set the Scaling factor for torsion term in resultant shear force. MFSC is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("mfsfc")

    @mfsfc.setter
    def mfsfc(self, value: float) -> None:
        """Set the mfsfc property."""
        self._cards[7].set_value("mfsfc", value)

    @property
    def defc(self) -> float:
        """Get or set the Fading energy for damage. DEFC is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("defc")

    @defc.setter
    def defc(self, value: float) -> None:
        """Set the defc property."""
        self._cards[7].set_value("defc", value)

    @property
    def npfc(self) -> float:
        """Get or set the Plastic displacement offset for damage initiation. NPFC is a function ID if MODEL > 30 (see Remark 2).
        """ # nopep8
        return self._cards[7].get_value("npfc")

    @npfc.setter
    def npfc(self, value: float) -> None:
        """Set the npfc property."""
        self._cards[7].set_value("npfc", value)

    @property
    def lcf_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcf:
                return kwd
        return None

    @lcf_link.setter
    def lcf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcf."""
        self.lcf = value.lcid

    @property
    def lcupf_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcupf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcupf:
                return kwd
        return None

    @lcupf_link.setter
    def lcupf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcupf."""
        self.lcupf = value.lcid

    @property
    def lcupr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcupr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcupr:
                return kwd
        return None

    @lcupr_link.setter
    def lcupr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcupr."""
        self.lcupr = value.lcid

    @property
    def lcdexp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcdexp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdexp:
                return kwd
        return None

    @lcdexp_link.setter
    def lcdexp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdexp."""
        self.lcdexp = value.lcid

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def pid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

