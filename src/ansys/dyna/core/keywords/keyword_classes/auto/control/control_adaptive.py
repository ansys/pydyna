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

"""Module providing the ControlAdaptive class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLADAPTIVE_CARD0 = (
    FieldSchema("adpfreq", float, 0, 10, None),
    FieldSchema("adptol", float, 10, 10, 1e+20),
    FieldSchema("adptyp", int, 20, 10, 1),
    FieldSchema("maxlvl", int, 30, 10, 3),
    FieldSchema("tbirth", float, 40, 10, 0.0),
    FieldSchema("tdeath", float, 50, 10, 1e+20),
    FieldSchema("lcadp", int, 60, 10, 0),
    FieldSchema("ioflag", int, 70, 10, 0),
)

_CONTROLADAPTIVE_CARD1 = (
    FieldSchema("adpsize", float, 0, 10, 0.0),
    FieldSchema("adpass", int, 10, 10, 0),
    FieldSchema("ireflg", int, 20, 10, 0),
    FieldSchema("adpene", float, 30, 10, 0.0),
    FieldSchema("adpth", float, 40, 10, 0.0),
    FieldSchema("memory", int, 50, 10, 0),
    FieldSchema("orient", int, 60, 10, 0),
    FieldSchema("maxel", int, 70, 10, 0),
)

_CONTROLADAPTIVE_CARD2 = (
    FieldSchema("adpscl", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("adpene", float, 30, 10, 0.0),
    FieldSchema("adpdam", int, 40, 10, 0),
    FieldSchema("memory", int, 50, 10, 0),
    FieldSchema("adpmic", int, 60, 10, 0),
    FieldSchema("adptgen", float, 70, 10, 0.0),
)

_CONTROLADAPTIVE_CARD3 = (
    FieldSchema("dam1", float, 0, 10, 0.0),
)

_CONTROLADAPTIVE_CARD4 = (
    FieldSchema("iadpn90", int, 0, 10, 0),
    FieldSchema("iadpgh", int, 10, 10, 0),
    FieldSchema("ncfreq", int, 20, 10, None),
    FieldSchema("iadpcl", int, 30, 10, 1),
    FieldSchema("adpctl", float, 40, 10, None),
    FieldSchema("cbirth", float, 50, 10, 0.0),
    FieldSchema("cdeath", float, 60, 10, 1e+20),
    FieldSchema("lclvl", int, 70, 10, None),
)

_CONTROLADAPTIVE_CARD5 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("d3trace", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("ifsand", int, 70, 10, 0),
)

_CONTROLADAPTIVE_CARD6 = (
    FieldSchema("inmemr", int, 0, 10, 0),
)

class ControlAdaptive(KeywordBase):
    """DYNA CONTROL_ADAPTIVE keyword"""

    keyword = "CONTROL"
    subkeyword = "ADAPTIVE"
    _link_fields = {
        "lcadp": LinkType.DEFINE_CURVE,
        "lclvl": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlAdaptive class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLADAPTIVE_CARD6,
                **kwargs,
            ),
        ]
    @property
    def adpfreq(self) -> typing.Optional[float]:
        """Get or set the Time interval between adaptive refinements.
        """ # nopep8
        return self._cards[0].get_value("adpfreq")

    @adpfreq.setter
    def adpfreq(self, value: float) -> None:
        """Set the adpfreq property."""
        self._cards[0].set_value("adpfreq", value)

    @property
    def adptol(self) -> float:
        """Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
        """ # nopep8
        return self._cards[0].get_value("adptol")

    @adptol.setter
    def adptol(self, value: float) -> None:
        """Set the adptol property."""
        self._cards[0].set_value("adptol", value)

    @property
    def adptyp(self) -> int:
        """Get or set the Adaptive options:
        EQ.1: angle change in degrees per adaptive refinement relative to the surrounding elements for each element to be refined (default).
        EQ.2: total angle change in degrees relative to the surrounding element for each element to be refined.
        Adapts when the shell error in the energy norm, e, exceeds ADPTOL/100 times the mean energy norm within the part.
        EQ.7: 3D r-adaptive remeshing for solid elements.  Tetrahedrons are used in the adaptive remeshing process (solid formulation 10 or 13, or if EFG, formulation 42), or in the case of 3D axisymmetry (orbital) adaptivity, hexahedral and pentahedral elements are used in the adaptive remeshing.  A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.  The mesh size is currently based on the minimum and maximum edge lengths defined on the *CONTROL_REMESHING keyword input.  This option remains under development, and we are not sure of its reliability on complex geometries.
        EQ.8/-8: 2D r-adaptive remeshing for plane stress, plane strain, and axisymmetric continuum elements,that is, shell formulations 12 through 15.
        A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.
        The mesh size is currently based on the value, ADPTOL, which gives the characteristic element size.
        This option is based on earlier work by Dick and Harris[1992].
        If ADPTYP is negative, then self-contacting material will not be merged together.
        The self-merging is often preferred since it eliminates sharp folds in the boundary;
        however, if the sharp fold is being simulated, unexpected results are generated.
        """ # nopep8
        return self._cards[0].get_value("adptyp")

    @adptyp.setter
    def adptyp(self, value: int) -> None:
        """Set the adptyp property."""
        if value not in [1, 2, 4, 7, 8, -8, None]:
            raise Exception("""adptyp must be `None` or one of {1,2,4,7,8,-8}.""")
        self._cards[0].set_value("adptyp", value)

    @property
    def maxlvl(self) -> int:
        """Get or set the Maximum number of refinement levels (default = 3).
        """ # nopep8
        return self._cards[0].get_value("maxlvl")

    @maxlvl.setter
    def maxlvl(self, value: int) -> None:
        """Set the maxlvl property."""
        self._cards[0].set_value("maxlvl", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def lcadp(self) -> int:
        """Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
        EQ.0: ADPFREQ is used.
        """ # nopep8
        return self._cards[0].get_value("lcadp")

    @lcadp.setter
    def lcadp(self, value: int) -> None:
        """Set the lcadp property."""
        self._cards[0].set_value("lcadp", value)

    @property
    def ioflag(self) -> int:
        """Get or set the Flag to generate adaptive mesh at exit including *NODE, *ELEMENT, *SHELL, *BOUNDARY_, *CONTACT_NODE_, and *CONSTRAINED_ ADAPTIVITY to be saved in the file, adapt.msh.
        EQ.0: no adaptive mesh generation at the exit,
        EQ.1: adaptive mesh generation at the exit.
        """ # nopep8
        return self._cards[0].get_value("ioflag")

    @ioflag.setter
    def ioflag(self, value: int) -> None:
        """Set the ioflag property."""
        if value not in [0, 1, None]:
            raise Exception("""ioflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ioflag", value)

    @property
    def adpsize(self) -> float:
        """Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("adpsize")

    @adpsize.setter
    def adpsize(self, value: float) -> None:
        """Set the adpsize property."""
        self._cards[1].set_value("adpsize", value)

    @property
    def adpass(self) -> int:
        """Get or set the One or two pass adaptivity flag:
        EQ.0: two pass adaptivity,
        EQ.1: one pass adaptivity.
        """ # nopep8
        return self._cards[1].get_value("adpass")

    @adpass.setter
    def adpass(self, value: int) -> None:
        """Set the adpass property."""
        if value not in [0, 1, None]:
            raise Exception("""adpass must be `None` or one of {0,1}.""")
        self._cards[1].set_value("adpass", value)

    @property
    def ireflg(self) -> int:
        """Get or set the If positive, the mesh is refined uniformly by IREFLG levels at time = TBIRTH. A value of 1, 2, 3,  creates 4, 16, 64,  shells, respectively, for each original shell.  MAXLVL must be greater than or equal to IREFLG for this to work.
        If negative, | IREFLG | is taken as a curve ID.The curve specifies the minimum element size as a function of time.If the ordinate values(minimum element size) are positive, those values will override other element size criteria.If the ordinate values are negative, the absolute value of the ordinate is the element size used for refinement.
        """ # nopep8
        return self._cards[1].get_value("ireflg")

    @ireflg.setter
    def ireflg(self, value: int) -> None:
        """Set the ireflg property."""
        self._cards[1].set_value("ireflg", value)

    @property
    def adpene(self) -> float:
        """Get or set the For shells, h-adapt the mesh when the FORMING contact surfaces approach or penetrate the tooling surface depending on whether the value of ADPENE is positive (approach) or negative (penetrates), respectively.  The tooling adaptive refinement is based on the curvature of the tooling.  If ADPENE is positive the refinement generally occurs before contact takes place; consequently, it is possible that the parameter ADPASS can be set to 1 in invoke the one pass adaptivity.
        """ # nopep8
        return self._cards[1].get_value("adpene")

    @adpene.setter
    def adpene(self, value: float) -> None:
        """Set the adpene property."""
        self._cards[1].set_value("adpene", value)

    @property
    def adpth(self) -> float:
        """Get or set the Thickness below which adaptive remeshing begins:
        EQ.0.0: This parameter is ignored.
        GT.0.0: Absolute shell thickness level below which adaptive remeshing should begin.
        LT.0.0: | ADPTH | is the element thickness reduction ratio.If the ratio of the element thickness to the original element thickness is less than 1.0 + ADPTHK, the element will be refined.
        This option works only if ADPTOL is nonzero.If thickness based adaptive remeshing is desired without angle changes, then set ADPTOL to a large angle for ADPTYP = 1 or 2
        ADPDAM: Type of damage accumulation in workpiece. See Remark 11.
        EQ.0: No damage accumulation
        EQ.1: Ratio of effective plastic strain to failure plastic strain
        EQ.2: Cockcroft - Latham damage
        """ # nopep8
        return self._cards[1].get_value("adpth")

    @adpth.setter
    def adpth(self, value: float) -> None:
        """Set the adpth property."""
        self._cards[1].set_value("adpth", value)

    @property
    def memory(self) -> int:
        """Get or set the This flag can have two meanings depending on whether the memory environmental variable is or is not set.  The command setenv LSTC_MEMORY auto (or for bourne shell export LSTC_MEMORY=auto) sets the memory environmental variable which causes LS-DYNA to expand memory automatically.  Note that automatic memory expansion is not always 100% reliable depending on the machine and operating system level; consequently, it is not yet the default.  To see if this is set on a particular machine type the command env.  If the environmental variable is not set then when memory usage reaches this percentage, MEMORY, further adaptivity is prevented to avoid exceeding the memory specified at execution time.  Caution is necessary since memory usage is checked after each adaptive step, and, if the memory usage increases by more than the residual percentage, 100-PERCENT, the calculation will terminate.
        If the memory environmental variable is set then when the number of words of memory allocated reaches or exceeds this value, MEMORY, further adaptivity is stopped.
        """ # nopep8
        return self._cards[1].get_value("memory")

    @memory.setter
    def memory(self, value: int) -> None:
        """Set the memory property."""
        self._cards[1].set_value("memory", value)

    @property
    def orient(self) -> int:
        """Get or set the This option applies to the FORMING contact option only.  If this flag is set to one (1), the user orientation for the contact interface is used.  If this flag is set to zero (0), LS-DYNA sets the global orientation of the contact surface the first time a potential contact is observed after the birth time.   If tracked nodes are found on both sides of the contact surface, the orientation is set based on the principle of majority rules. Experience has shown that this principle is not always reliable.
        """ # nopep8
        return self._cards[1].get_value("orient")

    @orient.setter
    def orient(self, value: int) -> None:
        """Set the orient property."""
        self._cards[1].set_value("orient", value)

    @property
    def maxel(self) -> int:
        """Get or set the If this number of shells is exceeded, adaptivity is stopped
        """ # nopep8
        return self._cards[1].get_value("maxel")

    @maxel.setter
    def maxel(self, value: int) -> None:
        """Set the maxel property."""
        self._cards[1].set_value("maxel", value)

    @property
    def adpscl(self) -> int:
        """Get or set the Strain-rate scale factor. See Remark 12.
        EQ.0.0: No strain - rate scaling
        GT.0.0: Stain rate scale factor
        """ # nopep8
        return self._cards[2].get_value("adpscl")

    @adpscl.setter
    def adpscl(self, value: int) -> None:
        """Set the adpscl property."""
        self._cards[2].set_value("adpscl", value)

    @property
    def adpene(self) -> float:
        """Get or set the For shells, h-adapt the mesh when the FORMING contact surfaces approach or penetrate the tooling surface depending on whether the value of ADPENE is positive (approach) or negative (penetrates), respectively.  The tooling adaptive refinement is based on the curvature of the tooling.  If ADPENE is positive the refinement generally occurs before contact takes place; consequently, it is possible that the parameter ADPASS can be set to 1 in invoke the one pass adaptivity.
        """ # nopep8
        return self._cards[2].get_value("adpene")

    @adpene.setter
    def adpene(self, value: float) -> None:
        """Set the adpene property."""
        self._cards[2].set_value("adpene", value)

    @property
    def adpdam(self) -> int:
        """Get or set the Type of damage accumulation in the workpiece. See Remark 11.
        EQ.0: No damage accumulation
        EQ.1: Ratio of effective plastic strain to failure plastic strain
        EQ.2: Cockcroft - Latham damage
        """ # nopep8
        return self._cards[2].get_value("adpdam")

    @adpdam.setter
    def adpdam(self, value: int) -> None:
        """Set the adpdam property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""adpdam must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("adpdam", value)

    @property
    def memory(self) -> int:
        """Get or set the This flag can have two meanings depending on whether the memory environmental variable is or is not set.  The command setenv LSTC_MEMORY auto (or for bourne shell export LSTC_MEMORY=auto) sets the memory environmental variable which causes LS-DYNA to expand memory automatically.  Note that automatic memory expansion is not always 100% reliable depending on the machine and operating system level; consequently, it is not yet the default.  To see if this is set on a particular machine type the command env.  If the environmental variable is not set then when memory usage reaches this percentage, MEMORY, further adaptivity is prevented to avoid exceeding the memory specified at execution time.  Caution is necessary since memory usage is checked after each adaptive step, and, if the memory usage increases by more than the residual percentage, 100-PERCENT, the calculation will terminate.
        If the memory environmental variable is set then when the number of words of memory allocated reaches or exceeds this value, MEMORY, further adaptivity is stopped.
        """ # nopep8
        return self._cards[2].get_value("memory")

    @memory.setter
    def memory(self, value: int) -> None:
        """Set the memory property."""
        self._cards[2].set_value("memory", value)

    @property
    def adpmic(self) -> int:
        """Get or set the Option to compute microstructure evolution in hot forging simulations
        EQ.0 : No microstructure evolution(default)
        EQ.1 : Activate microstructure evolution.Use *MAT_ADD_MICRO to input material constants
        """ # nopep8
        return self._cards[2].get_value("adpmic")

    @adpmic.setter
    def adpmic(self, value: int) -> None:
        """Set the adpmic property."""
        if value not in [0, 1, None]:
            raise Exception("""adpmic must be `None` or one of {0,1}.""")
        self._cards[2].set_value("adpmic", value)

    @property
    def adptgen(self) -> float:
        """Get or set the Option for selecting the tetrahedral volume remesher:
        EQ.- 1.0 : Do not use the alternative remesher.
        EQ.0.0 : Use the alternative remesher when the classic remesher fails(default).
        EQ.1.0 : Use the alternative remesher with default meshing parameter 1.06.
        GT.1.0 : Use the alternative remesher with meshing parameter ADPTGEN.
        """ # nopep8
        return self._cards[2].get_value("adptgen")

    @adptgen.setter
    def adptgen(self, value: float) -> None:
        """Set the adptgen property."""
        self._cards[2].set_value("adptgen", value)

    @property
    def dam1(self) -> float:
        """Get or set the Critical Cockcroft-Latham damage value (include if ADPDAM = 2)
        """ # nopep8
        return self._cards[3].get_value("dam1")

    @dam1.setter
    def dam1(self, value: float) -> None:
        """Set the dam1 property."""
        self._cards[3].set_value("dam1", value)

    @property
    def iadpn90(self) -> int:
        """Get or set the Fission control flag around radii:
        GT.0: Maximum number of shells after fission covering the entire radius from starting tangent to ending tangent
        EQ. - 1: This setting works with look - forward adaptivity, making more consistent mesh adaptivity along the radius from starting tangent to ending tangent.The actual number of elements covering the radius, will be controlled by ADPSIZEand MAXLVL.Note this setting also works to prevent the kinks that are likely to happen along the draw wall in the deep drawing scenario, under which the parameter ADPFREQ needs to be set fine enough for fission as the blank draws into the die radius.Also see Remark 5..
        """ # nopep8
        return self._cards[4].get_value("iadpn90")

    @iadpn90.setter
    def iadpn90(self, value: int) -> None:
        """Set the iadpn90 property."""
        self._cards[4].set_value("iadpn90", value)

    @property
    def iadpgh(self) -> int:
        """Get or set the Fission flag for neighbor splitting
        EQ:0 split all neighbor elements
        EQ:1 do not split neighbor elements
        """ # nopep8
        return self._cards[4].get_value("iadpgh")

    @iadpgh.setter
    def iadpgh(self, value: int) -> None:
        """Set the iadpgh property."""
        self._cards[4].set_value("iadpgh", value)

    @property
    def ncfreq(self) -> typing.Optional[int]:
        """Get or set the Frequency of fission to fusion steps.  For example, if NCFREQ=4, then fusion will occur on the fourth, eighth, twelfth,  etc., fission steps, respectively.  If this option is used NCFREQ>1 is recommended
        """ # nopep8
        return self._cards[4].get_value("ncfreq")

    @ncfreq.setter
    def ncfreq(self, value: int) -> None:
        """Set the ncfreq property."""
        self._cards[4].set_value("ncfreq", value)

    @property
    def iadpcl(self) -> int:
        """Get or set the Fusion will not occur until the fission level reaches IADPCL.  Therefore, if IADPCL=2, MAXLVL=5,  any  element can be split into 256 elements.  If the surface flattens out, the number of elements will be reduced if the fusion option is active, i.e.,  the 256 elements can be fused and reduced to 16
        """ # nopep8
        return self._cards[4].get_value("iadpcl")

    @iadpcl.setter
    def iadpcl(self, value: int) -> None:
        """Set the iadpcl property."""
        self._cards[4].set_value("iadpcl", value)

    @property
    def adpctl(self) -> typing.Optional[float]:
        """Get or set the Adaptivity error tolerance in degrees for activating fusion.  It follows the same rules as ADPOPT above
        """ # nopep8
        return self._cards[4].get_value("adpctl")

    @adpctl.setter
    def adpctl(self, value: float) -> None:
        """Set the adpctl property."""
        self._cards[4].set_value("adpctl", value)

    @property
    def cbirth(self) -> float:
        """Get or set the Birth time for adaptive fusion.  If ADPENE>0, look-ahead adaptivity is active.  In this case, fission, based on local tool curvature, will occur while the blank is still relatively flat.  The time value given for CBIRTH should be set to a time later in the simulation after the forming process is well underway.
        """ # nopep8
        return self._cards[4].get_value("cbirth")

    @cbirth.setter
    def cbirth(self, value: float) -> None:
        """Set the cbirth property."""
        self._cards[4].set_value("cbirth", value)

    @property
    def cdeath(self) -> float:
        """Get or set the Death time for adaptive fusion
        """ # nopep8
        return self._cards[4].get_value("cdeath")

    @cdeath.setter
    def cdeath(self, value: float) -> None:
        """Set the cdeath property."""
        self._cards[4].set_value("cdeath", value)

    @property
    def lclvl(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of a curve that defines the maximum refinement level as a function of time
        """ # nopep8
        return self._cards[4].get_value("lclvl")

    @lclvl.setter
    def lclvl(self, value: int) -> None:
        """Set the lclvl property."""
        self._cards[4].set_value("lclvl", value)

    @property
    def d3trace(self) -> int:
        """Get or set the Output flag:
        EQ.0: No additional output states
        EQ.1: A d3plot state will be output just before and after an adaptive step even though it may not be requested. You may want this output so that the LS-PrePost particle trace algorithm will work in the case of adaptivity
        """ # nopep8
        return self._cards[5].get_value("d3trace")

    @d3trace.setter
    def d3trace(self, value: int) -> None:
        """Set the d3trace property."""
        self._cards[5].set_value("d3trace", value)

    @property
    def ifsand(self) -> int:
        """Get or set the Set this flag to 1 for sandwiched sheet forming
        """ # nopep8
        return self._cards[5].get_value("ifsand")

    @ifsand.setter
    def ifsand(self, value: int) -> None:
        """Set the ifsand property."""
        self._cards[5].set_value("ifsand", value)

    @property
    def inmemr(self) -> int:
        """Get or set the Flag to determine the way shell h-adaptivity is performed (see Remark 8):
        EQ.0: Traditional out - of - core adaptivity(default).
        EQ.1: In - core adaptivity(under development).This approach is only supported in MPP and only for ADPTYP = 2.  It does not apply to composite sandwich h - adaptivity.
        """ # nopep8
        return self._cards[6].get_value("inmemr")

    @inmemr.setter
    def inmemr(self, value: int) -> None:
        """Set the inmemr property."""
        if value not in [0, 1, None]:
            raise Exception("""inmemr must be `None` or one of {0,1}.""")
        self._cards[6].set_value("inmemr", value)

    @property
    def lcadp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcadp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcadp:
                return kwd
        return None

    @lcadp_link.setter
    def lcadp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcadp."""
        self.lcadp = value.lcid

    @property
    def lclvl_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lclvl."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lclvl:
                return kwd
        return None

    @lclvl_link.setter
    def lclvl_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lclvl."""
        self.lclvl = value.lcid

