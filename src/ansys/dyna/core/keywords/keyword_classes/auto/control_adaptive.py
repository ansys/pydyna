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

class ControlAdaptive(KeywordBase):
    """DYNA CONTROL_ADAPTIVE keyword"""

    keyword = "CONTROL"
    subkeyword = "ADAPTIVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "adpfreq",
                        float,
                        0,
                        10,
                        kwargs.get("adpfreq")
                    ),
                    Field(
                        "adptol",
                        float,
                        10,
                        10,
                        kwargs.get("adptol", 1.0E+20)
                    ),
                    Field(
                        "adptyp",
                        int,
                        20,
                        10,
                        kwargs.get("adptyp", 1)
                    ),
                    Field(
                        "maxlvl",
                        int,
                        30,
                        10,
                        kwargs.get("maxlvl", 3)
                    ),
                    Field(
                        "tbirth",
                        float,
                        40,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "tdeath",
                        float,
                        50,
                        10,
                        kwargs.get("tdeath", 1.0E+20)
                    ),
                    Field(
                        "lcadp",
                        int,
                        60,
                        10,
                        kwargs.get("lcadp", 0)
                    ),
                    Field(
                        "ioflag",
                        int,
                        70,
                        10,
                        kwargs.get("ioflag", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "adpsize",
                        float,
                        0,
                        10,
                        kwargs.get("adpsize", 0.0)
                    ),
                    Field(
                        "adpass",
                        int,
                        10,
                        10,
                        kwargs.get("adpass", 0)
                    ),
                    Field(
                        "ireflg",
                        int,
                        20,
                        10,
                        kwargs.get("ireflg", 0)
                    ),
                    Field(
                        "adpene",
                        float,
                        30,
                        10,
                        kwargs.get("adpene", 0.0)
                    ),
                    Field(
                        "adpth",
                        float,
                        40,
                        10,
                        kwargs.get("adpth", 0.0)
                    ),
                    Field(
                        "memory",
                        int,
                        50,
                        10,
                        kwargs.get("memory", 0)
                    ),
                    Field(
                        "orient",
                        int,
                        60,
                        10,
                        kwargs.get("orient", 0)
                    ),
                    Field(
                        "maxel",
                        int,
                        70,
                        10,
                        kwargs.get("maxel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ladpn90",
                        int,
                        0,
                        10,
                        kwargs.get("ladpn90", 0)
                    ),
                    Field(
                        "ladpgh",
                        int,
                        10,
                        10,
                        kwargs.get("ladpgh", 0)
                    ),
                    Field(
                        "ncfred",
                        int,
                        20,
                        10,
                        kwargs.get("ncfred")
                    ),
                    Field(
                        "ladpcl",
                        int,
                        30,
                        10,
                        kwargs.get("ladpcl", 1)
                    ),
                    Field(
                        "adpctl",
                        float,
                        40,
                        10,
                        kwargs.get("adpctl")
                    ),
                    Field(
                        "cbirth",
                        float,
                        50,
                        10,
                        kwargs.get("cbirth", 0.0)
                    ),
                    Field(
                        "cdeath",
                        float,
                        60,
                        10,
                        kwargs.get("cdeath", 1.0E+20)
                    ),
                    Field(
                        "lclvl",
                        int,
                        70,
                        10,
                        kwargs.get("lclvl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cnla",
                        float,
                        0,
                        10,
                        kwargs.get("cnla", 110.0)
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
                        "mmm2d",
                        int,
                        30,
                        10,
                        kwargs.get("mmm2d", 0)
                    ),
                    Field(
                        "adperr",
                        str,
                        40,
                        10,
                        kwargs.get("adperr", "0")
                    ),
                    Field(
                        "d3trace",
                        int,
                        50,
                        10,
                        kwargs.get("d3trace", 0)
                    ),
                    Field(
                        "iadpcf",
                        int,
                        60,
                        10,
                        kwargs.get("iadpcf", 0)
                    ),
                    Field(
                        "ifsand",
                        int,
                        70,
                        10,
                        kwargs.get("ifsand", 0)
                    ),
                ],
            ),
        ]

    @property
    def adpfreq(self) -> typing.Optional[float]:
        """Get or set the Time interval between adaptive refinements.
        """ # nopep8
        return self._cards[0].get_value("adpfreq")

    @adpfreq.setter
    def adpfreq(self, value: float) -> None:
        self._cards[0].set_value("adpfreq", value)

    @property
    def adptol(self) -> float:
        """Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
        """ # nopep8
        return self._cards[0].get_value("adptol")

    @adptol.setter
    def adptol(self, value: float) -> None:
        self._cards[0].set_value("adptol", value)

    @property
    def adptyp(self) -> int:
        """Get or set the Adaptive options:
        EQ.1: angle change in degrees per adaptive refinement relative to the surrounding elements for each element to be refined (default).
        EQ.2: total angle change in degrees relative to the surrounding element for each element to be refined.
        Adapts when the shell error in the energy norm, Δe, exceeds ADPTOL/100 times the mean energy norm within the part.
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
        if value not in [1, 2, 4, 7, 8, -8]:
            raise Exception("""adptyp must be one of {1,2,4,7,8,-8}""")
        self._cards[0].set_value("adptyp", value)

    @property
    def maxlvl(self) -> int:
        """Get or set the Maximum number of refinement levels (default = 3).
        """ # nopep8
        return self._cards[0].get_value("maxlvl")

    @maxlvl.setter
    def maxlvl(self, value: int) -> None:
        self._cards[0].set_value("maxlvl", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def lcadp(self) -> int:
        """Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
        EQ.0: ADPFREQ is used.
        """ # nopep8
        return self._cards[0].get_value("lcadp")

    @lcadp.setter
    def lcadp(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""ioflag must be one of {0,1}""")
        self._cards[0].set_value("ioflag", value)

    @property
    def adpsize(self) -> float:
        """Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("adpsize")

    @adpsize.setter
    def adpsize(self, value: float) -> None:
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
        if value not in [0, 1]:
            raise Exception("""adpass must be one of {0,1}""")
        self._cards[1].set_value("adpass", value)

    @property
    def ireflg(self) -> int:
        """Get or set the Uniform refinement level. A values of 1, 2, 3, ... allow 4, 16, 64, ....  elements, respectively, to be created uniformly for each original element.
        """ # nopep8
        return self._cards[1].get_value("ireflg")

    @ireflg.setter
    def ireflg(self, value: int) -> None:
        self._cards[1].set_value("ireflg", value)

    @property
    def adpene(self) -> float:
        """Get or set the Adapt the mesh when the contact surfaces approach or penetrate the tooling surface.
        """ # nopep8
        return self._cards[1].get_value("adpene")

    @adpene.setter
    def adpene(self, value: float) -> None:
        self._cards[1].set_value("adpene", value)

    @property
    def adpth(self) -> float:
        """Get or set the Absolute shell thickness level below which adaptive remeshing should begin.
        EQ.0: ADPTH is ignored (default).
        This option works only if ADPTOL is nonzero.
        """ # nopep8
        return self._cards[1].get_value("adpth")

    @adpth.setter
    def adpth(self, value: float) -> None:
        self._cards[1].set_value("adpth", value)

    @property
    def memory(self) -> int:
        """Get or set the See keyword manual.
        EQ.0: MEMORY is ignored (default).
        """ # nopep8
        return self._cards[1].get_value("memory")

    @memory.setter
    def memory(self, value: int) -> None:
        self._cards[1].set_value("memory", value)

    @property
    def orient(self) -> int:
        """Get or set the This option applies to the FORMING contact option only.
        EQ.0: LS-DYNA sets the global orientation of the contact surface the first time a potential contact is observed after the birth time,
        EQ.1: the user orientation for the contact interface is used.
        """ # nopep8
        return self._cards[1].get_value("orient")

    @orient.setter
    def orient(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""orient must be one of {0,1}""")
        self._cards[1].set_value("orient", value)

    @property
    def maxel(self) -> int:
        """Get or set the Adaptivity is stopped if this number of elements is exceeded
        EQ.0: MAXEL is ignored (default).
        """ # nopep8
        return self._cards[1].get_value("maxel")

    @maxel.setter
    def maxel(self, value: int) -> None:
        self._cards[1].set_value("maxel", value)

    @property
    def ladpn90(self) -> int:
        """Get or set the Maximum number of elements covering 90 degree of radii.
        """ # nopep8
        return self._cards[2].get_value("ladpn90")

    @ladpn90.setter
    def ladpn90(self, value: int) -> None:
        self._cards[2].set_value("ladpn90", value)

    @property
    def ladpgh(self) -> int:
        """Get or set the Fission flag for neighbor splitting
        EQ:0 split all neighbor elements
        EQ:1 do not split neighbor elements
        """ # nopep8
        return self._cards[2].get_value("ladpgh")

    @ladpgh.setter
    def ladpgh(self, value: int) -> None:
        self._cards[2].set_value("ladpgh", value)

    @property
    def ncfred(self) -> typing.Optional[int]:
        """Get or set the Frequency of fission to fusion steps.  For example, if NCFREQ=4, then fusion will occur on the fourth, eighth, twelfth,  etc., fission steps, respectively.  If this option is used NCFREQ>1 is recommended
        """ # nopep8
        return self._cards[2].get_value("ncfred")

    @ncfred.setter
    def ncfred(self, value: int) -> None:
        self._cards[2].set_value("ncfred", value)

    @property
    def ladpcl(self) -> int:
        """Get or set the Fusion will not occur until the fission level reaches IADPCL.  Therefore, if IADPCL=2, MAXLVL=5,  any  element can be split into 256 elements.  If the surface flattens out, the number of elements will be reduced if the fusion option is active, i.e.,  the 256 elements can be fused and reduced to 16
        """ # nopep8
        return self._cards[2].get_value("ladpcl")

    @ladpcl.setter
    def ladpcl(self, value: int) -> None:
        self._cards[2].set_value("ladpcl", value)

    @property
    def adpctl(self) -> typing.Optional[float]:
        """Get or set the Adaptivity error tolerance in degrees for activating fusion.  It follows the same rules as ADPOPT above
        """ # nopep8
        return self._cards[2].get_value("adpctl")

    @adpctl.setter
    def adpctl(self, value: float) -> None:
        self._cards[2].set_value("adpctl", value)

    @property
    def cbirth(self) -> float:
        """Get or set the Birth time for adaptive fusion.  If ADPENE>0, look-ahead adaptivity is active.  In this case, fission, based on local tool curvature, will occur while the blank is still relatively flat.  The time value given for CBIRTH should be set to a time later in the simulation after the forming process is well underway.
        """ # nopep8
        return self._cards[2].get_value("cbirth")

    @cbirth.setter
    def cbirth(self, value: float) -> None:
        self._cards[2].set_value("cbirth", value)

    @property
    def cdeath(self) -> float:
        """Get or set the Death time for adaptive fusion
        """ # nopep8
        return self._cards[2].get_value("cdeath")

    @cdeath.setter
    def cdeath(self, value: float) -> None:
        self._cards[2].set_value("cdeath", value)

    @property
    def lclvl(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of a curve that defines the maximum refinement level as a function of time
        """ # nopep8
        return self._cards[2].get_value("lclvl")

    @lclvl.setter
    def lclvl(self, value: int) -> None:
        self._cards[2].set_value("lclvl", value)

    @property
    def cnla(self) -> float:
        """Get or set the Limit angle for corner nodes
        GT.0.0:	Limit angle is CNLA and simplified boundary lines for straight sections are used as remeshing basis.
        LT.0.0:	Limit angle is |CNLA| and accurate boundary lines are used as remeshing basis (recommended).
        """ # nopep8
        return self._cards[3].get_value("cnla")

    @cnla.setter
    def cnla(self, value: float) -> None:
        self._cards[3].set_value("cnla", value)

    @property
    def mmm2d(self) -> int:
        """Get or set the If non-zero, common boundaries of all adapted materials will be merged. Only for 2D r-adaptivity
        """ # nopep8
        return self._cards[3].get_value("mmm2d")

    @mmm2d.setter
    def mmm2d(self, value: int) -> None:
        self._cards[3].set_value("mmm2d", value)

    @property
    def adperr(self) -> str:
        """Get or set the 3-digit number, as "XYY", where "X" and "YY" define the options for the recovery techniques and the error estimators, respectively.
        For X:
        EQ.0: superconvergent patch recovery (SPR) (default);
        EQ.1: the least square fit of the stress to the nodes (Global L2);
        EQ.2: error density SPR;
        EQ.3: self-weighted SPR
        For YY:
        EQ.00: energy norm (default)
        EQ.01: Cauchy sigma_x
        EQ.02: sigma_y
        EQ.03: sigma_z
        EQ.04: tau_xy
        EQ.05: tau_yz
        EQ.06: tau_zx
        EQ.07: effective plastic strain, eps_ep
        EQ.08: pressure
        EQ.09: von Mises
        EQ.10: principal deviator stress s11
        EQ.11: S22
        EQ.12: S33
        EQ.13: Tresca
        EQ.14: principal stress sigma_11
        EQ.15: sigma_22
        EQ.16: sigma_33
        EQ.20: user subroutine "uadpval" to extract the numerical solutions for recovery, and "uadpnorm" to provide an error estimator
        """ # nopep8
        return self._cards[3].get_value("adperr")

    @adperr.setter
    def adperr(self, value: str) -> None:
        self._cards[3].set_value("adperr", value)

    @property
    def d3trace(self) -> int:
        """Get or set the Flag that is either 0 or 1. If set to 1 then a d3plot state will be output
        just before and after an adaptive step even though it may not be
        requested. The reason for wanting to do this is to allow the LS-PrePost particle trace algorithm to work in the case of adaptivity.
        """ # nopep8
        return self._cards[3].get_value("d3trace")

    @d3trace.setter
    def d3trace(self, value: int) -> None:
        self._cards[3].set_value("d3trace", value)

    @property
    def iadpcf(self) -> int:
        """Get or set the Flag to enable adaptive user control files:
        EQ.0:	No user control files
        EQ.1 : Perform run - time control on 3D adaptivity through control files
        See details of this option in Manual Volume IV : Multiscale Solvers
        """ # nopep8
        return self._cards[3].get_value("iadpcf")

    @iadpcf.setter
    def iadpcf(self, value: int) -> None:
        self._cards[3].set_value("iadpcf", value)

    @property
    def ifsand(self) -> int:
        """Get or set the Set this flag to “1” for sandwiched sheet forming
        """ # nopep8
        return self._cards[3].get_value("ifsand")

    @ifsand.setter
    def ifsand(self, value: int) -> None:
        self._cards[3].set_value("ifsand", value)

