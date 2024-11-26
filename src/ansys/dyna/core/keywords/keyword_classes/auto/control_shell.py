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

class ControlShell(KeywordBase):
    """DYNA CONTROL_SHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "SHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "wrpang",
                        float,
                        0,
                        10,
                        kwargs.get("wrpang", 20.0)
                    ),
                    Field(
                        "esort",
                        int,
                        10,
                        10,
                        kwargs.get("esort", 0)
                    ),
                    Field(
                        "irnxx",
                        int,
                        20,
                        10,
                        kwargs.get("irnxx", -1)
                    ),
                    Field(
                        "istupd",
                        int,
                        30,
                        10,
                        kwargs.get("istupd", 0)
                    ),
                    Field(
                        "theory",
                        int,
                        40,
                        10,
                        kwargs.get("theory", 2)
                    ),
                    Field(
                        "bwc",
                        int,
                        50,
                        10,
                        kwargs.get("bwc", 2)
                    ),
                    Field(
                        "miter",
                        int,
                        60,
                        10,
                        kwargs.get("miter", 1)
                    ),
                    Field(
                        "proj",
                        int,
                        70,
                        10,
                        kwargs.get("proj", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rotascl",
                        float,
                        0,
                        10,
                        kwargs.get("rotascl", 1.0)
                    ),
                    Field(
                        "intgrd",
                        int,
                        10,
                        10,
                        kwargs.get("intgrd", 0)
                    ),
                    Field(
                        "lamsht",
                        int,
                        20,
                        10,
                        kwargs.get("lamsht", 0)
                    ),
                    Field(
                        "cstyp6",
                        int,
                        30,
                        10,
                        kwargs.get("cstyp6", 1)
                    ),
                    Field(
                        "thshel",
                        int,
                        40,
                        10,
                        kwargs.get("thshel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "psstupd",
                        int,
                        0,
                        10,
                        kwargs.get("psstupd", 0)
                    ),
                    Field(
                        "sidt4tu",
                        int,
                        10,
                        10,
                        kwargs.get("sidt4tu", 0)
                    ),
                    Field(
                        "cntco",
                        int,
                        20,
                        10,
                        kwargs.get("cntco", 0)
                    ),
                    Field(
                        "itsflg",
                        int,
                        30,
                        10,
                        kwargs.get("itsflg", 0)
                    ),
                    Field(
                        "irquad",
                        int,
                        40,
                        10,
                        kwargs.get("irquad", 3)
                    ),
                    Field(
                        "w-mode",
                        float,
                        50,
                        10,
                        kwargs.get("w-mode")
                    ),
                    Field(
                        "stretch",
                        float,
                        60,
                        10,
                        kwargs.get("stretch")
                    ),
                    Field(
                        "icrq",
                        int,
                        70,
                        10,
                        kwargs.get("icrq", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nfail1",
                        int,
                        0,
                        10,
                        kwargs.get("nfail1")
                    ),
                    Field(
                        "nfail4",
                        int,
                        10,
                        10,
                        kwargs.get("nfail4")
                    ),
                    Field(
                        "psnfail",
                        int,
                        20,
                        10,
                        kwargs.get("psnfail", 0)
                    ),
                    Field(
                        "keepcs",
                        int,
                        30,
                        10,
                        kwargs.get("keepcs", 0)
                    ),
                    Field(
                        "delfr",
                        int,
                        40,
                        10,
                        kwargs.get("delfr", 0)
                    ),
                    Field(
                        "drcpsid",
                        int,
                        50,
                        10,
                        kwargs.get("drcpsid", 0)
                    ),
                    Field(
                        "drcprm",
                        float,
                        60,
                        10,
                        kwargs.get("drcprm", 1.0)
                    ),
                    Field(
                        "intperr",
                        int,
                        70,
                        10,
                        kwargs.get("intperr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "drcmth",
                        int,
                        0,
                        10,
                        kwargs.get("drcmth", 0)
                    ),
                    Field(
                        "lispsid",
                        int,
                        10,
                        10,
                        kwargs.get("lispsid", 0)
                    ),
                    Field(
                        "nlocdt",
                        int,
                        20,
                        10,
                        kwargs.get("nlocdt", 0)
                    ),
                ],
            ),
        ]

    @property
    def wrpang(self) -> float:
        """Get or set the Shell element warpage angle in degrees. If a warpage greater than this angle is found, a warning message is printed. (Default is 20 degrees).
        """ # nopep8
        return self._cards[0].get_value("wrpang")

    @wrpang.setter
    def wrpang(self, value: float) -> None:
        self._cards[0].set_value("wrpang", value)

    @property
    def esort(self) -> int:
        """Get or set the Automatic sorting of triangular shell elements to treat degenerate quadrilateral shell elements as C0 triangular shells, (see option THEORY inuser's manual):
        EQ.0: no sorting required (default).
        EQ.1: full sorting (C0 triangular shells),
        EQ.2: full sorting (DKT triangular shells)
        """ # nopep8
        return self._cards[0].get_value("esort")

    @esort.setter
    def esort(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""esort must be one of {0,1,2}""")
        self._cards[0].set_value("esort", value)

    @property
    def irnxx(self) -> int:
        """Get or set the Shell normal update option.  This option affects the Hughes-Liu, Belytschko-Wong-Chiang, and the Belytschko-Tsay shell formultions. The latter is affected if and only if the warping stiffness option is active, i.e. BWC=1. IRNXX must be set to 2 to invoke the top or bottom surface as the reference surface for the Hughes-Liu shell elements.
        EQ.-2: unique nodal fibers which are incrementally updated based on the nodal rotation at the location of the fiber,
        EQ.-1: recompute fiber directions each cycle,
        EQ.0: default set to -1,
        EQ.1: compute on restarts,
        EQ.n: compute every n cycles (Hughes-Liu shells only).
        """ # nopep8
        return self._cards[0].get_value("irnxx")

    @irnxx.setter
    def irnxx(self, value: int) -> None:
        self._cards[0].set_value("irnxx", value)

    @property
    def istupd(self) -> int:
        """Get or set the Shell thickness change option for deformable shells. The parameter, PSSTUPD, on the second optional card allows this option to be applied by part ID. For crash analysis, neglecting the elastic component of the strains, ISTUPD=4, may improve enery conservation and stability.
        EQ.0: no change.
        EQ.1: membrane straining causes thickness change (important for sheet metal forming or whenever membrane stretching is important).
        EQ.2: membrane straining causes thickness change in 8 node thick shell elements, types 1 and 2. This option is not recommended for implicit or explicit solutions which use the fully integrated type 2 element. The type 3 thick shell is a continuum based shell and thickness changes are always considered.
        EQ.3: options 1 and 2 apply.
        EQ.4: option 1 applies, but the elastic strains are neglected for the thickness update. This option only  applies to the most common elastic-plastic materials for which the elastic response is isotropic.
        """ # nopep8
        return self._cards[0].get_value("istupd")

    @istupd.setter
    def istupd(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""istupd must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("istupd", value)

    @property
    def theory(self) -> int:
        """Get or set the Default shell theory:
        EQ.1: Hughes-Liu,
        EQ.2: Belytschko-Tsay (default),
        EQ.3: BCIZ triangular shell (not recommended),
        EQ.4: Co triangular shell,
        EQ.5: Belytschko-Tsay membrane,
        EQ.6: S/R Hughes Liu,
        EQ.7: S/R co-rotational Hughes Liu,
        EQ.8: Belytschko-Leviathan shell,
        EQ.9: fully integrated Belytschko-Tsay membrane,
        EQ.10: Belytschko-Wong-Chiang,
        EQ.11: Fast (co-rotational) Hughes-Liu.
        EQ.12: Plane stress (x-y plane),
        EQ.13: Plane strain (x-y plane),
        EQ.14: Axisymmetric solid (y-axis of symmetry) - area weighted,
        EQ.15: Axisymmetric solid (y-axis of symmetry) - volume weighted
        EQ.16: Fully integrated shell element (very fast)
        EQ.17: Discrete Kirchhoff triangular shell (DKT)
        EQ.18: Discrete Kirchhoff linear shell either quadrilateral or triangular
        EQ.20: C0 linear shell element with drilling stiffness.
        For the 2D axisymmetric solid elements, high explosive applications work best with the area weighted approach and structural applications work best with the volume weighted approach. The volume weighted approach can lead to problems along the axis of symmetry under very large deformations.  Often the symmetry condition is not obeyed, and the elements will kink along the axis. The volume weigthed approach must be used if 2D shell elements are used in the mesh. Type 14 and 15 elements cannot be mixed in the same calculation.
        """ # nopep8
        return self._cards[0].get_value("theory")

    @theory.setter
    def theory(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20]:
            raise Exception("""theory must be one of {2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20}""")
        self._cards[0].set_value("theory", value)

    @property
    def bwc(self) -> int:
        """Get or set the Warping stiffness for Belytschko-Tsay shells:
        EQ.1: Belytschko-Wong-Chiang warping stiffness added.
        EQ.2: Belytschko-Tsay (default).
        """ # nopep8
        return self._cards[0].get_value("bwc")

    @bwc.setter
    def bwc(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""bwc must be one of {2,1}""")
        self._cards[0].set_value("bwc", value)

    @property
    def miter(self) -> int:
        """Get or set the Plane stress plasticity option (applies to materials 3, 18, 19, and 24):
        EQ.1: iterative plasticity with 3 secant iterations (default),
        EQ.2: full iterative plasticity,
        EQ.3: radial return noniterative plasticity. May lead to false results and has to be used with great care.
        """ # nopep8
        return self._cards[0].get_value("miter")

    @miter.setter
    def miter(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""miter must be one of {1,2,3}""")
        self._cards[0].set_value("miter", value)

    @property
    def proj(self) -> int:
        """Get or set the Projection method for warping stiffness in the Belytschko-Tsay shell and Belytschko-Wong-Chiang elements (See Remarks in user's manual).
        EQ.0: drill projection,
        EQ.1: full projection.
        """ # nopep8
        return self._cards[0].get_value("proj")

    @proj.setter
    def proj(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""proj must be one of {0,1}""")
        self._cards[0].set_value("proj", value)

    @property
    def rotascl(self) -> float:
        """Get or set the Define a scale factor for the rotary shell mass. This option is not for general use. The rotary inertia for shells is automatically scaled to permit a larger time step size. A scale factor other than the default, i.e., unity, is not recommended.
        """ # nopep8
        return self._cards[1].get_value("rotascl")

    @rotascl.setter
    def rotascl(self, value: float) -> None:
        self._cards[1].set_value("rotascl", value)

    @property
    def intgrd(self) -> int:
        """Get or set the Default shell through thickness numerical integration rule:
        EQ.0: Gauss integration. If 1-10 integration points are specified, the default rule is Gauss integration.
        EQ.1: Lobatto integration. If 3-10 integration points are specified, the default rule is Lobatto. For 2 point integration, the Lobatto rule is very inaccurate, so Gauss integration is used instead. Lobatto integration has an advantage in that the inner and outer integration points are on the shell surfaces.
        """ # nopep8
        return self._cards[1].get_value("intgrd")

    @intgrd.setter
    def intgrd(self, value: int) -> None:
        self._cards[1].set_value("intgrd", value)

    @property
    def lamsht(self) -> int:
        """Get or set the For composite shells with material types:
        *MAT_COMPOSITE_DAMAGE
        *MAT_ENHANCED_COMPOSITE_DAMAGE.
        If this flag is set laminated shell theory is used. Lamination theory is applied to correct for the assumption of a uniform constant shear strain through the thickness of the shell. Unless this correction is applied, the stiffness of the shell can be grossly incorrect if there are drastic differences in the elastic constants from ply to ply, especially for sandwich type shells. Generally, without this correction the results are too stiff. For the discrete Kirchhoff shell elements, which do not consider transverse shear, this option is ignored.
        EQ.0: do not update shear corrections,
        EQ.1: activate laminated shell theory.
        """ # nopep8
        return self._cards[1].get_value("lamsht")

    @lamsht.setter
    def lamsht(self, value: int) -> None:
        self._cards[1].set_value("lamsht", value)

    @property
    def cstyp6(self) -> int:
        """Get or set the Coordinate system for the type 6 shell element. The default system computes a unique local system at each inplane point. The uniform local system computes just one system used throughout the shell element. This involves fewer calculations and is therefore more efficient. The change of systems has a slight effect on results; therefore, the older method less efficient method is the default.
        EQ.1:  variable local coordinate system  (default),
        EQ.2:  uniform local system.
        """ # nopep8
        return self._cards[1].get_value("cstyp6")

    @cstyp6.setter
    def cstyp6(self, value: int) -> None:
        self._cards[1].set_value("cstyp6", value)

    @property
    def thshel(self) -> int:
        """Get or set the Thermal shell option.  Four node shells are treated internally as twelve node brick elements to allow heat conduction through the thickness of the shell.
        """ # nopep8
        return self._cards[1].get_value("thshel")

    @thshel.setter
    def thshel(self, value: int) -> None:
        self._cards[1].set_value("thshel", value)

    @property
    def psstupd(self) -> int:
        """Get or set the |PSSTUPD| is the optional shell part set ID specifying which part ID's have or do not have their thickness updated.  The shell thickness update by default applies to all shell elements in the mesh.  Generally, this part set ID is not needed.
        LT.0: these shell parts are excluded from the shell thickness update
        EQ.0: all deformable shells have their thickness updated
        GT.0: these shell parts are included in the shell thickness update
        """ # nopep8
        return self._cards[2].get_value("psstupd")

    @psstupd.setter
    def psstupd(self, value: int) -> None:
        self._cards[2].set_value("psstupd", value)

    @property
    def sidt4tu(self) -> int:
        """Get or set the Part set ID for parts which use the type 4 thickness update where elastic strains are ignored. This option is useful if different components of the final model are validated using different update options.
        """ # nopep8
        return self._cards[2].get_value("sidt4tu")

    @sidt4tu.setter
    def sidt4tu(self, value: int) -> None:
        self._cards[2].set_value("sidt4tu", value)

    @property
    def cntco(self) -> int:
        """Get or set the Flag to account for shell reference surface offsets in the contact treatment
        EQ.0: offsets are ignored
        EQ.1: offsets are treated using shell thickness
        EQ.2: offsets are treated using the user defined contact thickness which may be different than the shell thickness used in the element formulations
        """ # nopep8
        return self._cards[2].get_value("cntco")

    @cntco.setter
    def cntco(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""cntco must be one of {0,1,2}""")
        self._cards[2].set_value("cntco", value)

    @property
    def itsflg(self) -> int:
        """Get or set the Flag to activate/deactivate initial transverse shear stresses:
        EQ.0: keep transverse shear stresses
        EQ.1: set transverse shear stresses to zero.
        """ # nopep8
        return self._cards[2].get_value("itsflg")

    @itsflg.setter
    def itsflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itsflg must be one of {0,1}""")
        self._cards[2].set_value("itsflg", value)

    @property
    def irquad(self) -> int:
        """Get or set the In plane integration rule for the 8 node shell element:
        EQ.2: 2 x 2 Gauss quadrature,
        EQ.3: 3 x 3 Gauss quadrature..
        """ # nopep8
        return self._cards[2].get_value("irquad")

    @irquad.setter
    def irquad(self, value: int) -> None:
        if value not in [3, 2]:
            raise Exception("""irquad must be one of {3,2}""")
        self._cards[2].set_value("irquad", value)

    @property
    def w_mode(self) -> typing.Optional[float]:
        """Get or set the W-Mode amplitude for element deletion, specified in degrees
        """ # nopep8
        return self._cards[2].get_value("w-mode")

    @w_mode.setter
    def w_mode(self, value: float) -> None:
        self._cards[2].set_value("w-mode", value)

    @property
    def stretch(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio of element diagonals for element deletion. This option is activated if and only if either NFAIL1 or NFAIL4 are nonzero and STRETCH > 0.0
        """ # nopep8
        return self._cards[2].get_value("stretch")

    @stretch.setter
    def stretch(self, value: float) -> None:
        self._cards[2].set_value("stretch", value)

    @property
    def icrq(self) -> int:
        """Get or set the Continuous treatment across element edges for some specified result quantities.
        EQ.0: not active
        EQ.1: thickness and plastic strain
        """ # nopep8
        return self._cards[2].get_value("icrq")

    @icrq.setter
    def icrq(self, value: int) -> None:
        self._cards[2].set_value("icrq", value)

    @property
    def nfail1(self) -> typing.Optional[int]:
        """Get or set the Flag to check for highly distorted under-integrated shell elements, print a
        message, and delete the element or terminate. Generally, this flag is not
        needed for one point elements that do not use the warping stiffness. A
        distorted element is one where a negative Jacobian exist within the
        domain of the shell, not just at integration points. The checks are made
        away from the CPU requirements for one point elements. If nonzero,
        NFAIL1 can be changed in a restart.
        EQ.1: print message and delete element.
        EQ.2: print message, write D3DUMP file, and terminate
        GT.2: print message and delete element. When NFAIL1 elements
        are deleted then write D3DUMP file and terminate. These NFAIL1
        failed elements also include all shell elements that failed for other
        reasons than distortion. Before the D3DUMP file is written, NFAIL1
        is doubled, so the run can immediately be continued if desired.
        """ # nopep8
        return self._cards[3].get_value("nfail1")

    @nfail1.setter
    def nfail1(self, value: int) -> None:
        self._cards[3].set_value("nfail1", value)

    @property
    def nfail4(self) -> typing.Optional[int]:
        """Get or set the Flag to check for highly distorted fully-integrated shell elements, print a
        message and delete the element or terminate. Generally, this flag is
        recommended. A distorted element is one where a negative Jacobian
        exist within the domain of the shell, not just at integration points. The
        checks are made away from the integration points to enable the bad
        elements to be deleted before an instability leading to an error
        termination occurs. If nonzero, NFAIL1 can be changed in a restart.
        EQ.1: print message and delete element.
        EQ.2: print message, write D3DUMP file, and terminate
        GT.2: print message and delete element. When NFAIL4 elements
        are deleted then write D3DUMP file and terminate. These NFAIL4
        failed elements also include all shell elements that failed for other
        reasons than distortion. Before the D3DUMP file is written,
        NFAIL4 is doubled, so the run can immediately be continued if
        desired.
        """ # nopep8
        return self._cards[3].get_value("nfail4")

    @nfail4.setter
    def nfail4(self, value: int) -> None:
        self._cards[3].set_value("nfail4", value)

    @property
    def psnfail(self) -> int:
        """Get or set the Optional shell part set ID specifying which part IDs are checked by the FAIL1 and ¦ÒFAIL4 options. If zero, all shell part IDs are included
        """ # nopep8
        return self._cards[3].get_value("psnfail")

    @psnfail.setter
    def psnfail(self, value: int) -> None:
        self._cards[3].set_value("psnfail", value)

    @property
    def keepcs(self) -> int:
        """Get or set the Flag to keep the contact segments of failed shell elements in the
        calculation. The contact segments of the failed shells remain active
        until a node shared by the segments has no active shells attached. Only
        then are the segments deleted..
        EQ.0: Inactive
        EQ.1: Active.
        """ # nopep8
        return self._cards[3].get_value("keepcs")

    @keepcs.setter
    def keepcs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""keepcs must be one of {0,1}""")
        self._cards[3].set_value("keepcs", value)

    @property
    def delfr(self) -> int:
        """Get or set the Flag to delete shell elements whose neighboring shell elements have failed; consequently, the shell is detached from the structure and moving freely in space.  This condition is checked if NFAIL1 or NFAIL4 are nonzero.
        EQ.0:	Inactive
        EQ.1:	Isolated elements are deleted.
        EQ.2:	QuadrilateralIsolated quadrilateral elements that are isolated and triangular elements that are connected by only one node are deleted.
        EQ.3:	Elements that are either isolated or connected by only one node are deleted.
        """ # nopep8
        return self._cards[3].get_value("delfr")

    @delfr.setter
    def delfr(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""delfr must be one of {0,1,2,3}""")
        self._cards[3].set_value("delfr", value)

    @property
    def drcpsid(self) -> int:
        """Get or set the Part set ID for drilling rotation constraint method.
        """ # nopep8
        return self._cards[3].get_value("drcpsid")

    @drcpsid.setter
    def drcpsid(self, value: int) -> None:
        self._cards[3].set_value("drcpsid", value)

    @property
    def drcprm(self) -> float:
        """Get or set the Drilling rotation constraint parameter (default=1.0).
        """ # nopep8
        return self._cards[3].get_value("drcprm")

    @drcprm.setter
    def drcprm(self, value: float) -> None:
        self._cards[3].set_value("drcprm", value)

    @property
    def intperr(self) -> int:
        """Get or set the Flag for behavior in case of unwanted interpolation/extrapolation of
        initial stresses from *INITIAL_STRESS_SHELL.
        EQ.0: Only warning is written, calculation continues (default).
        EQ.1: Error exit, calculation stops.
        """ # nopep8
        return self._cards[3].get_value("intperr")

    @intperr.setter
    def intperr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""intperr must be one of {0,1}""")
        self._cards[3].set_value("intperr", value)

    @property
    def drcmth(self) -> int:
        """Get or set the Drilling rotation constraint method. Options to choose how drilling kinematics are determined.
        EQ.0:	Generalized drilling strain rate at shell element nodes involving drill rotation at the specific node plus
        the translational velocities of two adjacent nodes.See more details in Erhart and Borrvall[2013].
        EQ.1 : Direct use of the spin tensor(e.g.see section 21 in the LS - DYNA Theory Manual) with respect to the shell
        element normal direction, numerically integrated at element level.A similar approach is described in Kanok - Nukulchai[1979].
        """ # nopep8
        return self._cards[4].get_value("drcmth")

    @drcmth.setter
    def drcmth(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""drcmth must be one of {0,1}""")
        self._cards[4].set_value("drcmth", value)

    @property
    def lispsid(self) -> int:
        """Get or set the Part set ID related to *INITIAL_STRESS_SHELL. For all parts in this set,
        the initial stress components SIGXX, SIGYY, ..., SIGZX are defined in the local (element) coordinate system.
        """ # nopep8
        return self._cards[4].get_value("lispsid")

    @lispsid.setter
    def lispsid(self, value: int) -> None:
        self._cards[4].set_value("lispsid", value)

    @property
    def nlocdt(self) -> int:
        """Get or set the Flag for time step handling for shell elements with offset. If the shell reference surface is offset by NLOC (*SECTION_SHELL) or OFFSET (*ELEMENT_SHELL), the time step size of those shell elements is reduced to fix instabilities. The reduction of the time step size is based on numerical tests which show a dependence on the offset distance and the ratio of shell thickness to edge length (T/L).
        EQ.0:	Reduce time step size up to 10 % to avoid instabilities.Care has to be taken since a smaller time step will lead to larger masses due to mass scaling.
        EQ.1 : No reduction of time step to restore prior behavior if necessary.Instabilities were most likely observed for aspect ratios of T / L > 0.5
        """ # nopep8
        return self._cards[4].get_value("nlocdt")

    @nlocdt.setter
    def nlocdt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nlocdt must be one of {0,1}""")
        self._cards[4].set_value("nlocdt", value)

