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

"""Module providing the ControlShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLSHELL_CARD0 = (
    FieldSchema("wrpang", float, 0, 10, 20.0),
    FieldSchema("esort", int, 10, 10, 0),
    FieldSchema("irnxx", int, 20, 10, -1),
    FieldSchema("istupd", int, 30, 10, 0),
    FieldSchema("theory", int, 40, 10, 2),
    FieldSchema("bwc", int, 50, 10, 2),
    FieldSchema("miter", int, 60, 10, 1),
    FieldSchema("proj", int, 70, 10, 0),
)

_CONTROLSHELL_CARD1 = (
    FieldSchema("rotascl", float, 0, 10, 1.0),
    FieldSchema("intgrd", int, 10, 10, 0),
    FieldSchema("lamsht", int, 20, 10, 0),
    FieldSchema("cstyp6", int, 30, 10, 1),
    FieldSchema("thshel", int, 40, 10, 0),
)

_CONTROLSHELL_CARD2 = (
    FieldSchema("pstupd", int, 0, 10, 0),
    FieldSchema("sidt4tu", int, 10, 10, 0),
    FieldSchema("cntco", int, 20, 10, 0),
    FieldSchema("itsflg", int, 30, 10, 0),
    FieldSchema("irquad", int, 40, 10, 3),
    FieldSchema("w_mode", float, 50, 10, None, "w-mode"),
    FieldSchema("stretch", float, 60, 10, None),
    FieldSchema("icrq", int, 70, 10, 0),
)

_CONTROLSHELL_CARD3 = (
    FieldSchema("nfail1", int, 0, 10, None),
    FieldSchema("nfail4", int, 10, 10, None),
    FieldSchema("psnfail", int, 20, 10, 0),
    FieldSchema("keepcs", int, 30, 10, 0),
    FieldSchema("delfr", int, 40, 10, 0),
    FieldSchema("drcpsid", int, 50, 10, 0),
    FieldSchema("drcprm", float, 60, 10, 1.0),
    FieldSchema("intperr", int, 70, 10, 0),
)

_CONTROLSHELL_CARD4 = (
    FieldSchema("drcmth", int, 0, 10, 0),
    FieldSchema("lispsid", int, 10, 10, 0),
    FieldSchema("nlocdt", int, 20, 10, 0),
    FieldSchema("iswshl", int, 30, 10, 0),
)

class ControlShell(KeywordBase):
    """DYNA CONTROL_SHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "SHELL"
    _link_fields = {
        "sidt4tu": LinkType.SET_PART,
        "psnfail": LinkType.SET_PART,
        "drcpsid": LinkType.SET_PART,
        "lispsid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLSHELL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSHELL_CARD1,
                active_func=lambda: self._cards[1].has_nondefault_values() or self._cards[2].active,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSHELL_CARD2,
                active_func=lambda: self._cards[2].has_nondefault_values() or self._cards[3].active,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSHELL_CARD3,
                active_func=lambda: self._cards[3].has_nondefault_values() or self._cards[4].active,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLSHELL_CARD4,
                active_func=lambda: self._cards[4].has_nondefault_values(),
                **kwargs,
            ),
        ]
    @property
    def wrpang(self) -> float:
        """Get or set the Shell element warpage angle in degrees. If a warpage greater than this angle is found, a warning message is printed. (Default is 20 degrees).
        """ # nopep8
        return self._cards[0].get_value("wrpang")

    @wrpang.setter
    def wrpang(self, value: float) -> None:
        """Set the wrpang property."""
        self._cards[0].set_value("wrpang", value)

    @property
    def esort(self) -> int:
        """Get or set the Sorting of triangular shell elements to automatically switch degenerate quadrilateral shell formulations to more suitable triangular shell formulations.
        EQ.0: Do not sort(default).
        EQ.1: Sort(switch to C0 triangular shell formulation 4, or if a quadratic shell, switch to shell formulation 24, or if a shell formulation with thickness stretch, switch to shell formulation 27).
        EQ.2: Sort(switch to DKT triangular shell formulation 17, or if the shell is quadratic, switch to shell formulation 24).The DKT formulation will be unstable for an uncommonly thick, triangular shell.
        """ # nopep8
        return self._cards[0].get_value("esort")

    @esort.setter
    def esort(self, value: int) -> None:
        """Set the esort property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""esort must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("esort", value)

    @property
    def irnxx(self) -> int:
        """Get or set the Shell normal update option.  This option applies to the Hughes-Liu formulations (ELFORMs 1, 6, 7, and 11), the BWC formulation (ELFORM 10), and if warping stiffness is turned on, the Belytschko-Tsay formulations (ELFORMs 2 and 25 with BWC = 1 and ELFORMs -16, 16, and 26 with hourglass type 8).
        EQ. - 2: Unique nodal fibers which are incrementally updated based on the nodal rotation at the location of the fiber
        EQ. - 1: Recomputed fiber directions each cycle
        EQ.0: Default set to - 1
        EQ.1: Compute on restarts
        EQ.n: Compute every n cycles(Hughes - Liu shells only)
        """ # nopep8
        return self._cards[0].get_value("irnxx")

    @irnxx.setter
    def irnxx(self, value: int) -> None:
        """Set the irnxx property."""
        self._cards[0].set_value("irnxx", value)

    @property
    def istupd(self) -> int:
        """Get or set the Shell thickness change option for deformable shells.  For crash analysis, neglecting the elastic component of the strains, ISTUPD = 4, may improve energy conservation and stability.  The option specified with ISTUPD applies to all shell parts unless PSTUPD (and optionally, SIDT4TU) on Card 3 is specified. See the description of those variables below.
        EQ.0: No thickness change(default)
        EQ.1: Membrane straining causes thickness change in 3 and 4 node shell elements.This option is important in sheet metal forming or whenever membrane stretching is important.
        EQ.2: Membrane straining causes thickness change in 8 node thick shell elements, types 1 and 2. We do not recommend this option for implicit or explicit solutions which use the fully integrated type 2 elements.Types 3 and 5 thick shells are continuum - based,and thickness changes are always considered.
        EQ.3: Options 1 and 2 apply
        EQ.4: Option 1 applies, but the elastic strains are neglected for the thickness update.This option only applies to shells(not thick shells) and the most common elastic - plastic materials for which the elastic response is isotropic.See SIDT4TU for selective application of this option.
        EQ.5: 	Same as 1, but thickness changes are stored with double precision in single-precision binaries for shell elements with ELFORM = 2, 4, or 16. All other shell element types are stored with single precision in single-precision binaries. For type 16 shells, the internal energy computation is also stored with double precision in single-precision binaries.  These storage changes should make results more comparable (single precision versus double precision) in long-lasting explicit analyses.
        EQ.6:	Same as 4, but thickness changes are stored with double precision in single - precision binaries for shell elements with ELFORM = 2, 4, or 16. All other shell element types are stored with single precision in single - precision binaries.For type 16 shells, the internal energy computation is also stored with double precision in single - precision binaries.These storage changes should make results more comparable(single precision versus double precision) in long - lasting explicit analyses.
        """ # nopep8
        return self._cards[0].get_value("istupd")

    @istupd.setter
    def istupd(self, value: int) -> None:
        """Set the istupd property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, None]:
            raise Exception("""istupd must be `None` or one of {0,1,2,3,4,5,6}.""")
        self._cards[0].set_value("istupd", value)

    @property
    def theory(self) -> int:
        """Get or set the Default shell formulation.  For a complete list of shell formulations, refer to *SECTION_SHELL.  For remarks on overriding this default and how THEORY may affect contact behavior, see Remark 2.
        EQ.1: Hughes - Liu
        EQ.2: Belytschko - Tsay(default)
        EQ.3: BCIZ triangular shell(not recommended)
        EQ.4: C0 triangular shell
        EQ.5: Belytschko - Tsay membrane
        EQ.6: S / R Hughes Liu
        EQ.7: S / R co - rotational Hughes Liu
        EQ.8: Belytschko - Leviathan shell
        EQ.9: Fully integrated Belytschko - Tsay membrane
        EQ.10: Belytschko - Wong - Chiang
        EQ.11: Fast(co - rotational) Hughes - Liu
        EQ.12: Plane stress(xy - plane)
        EQ.13: Plane strain(xy - plane)
        EQ.14: Axisymmetric solid(y - axis of symmetry)  area weighted.See Remark 6
        EQ.15: Axisymmetric solid(y - axis of symmetry)  volume weighted.See Remark 6
        EQ.16: Fully integrated shell element(very fast)
        EQ.17: Discrete Kirchhoff triangular shell(DKT)
        EQ.18: Discrete Kirchhoff linear shell either quadrilateral or Triangular with 6DOF per node
        EQ.20: C0 linear shell element with 6 DOF per node
        EQ.21: C0 linear shell element with 5 DOF per node with the Pian - Sumihara membrane hybrid quadrilateral membrane
        EQ.25: Belytschko - Tsay shell with thickness stretch
        EQ.26: Fully integrated shell with thickness stretch
        EQ.27: C0 triangular shell with thickness stretch
        """ # nopep8
        return self._cards[0].get_value("theory")

    @theory.setter
    def theory(self, value: int) -> None:
        """Set the theory property."""
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
        """Set the bwc property."""
        if value not in [2, 1, None]:
            raise Exception("""bwc must be `None` or one of {2,1}.""")
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
        """Set the miter property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""miter must be `None` or one of {1,2,3}.""")
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
        """Set the proj property."""
        if value not in [0, 1, None]:
            raise Exception("""proj must be `None` or one of {0,1}.""")
        self._cards[0].set_value("proj", value)

    @property
    def rotascl(self) -> float:
        """Get or set the Define a scale factor for the rotary shell mass.  This option is not for general use.  The rotary inertia for shells is automatically scaled to permit a larger time step size.  A scale factor other than the default, that is, unity, is not recommended.
        """ # nopep8
        return self._cards[1].get_value("rotascl")

    @rotascl.setter
    def rotascl(self, value: float) -> None:
        """Set the rotascl property."""
        self._cards[1].set_value("rotascl", value)

    @property
    def intgrd(self) -> int:
        """Get or set the Default through thickness numerical integration rule for shells and thick shells.  If more than 10 integration points are requested, a trapezoidal rule is used unless a user defined rule is specified.
        EQ.0: Gauss integration.If 1 - 10 integration points are specified, the default rule is Gauss integration.
        EQ.1: Lobatto integration.If 3 - 10 integration points are specified, the default rule is Lobatto.For two point integration, the Lobatto rule is very inaccurate, so Gauss integration is used instead.Lobatto integration has an advantage in that the innerand outer integration points are on the shell surfaces
        """ # nopep8
        return self._cards[1].get_value("intgrd")

    @intgrd.setter
    def intgrd(self, value: int) -> None:
        """Set the intgrd property."""
        self._cards[1].set_value("intgrd", value)

    @property
    def lamsht(self) -> int:
        """Get or set the Laminated shell theory flag.  Except for those using the Green-Lagrange strain tensor, laminated shell theory is available for all thin shell and thick shell materials (this feature is developed and implemented by Professor Ala Tabiei).  It is activated when LAMSHT = 3, 4, or 5 and by using *PART_COMPOSITE or *INTEGRATION_SHELL to define the integration rule.  See Remark 7.
        EQ.0: Do not update shear corrections.
        EQ.1: Activate laminated shell theory.
        EQ.3: Activate laminated thin shells.
        EQ.4: Activate laminated shell theory for thick shells.
        EQ.5: Activate laminated shell theory for thinand thick shells
        """ # nopep8
        return self._cards[1].get_value("lamsht")

    @lamsht.setter
    def lamsht(self, value: int) -> None:
        """Set the lamsht property."""
        self._cards[1].set_value("lamsht", value)

    @property
    def cstyp6(self) -> int:
        """Get or set the Coordinate system for the type 6 shell element.  The default system computes a unique local system at each in plane point.  The uniform local system computes just one system used throughout the shell element.  This involves fewer calculations and is therefore more efficient.  The change of systems has a slight effect on results; therefore, the older, less efficient method is the default.
        EQ.1: Variable local coordinate system(default)
        EQ.2: Uniform local system
        """ # nopep8
        return self._cards[1].get_value("cstyp6")

    @cstyp6.setter
    def cstyp6(self, value: int) -> None:
        """Set the cstyp6 property."""
        self._cards[1].set_value("cstyp6", value)

    @property
    def thshel(self) -> int:
        """Get or set the Thermal shell option (applies only to thermal and coupled structural thermal analyses). See parameter THERM on DATABASE_EXTENT_BINARY keyword.
        EQ.0: No temperature gradient is considered through the shell thickness(default).
        EQ.1: A temperature gradient is calculated through the shell thickness
        """ # nopep8
        return self._cards[1].get_value("thshel")

    @thshel.setter
    def thshel(self, value: int) -> None:
        """Set the thshel property."""
        self._cards[1].set_value("thshel", value)

    @property
    def pstupd(self) -> int:
        """Get or set the |PSTUPD| is the optional shell part set ID specifying which part ID's have or do not have their thickness updated.  The shell thickness update by default applies to all shell elements in the mesh.  Generally, this part set ID is not needed.
        LT.0: these shell parts are excluded from the shell thickness update
        EQ.0: all deformable shells have their thickness updated
        GT.0: these shell parts are included in the shell thickness update
        """ # nopep8
        return self._cards[2].get_value("pstupd")

    @pstupd.setter
    def pstupd(self, value: int) -> None:
        """Set the pstupd property."""
        self._cards[2].set_value("pstupd", value)

    @property
    def sidt4tu(self) -> int:
        """Get or set the Shell part set ID for parts that use the type 4 thickness update where elastic strains are ignored.  The shell parts in part set SIDT4TU must also be included in the part set defined by PSTUPD.  SIDT4TU has no effect unless ISTUPD is set to 1 or 3.  Shell parts in the shell part set PSTUPD that are not also in the shell part set SIDT4TU use the type 1 thickness update.
        """ # nopep8
        return self._cards[2].get_value("sidt4tu")

    @sidt4tu.setter
    def sidt4tu(self, value: int) -> None:
        """Set the sidt4tu property."""
        self._cards[2].set_value("sidt4tu", value)

    @property
    def cntco(self) -> int:
        """Get or set the Flag affecting location of contact surfaces for shells when NLOC is nonzero in *SECTION_SHELL or in *PART_COMPOSITE, or when OFFSET is specified using *ELEMENT_SHELL_OFFSET.  CNTCO is not supported for the tracked side of NODES_TO_SURFACE type contacts, nor does it have any effect on Mortar contacts. For Mortar contacts NLOC or OFFSET completely determines the location of the contact surfaces, as if CNTCO = 1.
        EQ.0: NLOC and OFFSET have no effect on the location of the shell contact surfaces.
        EQ.1: Contact reference plane(see Remark 3) coincides with shell reference surface.
        EQ.2: Contact reference plane(see Remark 3) is affected by contact thickness.This is typically not physical.
        EQ.3: Similar to 1 but with improved behavior at corners or folds in the mesh in certain cases.See Remarks 3 and 4.
        EQ.4: Similar to 2 but with improved behavior at corners or folds in the mesh in certain cases.See Remarks 3 and 4.
        """ # nopep8
        return self._cards[2].get_value("cntco")

    @cntco.setter
    def cntco(self, value: int) -> None:
        """Set the cntco property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""cntco must be `None` or one of {0,1,2,3,4}.""")
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
        """Set the itsflg property."""
        if value not in [0, 1, None]:
            raise Exception("""itsflg must be `None` or one of {0,1}.""")
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
        """Set the irquad property."""
        if value not in [3, 2, None]:
            raise Exception("""irquad must be `None` or one of {3,2}.""")
        self._cards[2].set_value("irquad", value)

    @property
    def w_mode(self) -> typing.Optional[float]:
        """Get or set the W-Mode amplitude for element deletion, specified in degrees
        """ # nopep8
        return self._cards[2].get_value("w_mode")

    @w_mode.setter
    def w_mode(self, value: float) -> None:
        """Set the w_mode property."""
        self._cards[2].set_value("w_mode", value)

    @property
    def stretch(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio of element diagonals for quadrilateral elements or of side edges for triangular elements that results in element deletion. This option is activated only if either NFAIL1 or NFAIL4 are nonzero and STRETCH > 0.0
        """ # nopep8
        return self._cards[2].get_value("stretch")

    @stretch.setter
    def stretch(self, value: float) -> None:
        """Set the stretch property."""
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
        """Set the icrq property."""
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
        """Set the nfail1 property."""
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
        """Set the nfail4 property."""
        self._cards[3].set_value("nfail4", value)

    @property
    def psnfail(self) -> int:
        """Get or set the Optional shell part set ID specifying which part IDs are checked by the FAIL1 and FAIL4 options. If zero, all shell part IDs are included
        """ # nopep8
        return self._cards[3].get_value("psnfail")

    @psnfail.setter
    def psnfail(self, value: int) -> None:
        """Set the psnfail property."""
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
        """Set the keepcs property."""
        if value not in [0, 1, None]:
            raise Exception("""keepcs must be `None` or one of {0,1}.""")
        self._cards[3].set_value("keepcs", value)

    @property
    def delfr(self) -> int:
        """Get or set the Flag to delete shell elements whose neighboring shell elements have failed; consequently, the shell is detached from the structure and moving freely in space.  This condition is checked if NFAIL1 or NFAIL4 are nonzero.
        EQ.0: Inactive
        EQ.1: Isolated elements are deleted.
        EQ.2: QuadrilateralIsolated quadrilateral elements that are isolated and triangular elements that are connected by only one node are deleted.
        EQ.3: Elements that are either isolated or connected by only one node are deleted.
        """ # nopep8
        return self._cards[3].get_value("delfr")

    @delfr.setter
    def delfr(self, value: int) -> None:
        """Set the delfr property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""delfr must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("delfr", value)

    @property
    def drcpsid(self) -> int:
        """Get or set the Part set ID for drilling rotation constraint method.
        """ # nopep8
        return self._cards[3].get_value("drcpsid")

    @drcpsid.setter
    def drcpsid(self, value: int) -> None:
        """Set the drcpsid property."""
        self._cards[3].set_value("drcpsid", value)

    @property
    def drcprm(self) -> float:
        """Get or set the Drilling rotation constraint parameter (default=1.0).
        """ # nopep8
        return self._cards[3].get_value("drcprm")

    @drcprm.setter
    def drcprm(self, value: float) -> None:
        """Set the drcprm property."""
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
        """Set the intperr property."""
        if value not in [0, 1, None]:
            raise Exception("""intperr must be `None` or one of {0,1}.""")
        self._cards[3].set_value("intperr", value)

    @property
    def drcmth(self) -> int:
        """Get or set the Drilling rotation constraint method. Options to choose how drilling kinematics are determined.
        EQ.0: Generalized drilling strain rate at shell element nodes involving drill rotation at the specific node plus
        the translational velocities of two adjacent nodes.See more details in Erhart and Borrvall[2013].
        EQ.1: Direct use of the spin tensor(for example see Stress Update overview in the LS - DYNA Theory Manual) with respect to the shell
        element normal direction, numerically integrated at element level.A similar approach is described in Kanok - Nukulchai[1979].
        """ # nopep8
        return self._cards[4].get_value("drcmth")

    @drcmth.setter
    def drcmth(self, value: int) -> None:
        """Set the drcmth property."""
        if value not in [0, 1, None]:
            raise Exception("""drcmth must be `None` or one of {0,1}.""")
        self._cards[4].set_value("drcmth", value)

    @property
    def lispsid(self) -> int:
        """Get or set the Part set ID related to *INITIAL_STRESS_SHELL. For all parts in this set,
        the initial stress components SIGXX, SIGYY, ..., SIGZX are defined in the local (element) coordinate system.
        """ # nopep8
        return self._cards[4].get_value("lispsid")

    @lispsid.setter
    def lispsid(self, value: int) -> None:
        """Set the lispsid property."""
        self._cards[4].set_value("lispsid", value)

    @property
    def nlocdt(self) -> int:
        """Get or set the Flag for time step handling for shell elements with offset. If the shell reference surface is offset by NLOC (*SECTION_SHELL) or OFFSET (*ELEMENT_SHELL), the time step size of those shell elements is reduced to fix instabilities. The reduction of the time step size is based on numerical tests which show a dependence on the offset distance and the ratio of shell thickness to edge length (T/L).
        EQ.0: Reduce time step size up to 10 % to avoid instabilities.Care has to be taken since a smaller time step will lead to larger masses due to mass scaling.
        EQ.1: No reduction of time step to restore prior behavior if necessary.Instabilities were most likely observed for aspect ratios of T / L > 0.5
        """ # nopep8
        return self._cards[4].get_value("nlocdt")

    @nlocdt.setter
    def nlocdt(self, value: int) -> None:
        """Set the nlocdt property."""
        if value not in [0, 1, None]:
            raise Exception("""nlocdt must be `None` or one of {0,1}.""")
        self._cards[4].set_value("nlocdt", value)

    @property
    def iswshl(self) -> int:
        """Get or set the Flag for switching between formulations 16 and 30:
        EQ.0: Do not convert the shell formulations.
        EQ.1: Convert all formulation 16 shells to 30.
        EQ.2: Convert all formulation 30 shells to 16.
        """ # nopep8
        return self._cards[4].get_value("iswshl")

    @iswshl.setter
    def iswshl(self, value: int) -> None:
        """Set the iswshl property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iswshl must be `None` or one of {0,1,2}.""")
        self._cards[4].set_value("iswshl", value)

    @property
    def sidt4tu_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for sidt4tu."""
        return self._get_set_link("PART", self.sidt4tu)

    @sidt4tu_link.setter
    def sidt4tu_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for sidt4tu."""
        self.sidt4tu = value.sid

    @property
    def psnfail_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psnfail."""
        return self._get_set_link("PART", self.psnfail)

    @psnfail_link.setter
    def psnfail_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psnfail."""
        self.psnfail = value.sid

    @property
    def drcpsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for drcpsid."""
        return self._get_set_link("PART", self.drcpsid)

    @drcpsid_link.setter
    def drcpsid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for drcpsid."""
        self.drcpsid = value.sid

    @property
    def lispsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for lispsid."""
        return self._get_set_link("PART", self.lispsid)

    @lispsid_link.setter
    def lispsid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for lispsid."""
        self.lispsid = value.sid

