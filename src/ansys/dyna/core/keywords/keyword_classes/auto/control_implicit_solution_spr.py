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

"""Module providing the ControlImplicitSolutionSpr class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlImplicitSolutionSpr(KeywordBase):
    """DYNA CONTROL_IMPLICIT_SOLUTION_SPR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_SOLUTION_SPR"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitSolutionSpr class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsolvr",
                        int,
                        0,
                        10,
                        12,
                        **kwargs,
                    ),
                    Field(
                        "ilimit",
                        int,
                        10,
                        10,
                        11,
                        **kwargs,
                    ),
                    Field(
                        "maxref",
                        int,
                        20,
                        10,
                        15,
                        **kwargs,
                    ),
                    Field(
                        "dctol",
                        float,
                        30,
                        10,
                        0.001,
                        **kwargs,
                    ),
                    Field(
                        "ectol",
                        float,
                        40,
                        10,
                        0.01,
                        **kwargs,
                    ),
                    Field(
                        "rctol",
                        float,
                        50,
                        10,
                        1.0E+10,
                        **kwargs,
                    ),
                    Field(
                        "lstol",
                        float,
                        60,
                        10,
                        0.9,
                        **kwargs,
                    ),
                    Field(
                        "abstol",
                        float,
                        70,
                        10,
                        1.0E-10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dnorm",
                        int,
                        0,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "diverg",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "istif",
                        int,
                        20,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "nlprint",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nlnorm",
                        float,
                        40,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "d3itctl",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cpchk",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dmtol",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "emtol",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "rmtol",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        str,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nttol",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nrtol",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "rttol",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "rrtol",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "arcctl",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "arcdir",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "arclen",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "arcmth",
                        int,
                        30,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "arcdmp",
                        int,
                        40,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "arcpsi",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "arcalf",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "arctim",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lsmtd",
                        int,
                        0,
                        10,
                        4,
                        **kwargs,
                    ),
                    Field(
                        "lsdir",
                        int,
                        10,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "irad",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "srad",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "awgt",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sred",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nsolvr(self) -> int:
        """Get or set the Solution method for implicit analysis:
        EQ.-1: Multistep linear,
        EQ.1: Linear,
        EQ.6: Nonlinear with BFGS updates + arclength,
        EQ.7: Nonlinear with Broyden updates + arclength,
        EQ.8: Nonlinear with DFP updates + arclength,
        EQ.9: Nonlinear with Davidon updates + arclength.
        EQ.12: Nonlinear with BFGS updates.This solver incorporates different line search and integration schemes as compared to obsolete NSOLVR=2.  Inclusion of an arc length method is optional and is invoked by setting ARCMTH=3.
        """ # nopep8
        return self._cards[0].get_value("nsolvr")

    @nsolvr.setter
    def nsolvr(self, value: int) -> None:
        """Set the nsolvr property."""
        if value not in [12, -1, 1, 6, 7, 8, 9, None]:
            raise Exception("""nsolvr must be `None` or one of {12,-1,1,6,7,8,9}.""")
        self._cards[0].set_value("nsolvr", value)

    @property
    def ilimit(self) -> int:
        """Get or set the Iteration limit between automatic stiffness reformations.
        Default is set to ILIMIT = 11.
        """ # nopep8
        return self._cards[0].get_value("ilimit")

    @ilimit.setter
    def ilimit(self, value: int) -> None:
        """Set the ilimit property."""
        self._cards[0].set_value("ilimit", value)

    @property
    def maxref(self) -> int:
        """Get or set the Stiffness reformation limit per time step.
        LT.0:	If  matrix reformations occur, convergence for that time step is forced; see Remark 4.
        """ # nopep8
        return self._cards[0].get_value("maxref")

    @maxref.setter
    def maxref(self, value: int) -> None:
        """Set the maxref property."""
        self._cards[0].set_value("maxref", value)

    @property
    def dctol(self) -> float:
        """Get or set the Displacement relative convergence tolerance (see Remark 5).
        LT.0:	-DCTOL references a curve that defines tolerance as a function of time.
        """ # nopep8
        return self._cards[0].get_value("dctol")

    @dctol.setter
    def dctol(self, value: float) -> None:
        """Set the dctol property."""
        self._cards[0].set_value("dctol", value)

    @property
    def ectol(self) -> float:
        """Get or set the Energy relative convergence tolerance (see Remark 5).
        LT.0:	-ECTOL references a curve that defines tolerance as a function of time.
        """ # nopep8
        return self._cards[0].get_value("ectol")

    @ectol.setter
    def ectol(self, value: float) -> None:
        """Set the ectol property."""
        self._cards[0].set_value("ectol", value)

    @property
    def rctol(self) -> float:
        """Get or set the Residual (force) relative convergence tolerance (see Remark 5).
        LT.0:	-RCTOL references a curve that defines tolerance as a function of time
        """ # nopep8
        return self._cards[0].get_value("rctol")

    @rctol.setter
    def rctol(self, value: float) -> None:
        """Set the rctol property."""
        self._cards[0].set_value("rctol", value)

    @property
    def lstol(self) -> float:
        """Get or set the Line search convergence tolerance.
        Default is set to LSTOL = 0.9.
        LT.0: -LSTOL is the line search tolerance, but this option  activates an alternate strategy where line search acts only on the independent degrees of freedom. This is opposed to the default strategy, where prescribed motions on nodes and rigid bodies are also incorporated, sometimes leading to unnecessarily small time steps because of the requirement of fulfilling these boundary conditions
        """ # nopep8
        return self._cards[0].get_value("lstol")

    @lstol.setter
    def lstol(self, value: float) -> None:
        """Set the lstol property."""
        self._cards[0].set_value("lstol", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute convergence tolerance.
        LT.0:	Convergence detected when the residual norm is less than.Note : To drive convergence based on , set DCTOLand ECTOL to 10 - 20
        """ # nopep8
        return self._cards[0].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        """Set the abstol property."""
        self._cards[0].set_value("abstol", value)

    @property
    def dnorm(self) -> int:
        """Get or set the Displacement norm for convergence test:
        EQ.1: Increment vs. displacement over current step,
        EQ.2: Increment vs. total displacement (default).
        LT.0: |"DNORM" |; also activates reading of optional Card 2.1
        """ # nopep8
        return self._cards[1].get_value("dnorm")

    @dnorm.setter
    def dnorm(self, value: int) -> None:
        """Set the dnorm property."""
        self._cards[1].set_value("dnorm", value)

    @property
    def diverg(self) -> int:
        """Get or set the Divergence flag (force imbalance increase during equilibrium iterations):
        EQ.1: Reform stiffness if divergence detected (default),
        EQ.2: Ignore divergence.
        """ # nopep8
        return self._cards[1].get_value("diverg")

    @diverg.setter
    def diverg(self, value: int) -> None:
        """Set the diverg property."""
        if value not in [1, 2, None]:
            raise Exception("""diverg must be `None` or one of {1,2}.""")
        self._cards[1].set_value("diverg", value)

    @property
    def istif(self) -> int:
        """Get or set the Initial stiffness formation flag:
        EQ.1: Reform stiffness at start of each step (default),
        EQ.n: Reform stiffness at start of every n'th step.
        """ # nopep8
        return self._cards[1].get_value("istif")

    @istif.setter
    def istif(self, value: int) -> None:
        """Set the istif property."""
        self._cards[1].set_value("istif", value)

    @property
    def nlprint(self) -> int:
        """Get or set the Nonlinear solver print flag:
        EQ.0: No nolinear iteration information printed(new v970 default).
        EQ.1: Print iteration information to screen, messag, d3hsp files,
        EQ.2: Print extra norm information (NLNORM = 1).
        EQ.3: Same as 2, but also print information from line search.
        NOTE: during execution, sense switch nlprt can also be used to toggle this print flag on and off.
        """ # nopep8
        return self._cards[1].get_value("nlprint")

    @nlprint.setter
    def nlprint(self, value: int) -> None:
        """Set the nlprint property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""nlprint must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("nlprint", value)

    @property
    def nlnorm(self) -> float:
        """Get or set the Nonlinear convergence norm type:
        LT.0: Same as 4, but rotational degrees of freedom are scaled appropriately with characteristic length ABS(NLNORM) to account for units.
        EQ.1: consider translational and rotational degrees of freedom
        EQ.2: consider translational degrees of freedom only (default)
        EQ.4: consider sum of translational and rotational degrees of freedom, i.e., no separate treatment.
        """ # nopep8
        return self._cards[1].get_value("nlnorm")

    @nlnorm.setter
    def nlnorm(self, value: float) -> None:
        """Set the nlnorm property."""
        self._cards[1].set_value("nlnorm", value)

    @property
    def d3itctl(self) -> int:
        """Get or set the Control D3ITER database.  If nonzero, the search directions for the nonlinear implicit solution are written to the D3ITER database.  To reduce the size of the D3ITER database the database is reset every n time steps where n=D3ITCTL
        """ # nopep8
        return self._cards[1].get_value("d3itctl")

    @d3itctl.setter
    def d3itctl(self, value: int) -> None:
        """Set the d3itctl property."""
        self._cards[1].set_value("d3itctl", value)

    @property
    def cpchk(self) -> int:
        """Get or set the Contact penetration check flag
        EQ.0: no contact penetration is performed (default)
        EQ.1: check for contact penetration during the nonlinear solution
        procedure. If such penetration is found modify the line search to
        prevent unnecessary penetration.
        """ # nopep8
        return self._cards[1].get_value("cpchk")

    @cpchk.setter
    def cpchk(self, value: int) -> None:
        """Set the cpchk property."""
        if value not in [0, 1, None]:
            raise Exception("""cpchk must be `None` or one of {0,1}.""")
        self._cards[1].set_value("cpchk", value)

    @property
    def dmtol(self) -> float:
        """Get or set the Maximum displacement convergence tolerance; convergence is detected when the relative maximum nodal or rigid body displacement is less than this value.
        """ # nopep8
        return self._cards[2].get_value("dmtol")

    @dmtol.setter
    def dmtol(self, value: float) -> None:
        """Set the dmtol property."""
        self._cards[2].set_value("dmtol", value)

    @property
    def emtol(self) -> float:
        """Get or set the Maximum energy convergence tolerance; convergence is detected when the relative maximum nodal or rigid body energy increment is less than this value.
        """ # nopep8
        return self._cards[2].get_value("emtol")

    @emtol.setter
    def emtol(self, value: float) -> None:
        """Set the emtol property."""
        self._cards[2].set_value("emtol", value)

    @property
    def rmtol(self) -> float:
        """Get or set the Maximum residual convergence tolerance; convergence is detected when the relative maximum nodal or rigid body residual is less than this value.
        """ # nopep8
        return self._cards[2].get_value("rmtol")

    @rmtol.setter
    def rmtol(self, value: float) -> None:
        """Set the rmtol property."""
        self._cards[2].set_value("rmtol", value)

    @property
    def nttol(self) -> float:
        """Get or set the Nodal translational convergence tolerance; convergence is detected when the absolute maximum nodal translational residual is less than this value.
        """ # nopep8
        return self._cards[2].get_value("nttol")

    @nttol.setter
    def nttol(self, value: float) -> None:
        """Set the nttol property."""
        self._cards[2].set_value("nttol", value)

    @property
    def nrtol(self) -> float:
        """Get or set the Nodal rotational convergence tolerance; convergence is detected when the absolute maximum nodal rotational residual is less than this value.
        """ # nopep8
        return self._cards[2].get_value("nrtol")

    @nrtol.setter
    def nrtol(self, value: float) -> None:
        """Set the nrtol property."""
        self._cards[2].set_value("nrtol", value)

    @property
    def rttol(self) -> float:
        """Get or set the Rigid body translational convergence tolerance; convergence is detected when the absolute maximum rigid body translational residual is less than this value.
        """ # nopep8
        return self._cards[2].get_value("rttol")

    @rttol.setter
    def rttol(self, value: float) -> None:
        """Set the rttol property."""
        self._cards[2].set_value("rttol", value)

    @property
    def rrtol(self) -> float:
        """Get or set the Rigid body rotational convergence tolerance; convergence is detected when the absolute maximum rigid body rotational residual is less than this value.
        """ # nopep8
        return self._cards[2].get_value("rrtol")

    @rrtol.setter
    def rrtol(self, value: float) -> None:
        """Set the rrtol property."""
        self._cards[2].set_value("rrtol", value)

    @property
    def arcctl(self) -> int:
        """Get or set the Arc length controlling node ID:
        EQ.0: generalized arc length method (default).
        """ # nopep8
        return self._cards[3].get_value("arcctl")

    @arcctl.setter
    def arcctl(self, value: int) -> None:
        """Set the arcctl property."""
        self._cards[3].set_value("arcctl", value)

    @property
    def arcdir(self) -> int:
        """Get or set the Arc length controlling node direction (ignored if ARCCTL=0 above):
        EQ.1: global X-translation (default),
        EQ.2: global Y-translation,
        EQ.3: global Z-translation.
        """ # nopep8
        return self._cards[3].get_value("arcdir")

    @arcdir.setter
    def arcdir(self, value: int) -> None:
        """Set the arcdir property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""arcdir must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("arcdir", value)

    @property
    def arclen(self) -> float:
        """Get or set the Arc length size
        LE.0.0: chosen automatically using initial step size
        Default is set to ARCLEN = 0.0.
        """ # nopep8
        return self._cards[3].get_value("arclen")

    @arclen.setter
    def arclen(self, value: float) -> None:
        """Set the arclen property."""
        self._cards[3].set_value("arclen", value)

    @property
    def arcmth(self) -> int:
        """Get or set the Arc length method:
        EQ.1: Crisfield (default),
        EQ.2: Ramm.
        EQ.3: Modified Crisfield (used with NSOLVR = 12 only)
        """ # nopep8
        return self._cards[3].get_value("arcmth")

    @arcmth.setter
    def arcmth(self, value: int) -> None:
        """Set the arcmth property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""arcmth must be `None` or one of {1,2,3}.""")
        self._cards[3].set_value("arcmth", value)

    @property
    def arcdmp(self) -> int:
        """Get or set the Arc length damping option:
        EQ.1: On, oscillations in static solution are supressed,
        EQ.2: Off (default).
        """ # nopep8
        return self._cards[3].get_value("arcdmp")

    @arcdmp.setter
    def arcdmp(self, value: int) -> None:
        """Set the arcdmp property."""
        if value not in [2, 1, None]:
            raise Exception("""arcdmp must be `None` or one of {2,1}.""")
        self._cards[3].set_value("arcdmp", value)

    @property
    def arcpsi(self) -> float:
        """Get or set the Relative influence of load/time parameter in spherical arclength constraint,
        default value is 0 which corresponds to a cylindrical arclength
        constraint. Applies to ARCMTH = 3.
        """ # nopep8
        return self._cards[3].get_value("arcpsi")

    @arcpsi.setter
    def arcpsi(self, value: float) -> None:
        """Set the arcpsi property."""
        self._cards[3].set_value("arcpsi", value)

    @property
    def arcalf(self) -> float:
        """Get or set the Relative influence of predictor step direction for positioning of the arc
        center, default is 0 which means that the center is at the origin. Applies
        to ARCMTH = 3..
        """ # nopep8
        return self._cards[3].get_value("arcalf")

    @arcalf.setter
    def arcalf(self, value: float) -> None:
        """Set the arcalf property."""
        self._cards[3].set_value("arcalf", value)

    @property
    def arctim(self) -> float:
        """Get or set the Optional time when arc length method is initiated. Applies to ARCMTH = 3.
        """ # nopep8
        return self._cards[3].get_value("arctim")

    @arctim.setter
    def arctim(self, value: float) -> None:
        """Set the arctim property."""
        self._cards[3].set_value("arctim", value)

    @property
    def lsmtd(self) -> int:
        """Get or set the Line search convergence method:
        EQ.1: Energy method using only translational variables,
        EQ.2: Residual method,
        EQ.3: Energy method using both translational and rotational variables.
        EQ.4: Energy method using sum of translational and rotational degrees of freedom, i.e., no separate treatment (default)
        EQ.5: Same as 4, but account for residual norm growth to be extra conservative in step length (applies to NSOLVR=12)
        """ # nopep8
        return self._cards[4].get_value("lsmtd")

    @lsmtd.setter
    def lsmtd(self, value: int) -> None:
        """Set the lsmtd property."""
        if value not in [4, 1, 2, 3, 5, 6, None]:
            raise Exception("""lsmtd must be `None` or one of {4,1,2,3,5,6}.""")
        self._cards[4].set_value("lsmtd", value)

    @property
    def lsdir(self) -> int:
        """Get or set the Line search direction method:
        EQ.1: Search on all variables (traditional approach used in versions prior to 971),
        EQ.2: Search only on the independent (unconstrained) variables,
        EQ.3: Use adaptive line search (see AWGT, SRED),
        EQ.4: Use curved line search (see IRAD, SRAD).
        """ # nopep8
        return self._cards[4].get_value("lsdir")

    @lsdir.setter
    def lsdir(self, value: int) -> None:
        """Set the lsdir property."""
        if value not in [2, 1, 3, 4, None]:
            raise Exception("""lsdir must be `None` or one of {2,1,3,4}.""")
        self._cards[4].set_value("lsdir", value)

    @property
    def irad(self) -> float:
        """Get or set the Normalized curvature factor for curved line search, where 0 indicates a straight line search and 1 indicates full curved line search.
        """ # nopep8
        return self._cards[4].get_value("irad")

    @irad.setter
    def irad(self, value: float) -> None:
        """Set the irad property."""
        self._cards[4].set_value("irad", value)

    @property
    def srad(self) -> float:
        """Get or set the Radius of influence for determining curve in curved line search. For each independent node, all nodes within this radius are used for determining the curve. If 0, then all nodes connected to the same element as the independent node are used.
        """ # nopep8
        return self._cards[4].get_value("srad")

    @srad.setter
    def srad(self, value: float) -> None:
        """Set the srad property."""
        self._cards[4].set_value("srad", value)

    @property
    def awgt(self) -> float:
        """Get or set the Adaptive line search weight factor between 0 and 1. A high value tends to restrict the motion of oscillating nodes during the implicit process.
        """ # nopep8
        return self._cards[4].get_value("awgt")

    @awgt.setter
    def awgt(self, value: float) -> None:
        """Set the awgt property."""
        self._cards[4].set_value("awgt", value)

    @property
    def sred(self) -> float:
        """Get or set the Initial step reduction between 0 and 1 for adaptive line search, use large number for conservative start in  implicit procedure.
        """ # nopep8
        return self._cards[4].get_value("sred")

    @sred.setter
    def sred(self, value: float) -> None:
        """Set the sred property."""
        self._cards[4].set_value("sred", value)

