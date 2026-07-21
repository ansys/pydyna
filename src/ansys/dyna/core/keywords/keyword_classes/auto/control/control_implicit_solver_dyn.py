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

"""Module providing the ControlImplicitSolverDyn class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITSOLVERDYN_CARD0 = (
    FieldSchema("lsolvr", int, 0, 10, 7),
    FieldSchema("lprint", int, 10, 10, 0),
    FieldSchema("negev", int, 20, 10, 2),
    FieldSchema("order", int, 30, 10, 0),
    FieldSchema("drcm", int, 40, 10, 4),
    FieldSchema("drcprm", float, 50, 10, None),
    FieldSchema("autospc", int, 60, 10, 1),
    FieldSchema("autotol", float, 70, 10, None),
)

_CONTROLIMPLICITSOLVERDYN_CARD1 = (
    FieldSchema("lcpack", int, 0, 10, 2),
    FieldSchema("mtxdmp", int, 10, 10, 0),
    FieldSchema("iparm1", int, 20, 10, 1000),
    FieldSchema("rparm1", float, 30, 10, 1e-12),
    FieldSchema("rparm2", float, 40, 10, 1e-08),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("rparm4", float, 60, 10, 240.0),
    FieldSchema("rparm5", float, 70, 10, 0.0),
)

_CONTROLIMPLICITSOLVERDYN_CARD2 = (
    FieldSchema("emxdmp", int, 0, 10, 0),
    FieldSchema("rdcmem", float, 10, 10, 0.85),
    FieldSchema("absmem", float, 20, 10, None),
    FieldSchema("isingle", int, 30, 10, None),
    FieldSchema("iblropt", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("ispd", int, 60, 10, None),
    FieldSchema("memlvl", float, 70, 10, None),
)

class ControlImplicitSolverDyn(KeywordBase):
    """DYNA CONTROL_IMPLICIT_SOLVER_DYN keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_SOLVER_DYN"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitSolverDyn class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITSOLVERDYN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITSOLVERDYN_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITSOLVERDYN_CARD2,
                **kwargs,
            ),
        ]
    @property
    def lsolvr(self) -> int:
        """Get or set the Linear equation solver method (see Remarks below).
        EQ.7: Parallel multi-frontal sparse solver (default).Iterative if ISINGLE > 0 or IBLROPT > 0.
        EQ.2: Parallel multi-frontal sparse solver (default)
        EQ.22: iterative, CG with diagonal preconditioner when LCPACK = 2 (default) or GMRES with diagonal preconditioner when LCPACK = 3 (nonsymmetric matrix assembly)
        EQ.23: iterative, CG with SGS preconditioner
        EQ.24: iterative, CG with SSOR preconditioner
        EQ.25: iterative, CG with modified ILDLTD preconditioner
        EQ.26: iterative, CG with modified ILDLTO preconditioner that requires extra storage
        EQ.30 Parallel direct/hybrid solver MUMPS
        EQ.90: User Supplied Linear Equation Solver SMP only:
        EQ.6: BCSLIB-EXT, direct, sparse, double precision
        """ # nopep8
        return self._cards[0].get_value("lsolvr")

    @lsolvr.setter
    def lsolvr(self, value: int) -> None:
        """Set the lsolvr property."""
        if value not in [7, 2, 22, 23, 24, 25, 26, 30, 90, 6, None]:
            raise Exception("""lsolvr must be `None` or one of {7,2,22,23,24,25,26,30,90,6}.""")
        self._cards[0].set_value("lsolvr", value)

    @property
    def lprint(self) -> int:
        """Get or set the Linear solver print flag controls screen and message file output (see Remarks below).
        EQ.0: no printing
        EQ.1: output summary statistics on memory, cpu requirements
        EQ.2: more statistics
        EQ.3: even more statistics and debug checking
        During execution, use the interactive command "<ctrl-c>lprint" to toggle this print flag between 0 and 1.
        """ # nopep8
        return self._cards[0].get_value("lprint")

    @lprint.setter
    def lprint(self, value: int) -> None:
        """Set the lprint property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""lprint must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("lprint", value)

    @property
    def negev(self) -> int:
        """Get or set the Negative eigenvalue flag.  Selects procedure when negative eigenvalues are detected during stiffness matrix inversion (see Remarks below).
        EQ.1: stop, or retry step if auto step control is active
        EQ.2: print warning message, try to continue (default)
        """ # nopep8
        return self._cards[0].get_value("negev")

    @negev.setter
    def negev(self, value: int) -> None:
        """Set the negev property."""
        if value not in [2, 1, None]:
            raise Exception("""negev must be `None` or one of {2,1}.""")
        self._cards[0].set_value("negev", value)

    @property
    def order(self) -> int:
        """Get or set the Ordering option (see Remarks below)
        EQ.0: Method set automatically by LS-DYNA
        EQ.1: MMD, Multiple Minimum Degree.
        EQ.2: METIS
        EQ: ParMETIS
        EQ.4: LSGpart.
        """ # nopep8
        return self._cards[0].get_value("order")

    @order.setter
    def order(self, value: int) -> None:
        """Set the order property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""order must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("order", value)

    @property
    def drcm(self) -> int:
        """Get or set the Drilling rotation constraint method for shells (see Remarks below).
        EQ.1: add drilling stiffness (old Version 970 method)
        EQ.2: same as 4 below
        EQ.3: add no drilling stiffness
        EQ.4: add drilling stiffness (improved method) (default).
        """ # nopep8
        return self._cards[0].get_value("drcm")

    @drcm.setter
    def drcm(self, value: int) -> None:
        """Set the drcm property."""
        if value not in [4, 1, 2, 3, None]:
            raise Exception("""drcm must be `None` or one of {4,1,2,3}.""")
        self._cards[0].set_value("drcm", value)

    @property
    def drcprm(self) -> typing.Optional[float]:
        """Get or set the Drilling rotation constraint parameter for shells. This parameter scales the drilling stiffness.
        For the old method (DRCM = 1) the default value of DRCPRM is 1.0 for linear analysis,
        100.0 for nonlinear implicit analysis, and either 1.E-12 or 1.E-8 for eigenvalue analysis depending on the shell element type.
        For eigenvalue analysis, the input value for DRCPRM is ignored.  For the improved method (default, DRCM = 4),
        the default value of DRCPRM is as described above for the old method except the default value of DRCPRM is 1.0 for nonlinear implicit analysis.
        """ # nopep8
        return self._cards[0].get_value("drcprm")

    @drcprm.setter
    def drcprm(self, value: float) -> None:
        """Set the drcprm property."""
        self._cards[0].set_value("drcprm", value)

    @property
    def autospc(self) -> int:
        """Get or set the Automatic Constraint Scan flag
        EQ.1: scan the assembled stiffness matrix looking for unconstrained, unattached degrees of freedom.
        Generate additional constraints as necessary to avoid negative eigenvalues.
        EQ.2: do not add constraints.
        """ # nopep8
        return self._cards[0].get_value("autospc")

    @autospc.setter
    def autospc(self, value: int) -> None:
        """Set the autospc property."""
        if value not in [1, 2, None]:
            raise Exception("""autospc must be `None` or one of {1,2}.""")
        self._cards[0].set_value("autospc", value)

    @property
    def autotol(self) -> typing.Optional[float]:
        """Get or set the AUTOSPC tolerance.  The test for singularity is the ratio of the smallest singular value and the largest singular value.
        If this ratio is less than AUTOTOL, then the triple of columns is declared singular and a constraint is generated.
        Default values in single and double precision are 1e-4 and 10e-8, respectively.
        """ # nopep8
        return self._cards[0].get_value("autotol")

    @autotol.setter
    def autotol(self, value: float) -> None:
        """Set the autotol property."""
        self._cards[0].set_value("autotol", value)

    @property
    def lcpack(self) -> int:
        """Get or set the Matrix assembly package:
        EQ.2: Default.
        EQ.3: Same as 2, but incorporates a non-symmetric linear solver; see Remarks below
        """ # nopep8
        return self._cards[1].get_value("lcpack")

    @lcpack.setter
    def lcpack(self, value: int) -> None:
        """Set the lcpack property."""
        if value not in [2, 3, None]:
            raise Exception("""lcpack must be `None` or one of {2,3}.""")
        self._cards[1].set_value("lcpack", value)

    @property
    def mtxdmp(self) -> int:
        """Get or set the Matrix and right-hand-side dumping.  LS-DYNA has the option of dumping the globally assembled stiffness matrix and right-hand-side vector files in Harwell-Boeing sparse matrix format. Such output may help with comparing to other linear equation solution packages.
        EQ.0: No dumping
        GT.0: Dump all matrices and right-hand-side vectors every MTXDMP time steps.
        Output is written as ASCII text and the involved filenames are of the following form: K_xxxx_yyy.mtx.rb
        This file contains the stiffness matrix at step xxxx, iteration yyy.  M_xxxx_yyy.mtx.rb
        This file contains the mass matrix at step xxxx, iteration yyy.  Only for eigenvalue analysis.  W_xxxx_yyy.mtx.rb
        This file contains the damping matrix at step xxxx, iteration yyy.  Only for simulations with damping. K_xxxx_yyy_zzz.rhs.rb
        This file contains the right hand side at step xxxx, iteration yyy, where yyyis the iteration at which a stiffness matrix is formed; and zzz is the cumulative iteration number for the step.
        The values of yyy and zzz dont always coincide because the stiffness matrix is not necessarily reformed every iteration. Node_Data_xxxx_yyy
        This file maps stiffness matrix to nodes and provides nodal coordinates.
        LT.0: Like positive values of MTXDMP but dumped data is binary.
        EQ.|9999|: Simulation is terminated after dumping matrices and right hand side before factorization
        """ # nopep8
        return self._cards[1].get_value("mtxdmp")

    @mtxdmp.setter
    def mtxdmp(self, value: int) -> None:
        """Set the mtxdmp property."""
        self._cards[1].set_value("mtxdmp", value)

    @property
    def iparm1(self) -> int:
        """Get or set the For iterative solvers, maximum number of iterations.The default is 1000 for LSOLVR = 7. The default is 10000 for 22 <= LSOLVR <= 26.
        """ # nopep8
        return self._cards[1].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[1].set_value("iparm1", value)

    @property
    def rparm1(self) -> float:
        """Get or set the For iterative solvers, absolute tolerance for convergence.  The default is 10e-12.
        """ # nopep8
        return self._cards[1].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        """Set the rparm1 property."""
        self._cards[1].set_value("rparm1", value)

    @property
    def rparm2(self) -> float:
        """Get or set the For iterative solvers, relative tolerance for convergence.  The default is 10e-8.
        """ # nopep8
        return self._cards[1].get_value("rparm2")

    @rparm2.setter
    def rparm2(self, value: float) -> None:
        """Set the rparm2 property."""
        self._cards[1].set_value("rparm2", value)

    @property
    def rparm4(self) -> float:
        """Get or set the nFor LSOLVR =  7 with LCPACK = 2 (symmetric) and IBLROPT > 0, nominal block size. The default is 240
        """ # nopep8
        return self._cards[1].get_value("rparm4")

    @rparm4.setter
    def rparm4(self, value: float) -> None:
        """Set the rparm4 property."""
        self._cards[1].set_value("rparm4", value)

    @property
    def rparm5(self) -> float:
        """Get or set the For LSOLVR = 7 with LCPACK = 2 (symmetric) and IBLROPT > 0, compression tolerance for the low-rank approximation.
        For LSOLVR = 30, RPARM5 is the compression tolerance used to compute a low - rank factorization with the MUMPS solver(see Remark 2).
        """ # nopep8
        return self._cards[1].get_value("rparm5")

    @rparm5.setter
    def rparm5(self, value: float) -> None:
        """Set the rparm5 property."""
        self._cards[1].set_value("rparm5", value)

    @property
    def emxdmp(self) -> int:
        """Get or set the Flag for dumping elemental stiffness and mass matrices:
        EQ.0: No dumping
        GT.0: Dump all elemental matrices every EMXDMP time steps.
        Output is written as ASCII text and the involved filenames are of the following form: ElmStfMtx_xxxx_yyy
        This file contains the elemental stiffness matrix at step xxxx, iteration yyy. ElmMssMtx_xxxx_yyy
        This file contains the elemental mass matrix at step xxxx, iteration yyy.
        LT.0: Like positive values of MTXDMP but dumped data is binary.
        EQ.|9999|: Simulation is terminated after dumping matrices and right hand side prior to factorization
        """ # nopep8
        return self._cards[2].get_value("emxdmp")

    @emxdmp.setter
    def emxdmp(self, value: int) -> None:
        """Set the emxdmp property."""
        self._cards[2].set_value("emxdmp", value)

    @property
    def rdcmem(self) -> float:
        """Get or set the Starting with LS-DYNA R11, the memory for linear algebra has been moved from static to dynamic memory allocation.
        For implicit applications we have found that some operating systems are not robust when queried about how much dynamic memory is free. This factor caps the amount of dynamic memory requested for linear algebra applications to RDCMEM times the amount the operating system declares available.
        0.85 seems to work well for most systems. If you are using a workstation and starting up other applications while running LS-DYNA, you may need to use a number like 0.50
        """ # nopep8
        return self._cards[2].get_value("rdcmem")

    @rdcmem.setter
    def rdcmem(self, value: float) -> None:
        """Set the rdcmem property."""
        self._cards[2].set_value("rdcmem", value)

    @property
    def absmem(self) -> typing.Optional[float]:
        """Get or set the Absolute upper bound for the dynamic memory allocated for factorization. The allocated memory will be bounded above by the min(RDCME NWORDS ,ABSMEM ) where NWORDS is the number of available words determined by the operating system. If the predicted amount of required memory is less than this value, then less memory than this bound may be allocated.
        """ # nopep8
        return self._cards[2].get_value("absmem")

    @absmem.setter
    def absmem(self, value: float) -> None:
        """Set the absmem property."""
        self._cards[2].set_value("absmem", value)

    @property
    def isingle(self) -> typing.Optional[int]:
        """Get or set the Only active if LSOLVR = 7 and LCPACK = 2. This field controls tasks performed in single precision (see Remark 10):
        EQ.0: No single precision used.
        EQ.1: The factors of the matrix, which are computed in double precision, are stored in single precision, reducing the storage required to solve the linear system.
        EQ.2: Same as 1, but additionally some of the factorization operations are performed in single precision, reducing the runtime.
        """ # nopep8
        return self._cards[2].get_value("isingle")

    @isingle.setter
    def isingle(self, value: int) -> None:
        """Set the isingle property."""
        self._cards[2].set_value("isingle", value)

    @property
    def iblropt(self) -> typing.Optional[int]:
        """Get or set the Flag providing options for the block low-rank (BLR) computations. It is only active if LSOLVR = 7 and LCPACK = 2. As the value of IBLROPT increases, the treatment is increasingly aggressive. However, increasingly aggressive compression can introduce additional error into the factors of the linear system, which can slow down the convergence of the subsequent numerical solve or even prevent convergence. See Remark 10.
        EQ.0: BLR is not used.
        EQ.1: Factor, Solve, Update,and Compress algorithm(FSUC).A full - rank factorization is performed,and off - diagonal blocks of the factors are then subject to compression with BLR.This method should reduce the storage required to factor and solve the linear system.However, the additional BLR computations increase the runtime.
        EQ.2: Factor, Solve, Compress,and Update algorithm(FSCU).FSUC is applied with the fully assembled equations of the frontal matrix.Where BLR compression is applied, low - rank updates are performed to compute the Schur complement of the frontal matrix.This method should reduce both storage and runtime.
        EQ.3: Update, Factor, Solve,and Compress algorithm(UFSC).Low - rank approximations are used to update blocks within the fully assembled equations in a frontal matrix.Where BLR compression is applied, low - rank computations are also performed when computing the Schur complement of the frontal matrix.This option should reduce storage and further reduce runtime.
        EQ.4: UFSC is performed: In addition, BLR is applied to the contribution blocks of low-rank frontal matrices before they are placed on the real stack. This step further reduces the working storage needed for the low-rank factorization. The additional BLR computations increase the runtime relative to UFSC.
        """ # nopep8
        return self._cards[2].get_value("iblropt")

    @iblropt.setter
    def iblropt(self, value: int) -> None:
        """Set the iblropt property."""
        self._cards[2].set_value("iblropt", value)

    @property
    def ispd(self) -> typing.Optional[int]:
        """Get or set the Symmetric positive definite flag. Only active when LSOLVR = 7 and LCPACK = 2.
        EQ.0: The solver only assumes that the linear system is symmetric positive definite if the diagonals of the factorization are all positive.
        EQ.1: The solver assumes that the linear system is symmetric positive definite and ignores spurious negative diagonal entries.
        """ # nopep8
        return self._cards[2].get_value("ispd")

    @ispd.setter
    def ispd(self, value: int) -> None:
        """Set the ispd property."""
        self._cards[2].set_value("ispd", value)

    @property
    def memlvl(self) -> typing.Optional[float]:
        """Get or set the Control the load-balancing behavior. Only active when LSOLVR = 7, LCPACK = 2, and ISINGLE > 0. The solver's default behavior is to load balance to reduce runtime. That behavior can cause a subset of processors to have a disproportionate share of the factor entries and drive such processors out-of-core, even when enough storage should be available. MEMLVL enables instructing the solver to consider storage as well.
        EQ.0.0: Load balancing is entirely based on minimizing in - core runtime.
        GT.1.0: Perform load balancing to reduce storage disparity among the processors.
        GT.1.0: The first load balance is performed to reduce runtime.Then, it is adjusted to reduce any outlier cores, such that they have no more than MEMLVL times an equal share of the factor entries.In this case, MEMLVL is an aspirational target that the solver cannot always achieve.
        """ # nopep8
        return self._cards[2].get_value("memlvl")

    @memlvl.setter
    def memlvl(self, value: float) -> None:
        """Set the memlvl property."""
        self._cards[2].set_value("memlvl", value)

