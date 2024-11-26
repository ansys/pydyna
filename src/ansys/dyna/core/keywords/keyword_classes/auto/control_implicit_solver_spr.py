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

class ControlImplicitSolverSpr(KeywordBase):
    """DYNA CONTROL_IMPLICIT_SOLVER_SPR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_SOLVER_SPR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lsolvr",
                        int,
                        0,
                        10,
                        kwargs.get("lsolvr", 2)
                    ),
                    Field(
                        "lprint",
                        int,
                        10,
                        10,
                        kwargs.get("lprint", 0)
                    ),
                    Field(
                        "negev",
                        int,
                        20,
                        10,
                        kwargs.get("negev", 2)
                    ),
                    Field(
                        "order",
                        int,
                        30,
                        10,
                        kwargs.get("order", 0)
                    ),
                    Field(
                        "drcm",
                        int,
                        40,
                        10,
                        kwargs.get("drcm", 4)
                    ),
                    Field(
                        "drcprm",
                        float,
                        50,
                        10,
                        kwargs.get("drcprm")
                    ),
                    Field(
                        "autospc",
                        int,
                        60,
                        10,
                        kwargs.get("autospc", 1)
                    ),
                    Field(
                        "autotol",
                        float,
                        70,
                        10,
                        kwargs.get("autotol")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcpack",
                        int,
                        0,
                        10,
                        kwargs.get("lcpack", 2)
                    ),
                    Field(
                        "mtxdmp",
                        int,
                        10,
                        10,
                        kwargs.get("mtxdmp", 0)
                    ),
                    Field(
                        "iparm1",
                        int,
                        20,
                        10,
                        kwargs.get("iparm1", 500)
                    ),
                    Field(
                        "rparm1",
                        float,
                        30,
                        10,
                        kwargs.get("rparm1", 10.0e-10)
                    ),
                    Field(
                        "rparm2",
                        float,
                        40,
                        10,
                        kwargs.get("rparm2", 10.0e-4)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "rparm5",
                        float,
                        70,
                        10,
                        kwargs.get("rparm5", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "emxdmp",
                        int,
                        0,
                        10,
                        kwargs.get("emxdmp", 0)
                    ),
                    Field(
                        "rdcmem",
                        float,
                        10,
                        10,
                        kwargs.get("rdcmem", 0.85)
                    ),
                    Field(
                        "absmem",
                        float,
                        10,
                        10,
                        kwargs.get("absmem")
                    ),
                ],
            ),
        ]

    @property
    def lsolvr(self) -> int:
        """Get or set the Linear equation solver method (see Remarks below).
        EQ.2:	Parallel multi-frontal sparse solver (default)
        EQ.22:	iterative, CG with diagonal preconditioner
        EQ.23:	iterative, CG with SGS preconditioner
        EQ.24:	iterative, CG with SSOR preconditioner
        EQ.25:	iterative, CG with modified ILDLTD preconditioner
        EQ.26:	iterative, CG with modified ILDLTO preconditioner that requires extra storage
        EQ.30 Parallel direct/hybrid solver MUMPS
        EQ.90:	User Supplied Linear Equation Solver SMP only:
        EQ.6:	BCSLIB-EXT, direct, sparse, double precision
        """ # nopep8
        return self._cards[0].get_value("lsolvr")

    @lsolvr.setter
    def lsolvr(self, value: int) -> None:
        if value not in [2, 22, 23, 24, 25, 26, 30, 90, 6]:
            raise Exception("""lsolvr must be one of {2,22,23,24,25,26,30,90,6}""")
        self._cards[0].set_value("lsolvr", value)

    @property
    def lprint(self) -> int:
        """Get or set the Linear solver print flag controls screen and message file output (see Remarks below).
        EQ.0:	no printing
        EQ.1:	output summary statistics on memory, cpu requirements
        EQ.2:	more statistics
        EQ.3:	even more statistics and debug checking
        During execution, use the interactive command "<ctrl-c>lprint" to toggle this print flag between 0 and 1.
        """ # nopep8
        return self._cards[0].get_value("lprint")

    @lprint.setter
    def lprint(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""lprint must be one of {0,1,2,3}""")
        self._cards[0].set_value("lprint", value)

    @property
    def negev(self) -> int:
        """Get or set the Negative eigenvalue flag.  Selects procedure when negative eigenvalues are detected during stiffness matrix inversion (see Remarks below).
        EQ.1:	stop, or retry step if auto step control is active
        EQ.2:	print warning message, try to continue (default)
        """ # nopep8
        return self._cards[0].get_value("negev")

    @negev.setter
    def negev(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""negev must be one of {2,1}""")
        self._cards[0].set_value("negev", value)

    @property
    def order(self) -> int:
        """Get or set the Ordering option (see Remarks below)
        EQ.0:	Method set automatically by LS-DYNA
        EQ.1:	MMD, Multiple Minimum Degree.
        EQ.2:	Metis
        EQ.4:	LSGpart.
        """ # nopep8
        return self._cards[0].get_value("order")

    @order.setter
    def order(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""order must be one of {0,1,2,4}""")
        self._cards[0].set_value("order", value)

    @property
    def drcm(self) -> int:
        """Get or set the Drilling rotation constraint method for shells (see Remarks below).
        EQ.1:	add drilling stiffness (old Version 970 method)
        EQ.2:	same as 4 below
        EQ.3:	add no drilling stiffness
        EQ.4:	add drilling stiffness (improved method) (default).
        """ # nopep8
        return self._cards[0].get_value("drcm")

    @drcm.setter
    def drcm(self, value: int) -> None:
        if value not in [4, 1, 2, 3]:
            raise Exception("""drcm must be one of {4,1,2,3}""")
        self._cards[0].set_value("drcm", value)

    @property
    def drcprm(self) -> typing.Optional[float]:
        """Get or set the Drilling rotation constraint parameter for shells. This parameter scales the drilling stiffness.
        For the old method (DRCM = 1) the default value of DRCPRM is 1.0 for linear analysis,
        100.0 for nonlinear implicit analysis, and either 1.E-12 or 1.E-8 for eigenvalue analysis depending on the shell element type.
        For eigenvalue analysis, the input value for DRCPRM is ignored.  For the improved method (default, DRCM = 4),
        the default value of DRCPRM is as described above for the old method except default DRCPRM is 1.0 for nonlinear implicit analysis.
        """ # nopep8
        return self._cards[0].get_value("drcprm")

    @drcprm.setter
    def drcprm(self, value: float) -> None:
        self._cards[0].set_value("drcprm", value)

    @property
    def autospc(self) -> int:
        """Get or set the Automatic Constraint Scan flag
        EQ.1:	scan the assembled stiffness matrix looking for unconstrained, unattached degrees of freedom.
        Generate additional constraints as necessary to avoid negative eigenvalues.
        EQ.2:	do not add constraints.
        """ # nopep8
        return self._cards[0].get_value("autospc")

    @autospc.setter
    def autospc(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""autospc must be one of {1,2}""")
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
        self._cards[0].set_value("autotol", value)

    @property
    def lcpack(self) -> int:
        """Get or set the Matrix assembly package:
        EQ.2:	Default.
        EQ.3:	Same as 2, but incorporates a non-symmetric linear solver; see Remarks below
        """ # nopep8
        return self._cards[1].get_value("lcpack")

    @lcpack.setter
    def lcpack(self, value: int) -> None:
        if value not in [2, 3]:
            raise Exception("""lcpack must be one of {2,3}""")
        self._cards[1].set_value("lcpack", value)

    @property
    def mtxdmp(self) -> int:
        """Get or set the Matrix and right-hand-side dumping.  LS-DYNA has the option of dumping the globally assembled stiffness matrix and right-hand-side vectors files in Harwell-Boeing sparse matrix format.
        Such output may be useful for comparing to other linear equation solution packages.
        EQ.0:	No dumping
        GT.0:	Dump all matrices and right-hand-side vectors every MTXDMP time steps.
        Output is written as ASCII text and the involved filenames are of the following form:	K_xxxx_yyy.mtx.rb
        This file contains the stiffness matrix at step xxxx, iteration yyy.		M_xxxx_yyy.mtx.rb
        This file contains the mass matrix at step xxxx, iteration yyy.  Only for eigenvalue analysis.		W_xxxx_yyy.mtx.rb
        This file contains the damping matrix at step xxxx, iteration yyy.  Only for simulations with damping. K_xxxx_yyy_zzz.rhs.rb
        This file contains the right hand side at step xxxx, iteration yyy, where yyyis the iteration at which a stiffness matrix is formed; and zzz is the cumulative iteration number for the step.
        The values of yyy and zzz don’t always coincide because the stiffness matrix is not necessarily reformed every iteration. Node_Data_xxxx_yyy
        This file maps stiffness matrix to nodes and provides nodal coordinates.
        LT.0:	Like positive values of MTXDMP but dumped data is binary.
        EQ.|9999|:	Simulation is terminated after dumping matrices and right hand side prior to factorization
        """ # nopep8
        return self._cards[1].get_value("mtxdmp")

    @mtxdmp.setter
    def mtxdmp(self, value: int) -> None:
        self._cards[1].set_value("mtxdmp", value)

    @property
    def iparm1(self) -> int:
        """Get or set the For 22 <= LSOLVR <= 26 only, maximum number of iterations.  Default is 500
        """ # nopep8
        return self._cards[1].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        self._cards[1].set_value("iparm1", value)

    @property
    def rparm1(self) -> float:
        """Get or set the For 22 <= LSOLVR <= 26 only, absolute tolerance for convergence.  Default is 10e-10.
        """ # nopep8
        return self._cards[1].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        self._cards[1].set_value("rparm1", value)

    @property
    def rparm2(self) -> float:
        """Get or set the For 22 <= LSOLVR <= 26 only, relative tolerance for convergence.  Default is 10e-4.
        """ # nopep8
        return self._cards[1].get_value("rparm2")

    @rparm2.setter
    def rparm2(self, value: float) -> None:
        self._cards[1].set_value("rparm2", value)

    @property
    def rparm5(self) -> float:
        """Get or set the For LSOLVR = 30 only, compression tolerance used to compute a low - rank factorization with the MUMPS solver.Default is 0.0.
        """ # nopep8
        return self._cards[1].get_value("rparm5")

    @rparm5.setter
    def rparm5(self, value: float) -> None:
        self._cards[1].set_value("rparm5", value)

    @property
    def emxdmp(self) -> int:
        """Get or set the Flag for dumping elemental stiffness and mass matrices:
        EQ.0:	No dumping
        GT.0:	Dump all elemental matrices every EMXDMP time steps.
        Output is written as ASCII text and the involved filenames are of the following form: ElmStfMtx_xxxx_yyy
        This file contains the elemental stiffness matrix at step xxxx, iteration yyy. ElmMssMtx_xxxx_yyy
        This file contains the elemental mass matrix at step xxxx, iteration yyy.
        LT.0:	Like positive values of MTXDMP but dumped data is binary.
        EQ.|9999|:	Simulation is terminated after dumping matrices and right hand side prior to factorization
        """ # nopep8
        return self._cards[2].get_value("emxdmp")

    @emxdmp.setter
    def emxdmp(self, value: int) -> None:
        self._cards[2].set_value("emxdmp", value)

    @property
    def rdcmem(self) -> float:
        """Get or set the Starting with LS-DYNA R11, the memory for linear algebra has been moved from static memory allocation to dynamic memory allocation.
        For implicit applications we have found that some operating systems are not “robust” when queried about how much dynamic memory is free.
        This factor caps the amount of dynamic memory requested for linear algebra applications to RDCMEM times the amount that the operating system declares available.
        0.85 seems to work well for most systems. If you are using a workstation and starting up other applications while running LS-DYNA, you may need to use a number like 0.50
        """ # nopep8
        return self._cards[2].get_value("rdcmem")

    @rdcmem.setter
    def rdcmem(self, value: float) -> None:
        self._cards[2].set_value("rdcmem", value)

    @property
    def absmem(self) -> typing.Optional[float]:
        """Get or set the Absolute upper bound for the dynamic memory allocated for factorization. The allocated memory will be bounded above by the min⁡(RDCME ×NWORDS ,ABSMEM ) where NWORDS is the number of available words as determined by the operating system. If the predicted amount of required memory is less than this value, then less memory than this bound may be allocated.
        """ # nopep8
        return self._cards[2].get_value("absmem")

    @absmem.setter
    def absmem(self, value: float) -> None:
        self._cards[2].set_value("absmem", value)

