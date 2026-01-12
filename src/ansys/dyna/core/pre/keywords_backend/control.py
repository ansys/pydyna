# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Control keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class ControlKeywordsMixin:
    """Mixin class providing control keyword creation methods."""

    def create_termination(self, endtim: float) -> bool:
        """Create a CONTROL_TERMINATION keyword.

        Parameters
        ----------
        endtim : float
            Termination time.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        self._termination_time = endtim

        kw = keywords.ControlTermination()
        kw.endtim = endtim

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_TERMINATION with endtim={endtim}")
        return True

    def create_control_accuracy(
        self,
        osu: int = 0,
        inn: int = 0,
        pidosu: int = 0,
        iacc: int = 0,
        exacc: float = 0.0,
    ) -> bool:
        """Create a CONTROL_ACCURACY keyword.

        Parameters
        ----------
        osu : int
            Objective stress update flag.
        inn : int
            Invariant node numbering flag.
        pidosu : int
            Part set ID for objective stress.
        iacc : int
            Implicit accuracy flag.
        exacc : float
            Explicit accuracy flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlAccuracy()
        kw.osu = osu
        kw.inn = inn
        kw.pidosu = pidosu
        kw.iacc = iacc
        kw.exacc = exacc

        self._deck.append(kw)
        logger.debug("Created CONTROL_ACCURACY")
        return True

    def create_control_energy(
        self,
        hgen: int = 1,
        rwen: int = 2,
        slnten: int = 1,
        rylen: int = 1,
        irgen: int = 2,
    ) -> bool:
        """Create a CONTROL_ENERGY keyword.

        Parameters
        ----------
        hgen : int
            Hourglass energy flag.
        rwen : int
            Rigidwall energy flag.
        slnten : int
            Sliding interface energy flag.
        rylen : int
            Rayleigh energy flag.
        irgen : int
            Initial geometry energy flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlEnergy()
        kw.hgen = hgen
        kw.rwen = rwen
        kw.slnten = slnten
        kw.rylen = rylen
        kw.irgen = irgen

        self._deck.append(kw)
        logger.debug("Created CONTROL_ENERGY")
        return True

    def create_control_hourglass(self, ihq: int = 1, qh: float = 0.1) -> bool:
        """Create a CONTROL_HOURGLASS keyword.

        Parameters
        ----------
        ihq : int
            Hourglass control type.
        qh : float
            Hourglass coefficient.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlHourglass()
        kw.ihq = ihq
        kw.qh = qh

        self._deck.append(kw)
        logger.debug("Created CONTROL_HOURGLASS")
        return True

    def create_control_bulk_viscosity(self, q1: float = 1.5, q2: float = 0.06, bulk_type: int = 1) -> bool:
        """Create a CONTROL_BULK_VISCOSITY keyword.

        Parameters
        ----------
        q1 : float
            Quadratic viscosity coefficient.
        q2 : float
            Linear viscosity coefficient.
        bulk_type : int
            Bulk viscosity type.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlBulkViscosity()
        kw.q1 = q1
        kw.q2 = q2
        kw.type = bulk_type

        self._deck.append(kw)
        logger.debug("Created CONTROL_BULK_VISCOSITY")
        return True

    def create_control_timestep(
        self,
        tssfac: float = 0.9,
        isdo: int = 0,
        dt2ms: float = 0.0,
        lctm: int = 0,
    ) -> bool:
        """Create a CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        tssfac : float
            Scale factor for computed time step.
        isdo : int
            Basis of time size calculation.
        dt2ms : float
            Time step size for mass scaled solutions.
        lctm : int
            Curve ID limiting max time step.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlTimestep()
        kw.tssfac = tssfac
        kw.isdo = isdo
        kw.dt2ms = dt2ms
        kw.lctm = lctm

        self._deck.append(kw)
        logger.debug("Created CONTROL_TIMESTEP")
        return True

    def create_control_solution(self, soln: int = 0) -> bool:
        """Create a CONTROL_SOLUTION keyword.

        Parameters
        ----------
        soln : int
            Solution type flag:
            - 0: Structural only (default)
            - 1: Thermal only
            - 2: Coupled structural-thermal

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlSolution()
        kw.soln = soln

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_SOLUTION with soln={soln}")
        return True

    def create_control_thermal_solver(self, atype: int = 0) -> bool:
        """Create a CONTROL_THERMAL_SOLVER keyword.

        Parameters
        ----------
        atype : int
            Analysis type:
            - 0: Steady state (default)
            - 1: Transient

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlThermalSolver()
        kw.atype = atype

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_THERMAL_SOLVER with atype={atype}")
        return True

    def create_control_thermal_timestep(
        self,
        ts: int = 0,
        tip: float = 1.0,
        its: float = 0.0,
        tmin: float = 0.0,
        tmax: float = 0.0,
        dtemp: float = 0.0,
        tscp: float = 0.0,
        lcts: int = 0,
    ) -> bool:
        """Create a CONTROL_THERMAL_TIMESTEP keyword.

        Parameters
        ----------
        ts : int
            Time step control.
        tip : float
            Thermal integration parameter.
        its : float
            Initial thermal time step.
        tmin : float
            Minimum thermal time step.
        tmax : float
            Maximum thermal time step.
        dtemp : float
            Maximum temperature change per step.
        tscp : float
            Scale factor for thermal time step.
        lcts : int
            Curve ID for time step control.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlThermalTimestep()
        kw.ts = ts
        kw.tip = tip
        kw.its = its
        kw.tmin = tmin
        kw.tmax = tmax
        kw.dtemp = dtemp
        kw.tscp = tscp
        kw.lcts = lcts

        self._deck.append(kw)
        logger.debug("Created CONTROL_THERMAL_TIMESTEP")
        return True

    def create_control_shell(
        self,
        wrpang: float = 20.0,
        esort: int = 0,
        irnxx: int = -1,
        istupd: int = 0,
        theory: int = 2,
        bwc: int = 2,
        miter: int = 1,
        proj: int = 0,
        irquad: int = 0,
    ) -> bool:
        """Create a CONTROL_SHELL keyword.

        Parameters
        ----------
        wrpang : float
            Shell element warpage angle in degrees.
        esort : int
            Sorting of triangular shell elements.
        irnxx : int
            Shell normal update option.
        istupd : int
            Shell thickness change option.
        theory : int
            Default shell formulation.
        bwc : int
            Warping stiffness for Belytschko-Tsay shells.
        miter : int
            Plane stress plasticity option.
        proj : int
            Projection method for warping stiffness.
        irquad : int
            In-plane integration rule for eight-node quadratic shell.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlShell()
        kw.wrpang = wrpang
        kw.esort = esort
        kw.irnxx = irnxx
        kw.istupd = istupd
        # theory=0 means use default, which the keyword class doesn't accept
        # so we only set it if non-zero
        if theory != 0:
            kw.theory = theory
        kw.bwc = bwc
        kw.miter = miter
        kw.proj = proj
        # irquad=0 means use default, which the keyword class doesn't accept
        if irquad != 0:
            kw.irquad = irquad

        self._deck.append(kw)
        logger.debug("Created CONTROL_SHELL")
        return True

    def create_control_contact(
        self,
        rwpnal: float = 0.0,
        shlthk: int = 0,
        orien: int = 1,
        ssthk: int = 0,
        ignore: int = 0,
        igactc: int = 0,
    ) -> bool:
        """Create a CONTROL_CONTACT keyword.

        Parameters
        ----------
        rwpnal : float
            Scale factor for rigid wall penalties.
        shlthk : int
            Shell thickness offset flag.
        orien : int
            Automatic reorientation flag.
        ssthk : int
            Default contact thickness flag.
        ignore : int
            Ignore initial penetrations flag.
        igactc : int
            Isogeometric shells for contact flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlContact()
        kw.rwpnal = rwpnal
        kw.shlthk = shlthk
        kw.orien = orien
        kw.ssthk = ssthk
        kw.ignore = ignore
        kw.igactc = igactc

        self._deck.append(kw)
        logger.debug("Created CONTROL_CONTACT")
        return True

    def create_control_discrete_element(
        self,
        ndamp: float = 0.0,
        tdamp: float = 0.0,
        frics: float = 0.0,
        fricr: float = 0.0,
        normk: float = 0.01,
        sheark: float = 0.2857,
    ) -> bool:
        """Create a CONTROL_DISCRETE_ELEMENT keyword.

        Parameters
        ----------
        ndamp : float, optional
            Normal damping coefficient. Default is 0.0.
        tdamp : float, optional
            Tangential damping coefficient. Default is 0.0.
        frics : float, optional
            Static coefficient of friction. Default is 0.0.
        fricr : float, optional
            Rolling friction coefficient. Default is 0.0.
        normk : float, optional
            Scale factor of the normal spring constant. Default is 0.01.
        sheark : float, optional
            Ratio between sheark/normk. Default is 0.2857.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlDiscreteElement()
        kw.ndamp = ndamp
        kw.tdamp = tdamp
        kw.frics = frics
        kw.fricr = fricr
        kw.normk = normk
        kw.sheark = sheark

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_DISCRETE_ELEMENT: ndamp={ndamp}, tdamp={tdamp}")
        return True

    def create_control_output(
        self,
        npopt: int = 0,
        neecho: int = 0,
        ikedit: int = 0,
        iflush: int = 0,
    ) -> bool:
        """Create a CONTROL_OUTPUT keyword.

        Parameters
        ----------
        npopt : int, optional
            Print suppression during input phase. Default is 0.
        neecho : int, optional
            Print echo of keyword data. Default is 0.
        ikedit : int, optional
            Edit frequency for the d3hsp file. Default is 0.
        iflush : int, optional
            Flush frequency for output files. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlOutput()
        kw.npopt = npopt
        kw.neecho = neecho
        kw.ikedit = ikedit
        kw.iflush = iflush

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_OUTPUT: npopt={npopt}, neecho={neecho}")
        return True

    def create_control_implicit_general(
        self,
        imflag: int = 1,
        dt0: float = 0.0,
        imform: int = 2,
        nsbs: int = 1,
    ) -> bool:
        """Create a CONTROL_IMPLICIT_GENERAL keyword.

        Parameters
        ----------
        imflag : int, optional
            Implicit analysis flag. Default is 1.
        dt0 : float, optional
            Initial time step size. Default is 0.0.
        imform : int, optional
            Implicit formulation. Default is 2.
        nsbs : int, optional
            Number of stiffness matrix reformations. Default is 1.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlImplicitGeneral()
        kw.imflag = imflag
        kw.dt0 = dt0
        kw.imform = imform
        kw.nsbs = nsbs

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_IMPLICIT_GENERAL: imflag={imflag}, dt0={dt0}")
        return True

    def create_control_implicit_solution(
        self,
        nsolvr: int = 1,
        ilimit: int = 11,
        maxref: int = 15,
        dctol: float = 0.001,
        ectol: float = 0.01,
        rctol: float = 1.0e10,
        lstol: float = 0.9,
        nlprint: int = 0,
        nlnorm: float = 0.0,
    ) -> bool:
        """Create a CONTROL_IMPLICIT_SOLUTION keyword.

        Parameters
        ----------
        nsolvr : int, optional
            Solver type. Default is 1.
        ilimit : int, optional
            Iteration limit. Default is 11.
        maxref : int, optional
            Maximum number of reformations. Default is 15.
        dctol : float, optional
            Displacement convergence tolerance. Default is 0.001.
        ectol : float, optional
            Energy convergence tolerance. Default is 0.01.
        rctol : float, optional
            Residual convergence tolerance. Default is 1.0e10.
        lstol : float, optional
            Line search tolerance. Default is 0.9.
        nlprint : int, optional
            Nonlinear solver print flag. Default is 0.
        nlnorm : float, optional
            Nonlinear residual norm type. Default is 0.0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlImplicitSolution()
        kw.nsolvr = nsolvr
        kw.ilimit = ilimit
        kw.maxref = maxref
        kw.dctol = dctol
        kw.ectol = ectol
        kw.rctol = rctol
        kw.lstol = lstol
        kw.nlprint = nlprint
        kw.nlnorm = nlnorm

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_IMPLICIT_SOLUTION: nsolvr={nsolvr}, ilimit={ilimit}")
        return True

    def create_control_implicit_eigenvalue(
        self,
        neig: int = 0,
        center: float = 0.0,
        eigmth: int = 2,
        shfscl: float = 0.0,
    ) -> bool:
        """Create a CONTROL_IMPLICIT_EIGENVALUE keyword.

        Parameters
        ----------
        neig : int, optional
            Number of eigenvalues to extract. Default is 0.
        center : float, optional
            Center frequency for eigenvalue extraction. Default is 0.0.
        eigmth : int, optional
            Eigenvalue extraction method. Default is 2.
        shfscl : float, optional
            Shift scale factor. Default is 0.0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlImplicitEigenvalue()
        kw.neig = neig
        kw.center = center
        kw.eigmth = eigmth
        kw.shfscl = shfscl

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_IMPLICIT_EIGENVALUE: neig={neig}, eigmth={eigmth}")
        return True

    def create_control_termination(
        self,
        endtim: float = 0.0,
        endcyc: int = 0,
        dtmin: float = 0.0,
        endeng: float = 0.0,
        endmas: float = 1.0e8,
        nosol: int = 0,
    ) -> bool:
        """Create a CONTROL_TERMINATION keyword with all parameters.

        Parameters
        ----------
        endtim : float, optional
            Termination time. Default is 0.0.
        endcyc : int, optional
            Termination cycle. Default is 0.
        dtmin : float, optional
            Minimum time step. Default is 0.0.
        endeng : float, optional
            Energy ratio for termination. Default is 0.0.
        endmas : float, optional
            Mass increase for termination. Default is 1.0e8.
        nosol : int, optional
            Flag to stop solution. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        self._termination_time = endtim

        kw = keywords.ControlTermination()
        kw.endtim = endtim
        kw.endcyc = endcyc
        kw.dtmin = dtmin
        kw.endeng = endeng
        kw.endmas = endmas
        kw.nosol = nosol

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_TERMINATION: endtim={endtim}, endcyc={endcyc}")
        return True

    def create_control_timestep(
        self,
        dtinit: float = 0.0,
        tssfac: float = 1.0,
        isdo: int = 0,
        tslimt: float = 0.0,
        dt2ms: float = 0.0,
        lctm: int = 0,
        erode: int = 0,
        ms1st: int = 0,
    ) -> bool:
        """Create a CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        dtinit : float, optional
            Initial time step. Default is 0.0.
        tssfac : float, optional
            Scale factor for time step. Default is 1.0.
        isdo : int, optional
            Time step control flag. Default is 0.
        tslimt : float, optional
            Time step size limit. Default is 0.0.
        dt2ms : float, optional
            Mass scaling time step. Default is 0.0.
        lctm : int, optional
            Load curve for time step multiplier. Default is 0.
        erode : int, optional
            Erosion flag. Default is 0.
        ms1st : int, optional
            Mass scaling first cycle. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlTimestep()
        kw.dtinit = dtinit
        kw.tssfac = tssfac
        kw.isdo = isdo
        kw.tslimt = tslimt
        kw.dt2ms = dt2ms
        kw.lctm = lctm
        kw.erode = erode
        kw.ms1st = ms1st

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_TIMESTEP: dtinit={dtinit}, tssfac={tssfac}")
        return True
