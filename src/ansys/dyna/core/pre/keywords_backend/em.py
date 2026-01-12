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

"""Electromagnetic (EM) keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class EMKeywordsMixin:
    """Mixin class providing electromagnetic keyword creation methods."""

    def create_em_control(
        self,
        emsol: int = 0,
        numls: int = 100,
        macrodt: float = 0.0,
        dimtype: int = 0,
        nperio: int = 2,
        ncylfem: int = 5000,
        ncylbem: int = 5000,
    ) -> bool:
        """Create an EM_CONTROL keyword.

        Parameters
        ----------
        emsol : int
            EM solver type.
        numls : int
            Number of local EM steps.
        macrodt : float
            Macro time step.
        dimtype : int
            Problem dimension type.
        nperio : int
            Number of periods.
        ncylfem : int
            FEM recalculation cycles.
        ncylbem : int
            BEM recalculation cycles.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmControl()
        kw.emsol = emsol
        kw.numls = numls
        kw.macrodt = macrodt
        kw.dimtype = dimtype
        kw.nperio = nperio
        kw.ncylfem = ncylfem
        kw.ncylbem = ncylbem

        self._deck.append(kw)
        logger.debug(f"Created EM_CONTROL with emsol={emsol}")
        return True

    def create_em_timestep(
        self,
        tstype: int = 1,
        dtconst: float = 0.0,
        factor: float = 1.0,
        rlcsf: int = 25,
    ) -> bool:
        """Create an EM_CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        tstype : int
            Time step type.
        dtconst : float
            Constant time step value.
        factor : float
            Time step factor.
        rlcsf : int
            RLC sub-cycling factor.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmControlTimestep()
        kw.tstype = tstype
        kw.dtcons = dtconst
        kw.factor = factor
        kw.rlcsf = rlcsf

        self._deck.append(kw)
        logger.debug(f"Created EM_CONTROL_TIMESTEP with tstype={tstype}, dtcons={dtconst}")
        return True

    def create_em_output(
        self,
        mats: int = 0,
        matf: int = 0,
        sols: int = 0,
        solf: int = 0,
    ) -> bool:
        """Create an EM_OUTPUT keyword.

        Parameters
        ----------
        mats : int
            Matrix output level to screen.
        matf : int
            Matrix output level to file.
        sols : int
            Solver output level to screen.
        solf : int
            Solver output level to file.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmOutput()
        kw.mats = mats
        kw.matf = matf
        kw.sols = sols
        kw.solf = solf

        self._deck.append(kw)
        logger.debug(f"Created EM_OUTPUT with mats={mats}, matf={matf}, sols={sols}, solf={solf}")
        return True

    def create_em_mat_001(
        self,
        mid: int,
        mtype: int = 0,
        sigma: float = 0.0,
    ) -> bool:
        """Create an EM_MAT_001 keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Material type.
        sigma : float
            Electrical conductivity.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmMat001()
        kw.mid = mid
        kw.mtype = mtype
        kw.sigma = sigma

        self._deck.append(kw)
        logger.debug(f"Created EM_MAT_001 with mid={mid}, mtype={mtype}")
        return True

    def create_em_mat_004(
        self,
        mid: int,
        mtype: int = 0,
        sigma: float = 0.0,
        eosid: int = 0,
        nele: int = 0,
        murel: float = 0.0,
        eosmu: int = 0,
        deatht: float = 0.0,
    ) -> bool:
        """Create an EM_MAT_004 keyword (2D resistive heating material).

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Material type.
        sigma : float
            Electrical conductivity.
        eosid : int
            Equation of state ID.
        nele : int
            Number of elements.
        murel : float
            Relative permeability.
        eosmu : int
            EOS for permeability.
        deatht : float
            Death time.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmMat004()
        kw.mid = mid
        kw.mtype = mtype
        kw.sigma = sigma
        kw.eosid = eosid
        kw.nele = nele
        kw.murel = murel
        kw.eosmu = eosmu
        kw.deatht = deatht

        self._deck.append(kw)
        logger.debug(f"Created EM_MAT_004 with mid={mid}, mtype={mtype}")
        return True

    def create_em_isopotential(
        self,
        settype: int = 1,
        setid: int = 0,
        rdltype: int = 0,
    ) -> int:
        """Create an EM_ISOPOTENTIAL keyword.

        Parameters
        ----------
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.
        rdltype : int
            Randles layer type.

        Returns
        -------
        int
            Isopotential ID.
        """
        from ansys.dyna.core.keywords import keywords

        isoid = self.next_id("isopotential")

        kw = keywords.EmIsopotential()
        kw.isoid = isoid
        kw.settype = settype
        kw.setid = setid
        kw.rdltype = rdltype

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL with isoid={isoid}, settype={settype}, setid={setid}")
        return isoid

    def create_em_isopotential_connect(
        self,
        contype: int = 1,
        isoid1: int = 0,
        isoid2: int = 0,
        val: float = 0.0,
        lcid: int = 0,
        l: float = 0.0,
        c: float = 0.0,
        v0: float = 0.0,
    ) -> int:
        """Create an EM_ISOPOTENTIAL_CONNECT keyword.

        Parameters
        ----------
        contype : int
            Connection type.
        isoid1 : int
            First isopotential ID.
        isoid2 : int
            Second isopotential ID.
        val : float
            Value (resistance, voltage, or current).
        lcid : int
            Load curve ID.
        l : float
            Inductance (for RLC circuit).
        c : float
            Capacitance (for RLC circuit).
        v0 : float
            Initial voltage (for RLC circuit).

        Returns
        -------
        int
            Connection ID.
        """
        from ansys.dyna.core.keywords import keywords

        conid = self.next_id("isopotential_connect")

        kw = keywords.EmIsopotentialConnect()
        kw.conid = conid
        kw.contype = contype
        kw.isoid1 = isoid1
        kw.isoid2 = isoid2
        kw.val = val
        kw.lcid_rdlid = lcid
        # For RLC circuits (contype=6), set the second card values
        if contype == 6:
            kw.l = l
            kw.c = c
            kw.v0 = v0

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL_CONNECT with conid={conid}, contype={contype}")
        return conid

    def create_em_isopotential_rogo(
        self,
        isoid: int,
        settype: int = 1,
        setid: int = 0,
    ) -> bool:
        """Create an EM_ISOPOTENTIAL_ROGO keyword.

        Parameters
        ----------
        isoid : int
            Isopotential ID for Rogowski coil.
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmIsopotentialRogo()
        kw.isoid = isoid
        kw.settype = settype
        kw.setid = setid

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL_ROGO with isoid={isoid}, setid={setid}")
        return True

    def create_em_circuit_rogo(
        self,
        settype: int = 1,
        setid: int = 0,
        curtyp: int = 1,
    ) -> int:
        """Create an EM_CIRCUIT_ROGO keyword (for backward compatibility).

        Parameters
        ----------
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.
        curtyp : int
            Current type.

        Returns
        -------
        int
            Rogowski coil ID.
        """
        from ansys.dyna.core.keywords import keywords

        rogid = self.next_id("rogo")

        kw = keywords.EmCircuitRogo()
        kw.rogid = rogid
        kw.settype = settype
        kw.setid = setid
        kw.curtyp = curtyp

        self._deck.append(kw)
        logger.debug(f"Created EM_CIRCUIT_ROGO with rogid={rogid}, setid={setid}")
        return rogid

    def create_em_solver_fem(
        self,
        reltol: float = 1e-6,
        maxite: int = 1000,
        stype: int = 1,
        precon: int = 1,
        uselast: int = 1,
        ncyclfem: int = 3,
    ) -> bool:
        """Create an EM_SOLVER_FEM keyword.

        Parameters
        ----------
        reltol : float
            Relative tolerance.
        maxite : int
            Maximum iterations.
        stype : int
            Solver type.
        precon : int
            Preconditioner type.
        uselast : int
            Use last solution flag.
        ncyclfem : int
            Number of cycles.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmSolverFem()
        kw.reltol = reltol
        kw.maxite = maxite
        kw.stype = stype
        kw.precon = precon
        kw.uselast = uselast
        kw.ncyclfem = ncyclfem

        self._deck.append(kw)
        logger.debug(f"Created EM_SOLVER_FEM with stype={stype}, reltol={reltol}")
        return True

    def create_em_solver_bem(
        self,
        reltol: float = 1e-6,
        maxite: int = 1000,
        stype: int = 2,
        precon: int = 1,
        uselast: int = 1,
        ncyclbem: int = 3,
    ) -> bool:
        """Create an EM_SOLVER_BEM keyword.

        Parameters
        ----------
        reltol : float
            Relative tolerance.
        maxite : int
            Maximum iterations.
        stype : int
            Solver type (2=PCG).
        precon : int
            Preconditioner type.
        uselast : int
            Use last solution flag.
        ncyclbem : int
            Number of cycles.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmSolverBem()
        kw.reltol = reltol
        kw.maxite = maxite
        kw.stype = stype
        kw.precon = precon
        kw.uselast = uselast
        kw.ncyclbem = ncyclbem

        self._deck.append(kw)
        logger.debug(f"Created EM_SOLVER_BEM with stype={stype}, reltol={reltol}")
        return True

    def create_em_solver_bemmat(
        self,
        matid: int,
        reltol: float = 1e-6,
    ) -> bool:
        """Create an EM_SOLVER_BEMMAT keyword.

        Parameters
        ----------
        matid : int
            Material ID.
        reltol : float
            Relative tolerance for this material.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmSolverBemmat()
        kw.matid = matid
        kw.reltol = reltol

        self._deck.append(kw)
        logger.debug(f"Created EM_SOLVER_BEMMAT with matid={matid}, reltol={reltol}")
        return True

    def create_em_control_contact(
        self,
        emct: int = 1,
        cconly: int = 0,
        ctype: int = 0,
        cotype: int = 0,
        eps1: float = 0.3,
        eps2: float = 0.3,
        eps3: float = 0.3,
        d0: float = 0.0,
    ) -> bool:
        """Create an EM_CONTROL_CONTACT keyword.

        Parameters
        ----------
        emct : int
            EM contact flag.
        cconly : int
            Contact coupling only flag.
        ctype : int
            Contact type.
        cotype : int
            Contact output type.
        eps1 : float
            Penetration parameter 1.
        eps2 : float
            Penetration parameter 2.
        eps3 : float
            Penetration parameter 3.
        d0 : float
            Distance parameter.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmControlContact()
        kw.emct = emct
        kw.cconly = cconly
        kw.ctype = ctype
        kw.cotype = cotype
        kw.eps1 = eps1
        kw.eps2 = eps2
        kw.eps3 = eps3
        kw.d0 = d0

        self._deck.append(kw)
        logger.debug(f"Created EM_CONTROL_CONTACT with emct={emct}")
        return True

    def create_em_database_globalenergy(
        self,
        outlv: int = 0,
        dtout: float = 0.0,
    ) -> bool:
        """Create an EM_DATABASE_GLOBALENERGY keyword.

        Parameters
        ----------
        outlv : int
            Output level.
        dtout : float
            Output time interval.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmDatabaseGlobalenergy()
        kw.outlv = outlv
        kw.dtout = dtout

        self._deck.append(kw)
        logger.debug(f"Created EM_DATABASE_GLOBALENERGY with outlv={outlv}")
        return True
