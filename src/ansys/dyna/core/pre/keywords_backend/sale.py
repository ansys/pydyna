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

"""SALE (Simplified ALE) keyword creation methods for the keywords backend."""

import logging
from typing import List

logger = logging.getLogger(__name__)


class SALEKeywordsMixin:
    """Mixin class providing SALE-related keyword creation methods."""

    def create_mat_vacuum(
        self,
        mid: int = None,
        rho: float = 1.0e-9,
    ) -> int:
        """Create a MAT_VACUUM keyword.

        Parameters
        ----------
        mid : int, optional
            Material ID. If not provided, auto-generates one.
        rho : float, optional
            Density. Default is 1.0e-9.

        Returns
        -------
        int
            The material ID.
        """
        from ansys.dyna.core.keywords import keywords

        if mid is None:
            mid = self.next_id("material")

        kw = keywords.MatVacuum()
        kw.mid = mid
        kw.rho = rho

        self._deck.append(kw)
        logger.debug(f"Created MAT_VACUUM with mid={mid}")
        return mid

    def create_mat_null(
        self,
        mid: int = None,
        ro: float = 0.0,
        pc: float = 0.0,
        mu: float = 0.0,
        terod: float = 0.0,
        cerod: float = 0.0,
        ym: float = 0.0,
        pr: float = 0.0,
    ) -> int:
        """Create a MAT_NULL keyword.

        Parameters
        ----------
        mid : int, optional
            Material ID. If not provided, auto-generates one.
        ro : float, optional
            Density. Default is 0.0.
        pc : float, optional
            Pressure cutoff. Default is 0.0.
        mu : float, optional
            Dynamic viscosity. Default is 0.0.
        terod : float, optional
            Erosion flag in tension. Default is 0.0.
        cerod : float, optional
            Erosion flag in compression. Default is 0.0.
        ym : float, optional
            Young's modulus. Default is 0.0.
        pr : float, optional
            Poisson's ratio. Default is 0.0.

        Returns
        -------
        int
            The material ID.
        """
        from ansys.dyna.core.keywords import keywords

        if mid is None:
            mid = self.next_id("material")

        kw = keywords.MatNull()
        kw.mid = mid
        kw.ro = ro
        kw.pc = pc
        kw.mu = mu
        kw.terod = terod
        kw.cerod = cerod
        kw.ym = ym
        kw.pr = pr

        self._deck.append(kw)
        logger.debug(f"Created MAT_NULL with mid={mid}")
        return mid

    def create_mat_high_explosive_burn(
        self,
        mid: int = None,
        ro: float = 0.0,
        d: float = 0.0,
        pcj: float = 0.0,
        beta: float = 0.0,
        k: float = 0.0,
        g: float = 0.0,
        sigy: float = 0.0,
    ) -> int:
        """Create a MAT_HIGH_EXPLOSIVE_BURN keyword.

        Parameters
        ----------
        mid : int, optional
            Material ID. If not provided, auto-generates one.
        ro : float, optional
            Density. Default is 0.0.
        d : float, optional
            Detonation velocity. Default is 0.0.
        pcj : float, optional
            Chapman-Jouget pressure. Default is 0.0.
        beta : float, optional
            Beta burn flag. Default is 0.0.
        k : float, optional
            Bulk modulus. Default is 0.0.
        g : float, optional
            Shear modulus. Default is 0.0.
        sigy : float, optional
            Yield stress. Default is 0.0.

        Returns
        -------
        int
            The material ID.
        """
        from ansys.dyna.core.keywords import keywords

        if mid is None:
            mid = self.next_id("material")

        kw = keywords.MatHighExplosiveBurn()
        kw.mid = mid
        kw.ro = ro
        kw.d = d
        kw.pcj = pcj
        kw.beta = beta
        kw.k = k
        kw.g = g
        kw.sigy = sigy

        self._deck.append(kw)
        logger.debug(f"Created MAT_HIGH_EXPLOSIVE_BURN with mid={mid}")
        return mid

    def create_mat_johnson_cook(
        self,
        mid: int = None,
        ro: float = 0.0,
        g: float = 0.0,
        e: float = 0.0,
        pr: float = 0.0,
        dtf: float = 0.0,
        vp: float = 0.0,
        rateop: float = 0.0,
        a: float = 0.0,
        b: float = 0.0,
        n: float = 0.0,
        c: float = 0.0,
        m: float = 0.0,
        tm: float = 0.0,
        tr: float = 0.0,
        epso: float = 0.0,
        cp: float = 0.0,
        pc: float = 0.0,
        spall: float = 0.0,
        it: float = 0.0,
        d1: float = 0.0,
        d2: float = 0.0,
        d3: float = 0.0,
        d4: float = 0.0,
        d5: float = 0.0,
        c2_or_erod: float = 0.0,
    ) -> int:
        """Create a MAT_JOHNSON_COOK keyword.

        Parameters
        ----------
        mid : int, optional
            Material ID. If not provided, auto-generates one.
        ro : float, optional
            Density. Default is 0.0.
        g : float, optional
            Shear modulus. Default is 0.0.
        e : float, optional
            Young's modulus. Default is 0.0.
        pr : float, optional
            Poisson's ratio. Default is 0.0.
        ... (other parameters with defaults)

        Returns
        -------
        int
            The material ID.
        """
        from ansys.dyna.core.keywords import keywords

        if mid is None:
            mid = self.next_id("material")

        kw = keywords.MatJohnsonCook()
        kw.mid = mid
        kw.ro = ro
        kw.g = g
        kw.e = e
        kw.pr = pr
        kw.dtf = dtf
        kw.vp = vp
        kw.rateop = rateop
        kw.a = a
        kw.b = b
        kw.n = n
        kw.c = c
        kw.m = m
        kw.tm = tm
        kw.tr = tr
        kw.epso = epso
        kw.cp = cp
        kw.pc = pc
        kw.spall = spall
        kw.it = it
        kw.d1 = d1
        kw.d2 = d2
        kw.d3 = d3
        kw.d4 = d4
        kw.d5 = d5
        kw.c2_p = c2_or_erod

        self._deck.append(kw)
        logger.debug(f"Created MAT_JOHNSON_COOK with mid={mid}")
        return mid

    def create_eos_linear_polynomial(
        self,
        eosid: int = None,
        c0: float = 0.0,
        c1: float = 0.0,
        c2: float = 0.0,
        c3: float = 0.0,
        c4: float = 0.0,
        c5: float = 0.0,
        c6: float = 0.0,
        e0: float = 0.0,
        v0: float = 0.0,
    ) -> int:
        """Create an EOS_LINEAR_POLYNOMIAL keyword.

        Parameters
        ----------
        eosid : int, optional
            EOS ID. If not provided, auto-generates one.
        c0-c6 : float, optional
            Polynomial coefficients. Default is 0.0.
        e0 : float, optional
            Initial internal energy. Default is 0.0.
        v0 : float, optional
            Initial relative volume. Default is 0.0.

        Returns
        -------
        int
            The EOS ID.
        """
        from ansys.dyna.core.keywords import keywords

        if eosid is None:
            eosid = self.next_id("eos")

        kw = keywords.EosLinearPolynomial()
        kw.eosid = eosid
        kw.c0 = c0
        kw.c1 = c1
        kw.c2 = c2
        kw.c3 = c3
        kw.c4 = c4
        kw.c5 = c5
        kw.c6 = c6
        kw.e0 = e0
        kw.v0 = v0

        self._deck.append(kw)
        logger.debug(f"Created EOS_LINEAR_POLYNOMIAL with eosid={eosid}")
        return eosid

    def create_eos_jwl(
        self,
        eosid: int = None,
        a: float = 0.0,
        b: float = 0.0,
        r1: float = 0.0,
        r2: float = 0.0,
        omeg: float = 0.0,
        e0: float = 0.0,
        vo: float = 0.0,
    ) -> int:
        """Create an EOS_JWL keyword.

        Parameters
        ----------
        eosid : int, optional
            EOS ID. If not provided, auto-generates one.
        a : float, optional
            JWL parameter A. Default is 0.0.
        b : float, optional
            JWL parameter B. Default is 0.0.
        r1 : float, optional
            JWL parameter R1. Default is 0.0.
        r2 : float, optional
            JWL parameter R2. Default is 0.0.
        omeg : float, optional
            JWL parameter omega. Default is 0.0.
        e0 : float, optional
            Initial internal energy. Default is 0.0.
        vo : float, optional
            Initial relative volume. Default is 0.0.

        Returns
        -------
        int
            The EOS ID.
        """
        from ansys.dyna.core.keywords import keywords

        if eosid is None:
            eosid = self.next_id("eos")

        kw = keywords.EosJwl()
        kw.eosid = eosid
        kw.a = a
        kw.b = b
        kw.r1 = r1
        kw.r2 = r2
        kw.omeg = omeg
        kw.e0 = e0
        kw.vo = vo

        self._deck.append(kw)
        logger.debug(f"Created EOS_JWL with eosid={eosid}")
        return eosid

    def create_eos_gruneisen(
        self,
        eosid: int = None,
        c: float = 0.0,
        s1: float = 0.0,
        s2: float = 0.0,
        s3: float = 0.0,
        gamao: float = 0.0,
        a: float = 0.0,
        e0: float = 0.0,
        v0: float = 0.0,
    ) -> int:
        """Create an EOS_GRUNEISEN keyword.

        Parameters
        ----------
        eosid : int, optional
            EOS ID. If not provided, auto-generates one.
        c : float, optional
            Intercept of shock velocity vs particle velocity curve. Default is 0.0.
        s1-s3 : float, optional
            Slope coefficients. Default is 0.0.
        gamao : float, optional
            Gruneisen coefficient. Default is 0.0.
        a : float, optional
            First order volume correction. Default is 0.0.
        e0 : float, optional
            Initial internal energy. Default is 0.0.
        v0 : float, optional
            Initial relative volume. Default is 0.0.

        Returns
        -------
        int
            The EOS ID.
        """
        from ansys.dyna.core.keywords import keywords

        if eosid is None:
            eosid = self.next_id("eos")

        kw = keywords.EosGruneisen()
        kw.eosid = eosid
        kw.c = c
        kw.s1 = s1
        kw.s2 = s2
        kw.s3 = s3
        kw.gamao = gamao
        kw.a = a
        kw.e0 = e0
        kw.v0 = v0

        self._deck.append(kw)
        logger.debug(f"Created EOS_GRUNEISEN with eosid={eosid}")
        return eosid

    def create_initial_detonation(
        self,
        pid: int,
        x: float = 0.0,
        y: float = 0.0,
        z: float = 0.0,
        lt: float = 0.0,
        mmgset: int = 0,
    ) -> bool:
        """Create an INITIAL_DETONATION keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        x : float, optional
            X-coordinate of detonation point. Default is 0.0.
        y : float, optional
            Y-coordinate of detonation point. Default is 0.0.
        z : float, optional
            Z-coordinate of detonation point. Default is 0.0.
        lt : float, optional
            Lighting time. Default is 0.0.
        mmgset : int, optional
            Multi-material group set. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.InitialDetonation()
        kw.pid = pid
        kw.x = x
        kw.y = y
        kw.z = z
        kw.lt = lt
        kw.mmgset = mmgset

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_DETONATION for pid={pid}")
        return True

    def create_ale_structured_mesh(
        self,
        mshid: int,
        dpid: int = 0,
        nbid: int = 0,
        ebid: int = 0,
        tdeath: float = 0.0,
        cpidx: int = 0,
        cpidy: int = 0,
        cpidz: int = 0,
        nid0: int = 0,
        lcsid: int = 0,
    ) -> bool:
        """Create an ALE_STRUCTURED_MESH keyword.

        Parameters
        ----------
        mshid : int
            Mesh ID.
        dpid : int, optional
            Domain part ID. Default is 0.
        nbid : int, optional
            Node base ID. Default is 0.
        ebid : int, optional
            Element base ID. Default is 0.
        tdeath : float, optional
            Death time. Default is 0.0.
        cpidx : int, optional
            Control point ID for X direction. Default is 0.
        cpidy : int, optional
            Control point ID for Y direction. Default is 0.
        cpidz : int, optional
            Control point ID for Z direction. Default is 0.
        nid0 : int, optional
            Reference node ID. Default is 0.
        lcsid : int, optional
            Load curve set ID. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.AleStructuredMesh()
        kw.mshid = mshid
        kw.dpid = dpid
        kw.nbid = nbid
        kw.ebid = ebid
        kw.tdeath = tdeath
        kw.cpidx = cpidx
        kw.cpidy = cpidy
        kw.cpidz = cpidz
        kw.nid0 = nid0
        kw.lcsid = lcsid

        self._deck.append(kw)
        logger.debug(f"Created ALE_STRUCTURED_MESH with mshid={mshid}")
        return True

    def create_ale_structured_mesh_control_points(
        self,
        cpid: int,
        icase: int = 2,
        sfo: float = 1.0,
        offo: float = 0.0,
        points: List[tuple] = None,
    ) -> bool:
        """Create an ALE_STRUCTURED_MESH_CONTROL_POINTS keyword.

        Parameters
        ----------
        cpid : int
            Control point ID.
        icase : int, optional
            Case type. Default is 2.
        sfo : float, optional
            Scale factor. Default is 1.0.
        offo : float, optional
            Offset. Default is 0.0.
        points : List[tuple], optional
            List of (n, x, ratio) tuples for control points.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        if points is None:
            points = []

        kw = keywords.AleStructuredMeshControlPoints()
        kw.cpid = cpid
        kw.icase = icase
        kw.sfo = sfo
        kw.offo = offo

        # Add control points via the table
        import pandas as pd

        if points:
            data = []
            for p in points:
                data.append({"n": p[0], "x": p[1], "ratio_xl": p[2]})
            kw.control_points = pd.DataFrame(data)

        self._deck.append(kw)
        logger.debug(f"Created ALE_STRUCTURED_MESH_CONTROL_POINTS with cpid={cpid}")
        return True

    def create_ale_structured_multi_material_group(
        self,
        groups: List[dict] = None,
    ) -> bool:
        """Create ALE_STRUCTURED_MULTI-MATERIAL_GROUP keyword(s).

        Creates one keyword instance per group entry. In LS-DYNA, multiple
        material groups are defined by repeating the keyword card.

        Parameters
        ----------
        groups : List[dict], optional
            List of dictionaries with keys: ammgnm, mid, eosid, pref.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        if groups is None:
            groups = []

        # Create one keyword per group entry
        for group in groups:
            kw = keywords.AleStructuredMulti_MaterialGroup()
            kw.ammg_name = group.get("ammgnm", "")
            kw.mid = group.get("mid")
            kw.eosid = group.get("eosid")
            kw.pref = group.get("pref", 0.0)
            self._deck.append(kw)

        logger.debug(f"Created {len(groups)} ALE_STRUCTURED_MULTI-MATERIAL_GROUP keywords")
        return True

    def create_ale_structured_mesh_volume_filling(
        self,
        mshid: int,
        ammgto: str = "",
        nsample: int = 0,
        vid: int = 0,
        geom: str = "ALL",
        in_out: int = 0,
        e1: float = 0.0,
        e2: float = 0.0,
        e3: float = 0.0,
        e4: float = 0.0,
        e5: float = 0.0,
    ) -> bool:
        """Create an ALE_STRUCTURED_MESH_VOLUME_FILLING keyword.

        Parameters
        ----------
        mshid : int
            Mesh ID.
        ammgto : str, optional
            Multi-material group name. Default is "".
        nsample : int, optional
            Number of samples. Default is 0.
        vid : int, optional
            Volume ID. Default is 0.
        geom : str, optional
            Geometry type (ALL, PART, etc.). Default is "ALL".
        in_out : int, optional
            Inside/outside flag. Default is 0.
        e1-e5 : float, optional
            Geometry parameters. For PART, e1 is the part ID.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.AleStructuredMeshVolumeFilling()
        kw.mshid = mshid
        kw.ammgto = ammgto
        kw.nsample = nsample
        kw.vid = vid
        kw.geom = geom
        kw.in_out = in_out
        kw.e1 = e1
        kw.e2 = e2
        kw.e3 = e3
        kw.e4 = e4
        kw.e5 = e5

        self._deck.append(kw)
        logger.debug(f"Created ALE_STRUCTURED_MESH_VOLUME_FILLING for mshid={mshid}, geom={geom}")
        return True

    def create_ale_structured_mesh_refine(
        self,
        mshid: int,
        ityp: int = 0,
        idir: int = 0,
        n1: int = 0,
        n2: int = 0,
        ntimes: int = 0,
    ) -> bool:
        """Create an ALE_STRUCTURED_MESH_REFINE keyword.

        Parameters
        ----------
        mshid : int
            Mesh ID.
        ityp : int, optional
            Refinement type. Default is 0.
        idir : int, optional
            Direction. Default is 0.
        n1 : int, optional
            First node/element. Default is 0.
        n2 : int, optional
            Second node/element. Default is 0.
        ntimes : int, optional
            Number of refinement times. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.AleStructuredMeshRefine()
        kw.mshid = mshid
        kw.ityp = ityp
        kw.idir = idir
        kw.n1 = n1
        kw.n2 = n2
        kw.ntimes = ntimes

        self._deck.append(kw)
        logger.debug(f"Created ALE_STRUCTURED_MESH_REFINE for mshid={mshid}")
        return True
