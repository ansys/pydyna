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

"""
ICFD Keywords Mixin
===================

Mixin class providing ICFD (Incompressible CFD) keyword functionality.
"""

import logging
from typing import List, Optional, Set

import pandas as pd

logger = logging.getLogger(__name__)


class ICFDKeywordsMixin:
    """Mixin class providing ICFD keyword creation methods."""

    def _get_icfd_sections_created(self) -> Set[int]:
        """Get the set of created ICFD section IDs (lazily initialized)."""
        if not hasattr(self, "_icfd_sections_created"):
            self._icfd_sections_created: Set[int] = set()
        return self._icfd_sections_created

    def create_icfd_control_time(
        self,
        ttm: float = 1e28,
        dt: float = 0.0,
        cfl: float = 1.0,
        lcidsf: int = 0,
        dtmin: float = 0.0,
        dtmax: float = 0.0,
        dtinit: float = 0.0,
        tdeath: float = 0.0,
    ) -> None:
        """Create ICFD_CONTROL_TIME keyword.

        Parameters
        ----------
        ttm : float, optional
            Total time of simulation for the fluid problem. Default is 1e28.
        dt : float, optional
            Time step for fluid problem. If 0, auto-computed. Default is 0.0.
        cfl : float, optional
            CFL number for time step calculation. Default is 1.0.
        lcidsf : int, optional
            Load curve ID for scaling factor. Default is 0.
        dtmin : float, optional
            Minimum time step. Default is 0.0.
        dtmax : float, optional
            Maximum time step. Default is 0.0.
        dtinit : float, optional
            Initial time step. Default is 0.0.
        tdeath : float, optional
            Death time for ICFD solver. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_CONTROL_TIME: ttm={ttm}, dt={dt}, cfl={cfl}, "
            f"lcidsf={lcidsf}, dtmin={dtmin}, dtmax={dtmax}, dtinit={dtinit}, tdeath={tdeath}"
        )

        kw = keywords.IcfdControlTime()
        kw.ttm = ttm
        kw.dt = dt
        kw.cfl = cfl
        kw.lcidsf = lcidsf
        kw.dtmin = dtmin
        kw.dtmax = dtmax
        kw.dtinit = dtinit
        kw.tdeath = tdeath

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_TIME keyword")

    def create_icfd_section(self, sid: int) -> None:
        """Create ICFD_SECTION keyword.

        Skips creation if a section with the same sid has already been created.

        Parameters
        ----------
        sid : int
            Section identification.
        """
        # Check if this section already exists to avoid duplicates
        created = self._get_icfd_sections_created()
        if sid in created:
            logger.debug(f"ICFD_SECTION with sid={sid} already exists, skipping")
            return

        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_SECTION: sid={sid}")

        kw = keywords.IcfdSection()
        kw.sid = sid

        self._deck.append(kw)
        created.add(sid)
        logger.info(f"Created ICFD_SECTION keyword with sid={sid}")

    def create_icfd_part_vol(
        self,
        pid: int,
        secid: int,
        mid: int = 0,
        spids: Optional[List[int]] = None,
    ) -> int:
        """Create ICFD_PART_VOL keyword.

        Parameters
        ----------
        pid : int
            Part identification for volume.
        secid : int
            Section ID defined in *ICFD_SECTION.
        mid : int, optional
            Material ID. Default is 0.
        spids : list of int, optional
            Surface part IDs that define the volume. Default is None.

        Returns
        -------
        int
            The part ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_PART_VOL: pid={pid}, secid={secid}, mid={mid}, spids={spids}")

        kw = keywords.IcfdPartVol()
        kw.pid = pid
        kw.secid = secid
        kw.mid = mid

        # Set the surface part IDs in the table card
        if spids:
            # Pad the list to 8 elements
            padded_spids = spids + [0] * (8 - len(spids)) if len(spids) < 8 else spids[:8]
            df = pd.DataFrame(
                [padded_spids],
                columns=["spid1", "spid2", "spid3", "spid4", "spid5", "spid6", "spid7", "spid8"],
            )
            kw.nodes = df

        self._deck.append(kw)
        logger.info(f"Created ICFD_PART_VOL keyword with pid={pid}")
        return pid

    def create_icfd_control_general(
        self,
        atype: int = 0,
        mtype: int = 0,
        dvcl: int = 0,
        rdvcl: int = 0,
    ) -> None:
        """Create ICFD_CONTROL_GENERAL keyword.

        Parameters
        ----------
        atype : int, optional
            Analysis type. 0=Transient, 1=Steady state, -1=Turn off ICFD. Default is 0.
        mtype : int, optional
            Solving method type. Default is 0.
        dvcl : int, optional
            Flag for divergence cleaning. Default is 0.
        rdvcl : int, optional
            Flag for remeshing divergence cleaning. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_CONTROL_GENERAL: atype={atype}, mtype={mtype}, "
            f"dvcl={dvcl}, rdvcl={rdvcl}"
        )

        kw = keywords.IcfdControlGeneral()
        kw.atype = atype
        kw.mtype = mtype
        kw.dvcl = dvcl
        kw.rdvcl = rdvcl

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_GENERAL keyword")

    def create_icfd_control_output(
        self,
        msgl: int = 0,
        itout: int = 0,
    ) -> None:
        """Create ICFD_CONTROL_OUTPUT keyword.

        Parameters
        ----------
        msgl : int, optional
            Message level. 0=timestep info, 4=full output. Default is 0.
        itout : int, optional
            Iteration interval to print output. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_CONTROL_OUTPUT: msgl={msgl}, itout={itout}")

        kw = keywords.IcfdControlOutput()
        kw.msgl = msgl
        kw.itout = itout
        # Leave lsppout at default (None → blank in output)

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_OUTPUT keyword")

    def create_icfd_control_steady(
        self,
        its: int = 1000000,
        tol1: float = 0.001,
        tol2: float = 0.001,
        tol3: float = 0.001,
        rel1: float = 0.3,
        rel2: float = 0.7,
    ) -> None:
        """Create ICFD_CONTROL_STEADY keyword.

        Parameters
        ----------
        its : int, optional
            Maximum number of iterations. Default is 1000000.
        tol1 : float, optional
            Momentum tolerance limit. Default is 0.001.
        tol2 : float, optional
            Pressure tolerance limit. Default is 0.001.
        tol3 : float, optional
            Temperature tolerance limit. Default is 0.001.
        rel1 : float, optional
            Velocity relaxation parameter. Default is 0.3.
        rel2 : float, optional
            Pressure relaxation parameter. Default is 0.7.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_CONTROL_STEADY: its={its}, tol1={tol1}, tol2={tol2}, "
            f"tol3={tol3}, rel1={rel1}, rel2={rel2}"
        )

        kw = keywords.IcfdControlSteady()
        kw.its = its
        kw.tol1 = tol1
        kw.tol2 = tol2
        kw.tol3 = tol3
        kw.rel1 = rel1
        kw.rel2 = rel2

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_STEADY keyword")

    def create_icfd_mat(
        self,
        mid: int,
        flg: int = 1,
        ro: float = 0.0,
        vis: float = 0.0,
        hc: float = 0.0,
        tc: float = 0.0,
        beta: float = 0.0,
        prt: float = 0.0,
    ) -> int:
        """Create ICFD_MAT keyword.

        Parameters
        ----------
        mid : int
            Material identification.
        flg : int, optional
            Flag for compressibility. 1=fully incompressible. Default is 1.
        ro : float, optional
            Flow density. Default is 0.0.
        vis : float, optional
            Dynamic viscosity. Default is 0.0.
        hc : float, optional
            Heat capacity. Default is 0.0.
        tc : float, optional
            Thermal conductivity. Default is 0.0.
        beta : float, optional
            Thermal expansion coefficient. Default is 0.0.
        prt : float, optional
            Prandtl number. Default is 0.0.

        Returns
        -------
        int
            The material ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_MAT: mid={mid}, flg={flg}, ro={ro}, vis={vis}, hc={hc}, tc={tc}, beta={beta}, prt={prt}")

        kw = keywords.IcfdMat()
        kw.mid = mid
        kw.flg = flg
        kw.ro = ro
        kw.vis = vis
        kw.hc = hc
        kw.tc = tc
        kw.beta = beta
        kw.prt = prt

        self._deck.append(kw)
        logger.info(f"Created ICFD_MAT keyword with mid={mid}")
        return mid

    def create_icfd_part(
        self,
        pid: int,
        secid: int,
        mid: int = 0,
    ) -> int:
        """Create ICFD_PART keyword.

        Parameters
        ----------
        pid : int
            Part identification.
        secid : int
            Section ID from *ICFD_SECTION.
        mid : int, optional
            Material ID from *ICFD_MAT. Default is 0.

        Returns
        -------
        int
            The part ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_PART: pid={pid}, secid={secid}, mid={mid}")

        kw = keywords.IcfdPart()
        # ICFD_PART uses a table card for pid/secid/mid
        df = pd.DataFrame(
            [[pid, secid, mid]],
            columns=["pid", "secid", "mid"],
        )
        kw.nodes = df

        self._deck.append(kw)
        logger.info(f"Created ICFD_PART keyword with pid={pid}")
        return pid

    def set_icfd_part_property(
        self,
        pid: int,
        secid: int,
        mid: int,
    ) -> None:
        """Update ICFD_PART with section and material IDs.

        Finds the existing ICFD_PART with the given pid and updates its secid and mid.

        Parameters
        ----------
        pid : int
            Part identification to update.
        secid : int
            Section ID from *ICFD_SECTION.
        mid : int
            Material ID from *ICFD_MAT.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Setting ICFD_PART property: pid={pid}, secid={secid}, mid={mid}")

        # Find and update the ICFD_PART with this pid
        for kw in self._deck:
            if isinstance(kw, keywords.IcfdPart):
                df = kw.nodes
                if len(df) > 0 and df.iloc[0]["pid"] == pid:
                    df.iloc[0, df.columns.get_loc("secid")] = secid
                    df.iloc[0, df.columns.get_loc("mid")] = mid
                    kw.nodes = df
                    logger.info(f"Updated ICFD_PART pid={pid} with secid={secid}, mid={mid}")
                    return

        logger.warning(f"ICFD_PART with pid={pid} not found, cannot update property")

    def create_icfd_boundary_nonslip(self, pid: int) -> None:
        """Create ICFD_BOUNDARY_NONSLIP keyword.

        Parameters
        ----------
        pid : int
            Part identification for the non-slip boundary.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_BOUNDARY_NONSLIP: pid={pid}")

        kw = keywords.IcfdBoundaryNonslip()
        kw.pid = pid

        self._deck.append(kw)
        logger.info(f"Created ICFD_BOUNDARY_NONSLIP keyword with pid={pid}")

    def create_icfd_boundary_prescribed_vel(
        self,
        pid: int,
        dof: int = 1,
        vad: int = 1,
        lcid: int = 0,
        sf: float = 1.0,
        death: float = 0.0,
        birth: float = 0.0,
    ) -> None:
        """Create ICFD_BOUNDARY_PRESCRIBED_VEL keyword.

        Parameters
        ----------
        pid : int
            Part identification for the prescribed velocity boundary.
        dof : int, optional
            Degree of freedom. 1=x, 2=y, 3=z, 4=normal. Default is 1.
        vad : int, optional
            Velocity flag. 1=linear, 2=angular. Default is 1.
        lcid : int, optional
            Load curve ID. Default is 0.
        sf : float, optional
            Scale factor. Default is 1.0.
        death : float, optional
            Death time. Default is 0.0.
        birth : float, optional
            Birth time. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_BOUNDARY_PRESCRIBED_VEL: pid={pid}, dof={dof}, vad={vad}, "
            f"lcid={lcid}, sf={sf}, death={death}, birth={birth}"
        )

        kw = keywords.IcfdBoundaryPrescribedVel()
        kw.pid = pid
        kw.dof = dof
        kw.vad = vad
        kw.lcid = lcid
        kw.sf = sf
        kw.death = death
        kw.birth = birth

        self._deck.append(kw)
        logger.info(f"Created ICFD_BOUNDARY_PRESCRIBED_VEL keyword with pid={pid}")

    def create_icfd_boundary_freeslip(self, pid: int) -> None:
        """Create ICFD_BOUNDARY_FREESLIP keyword.

        Parameters
        ----------
        pid : int
            Part identification for the free-slip boundary.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_BOUNDARY_FREESLIP: pid={pid}")

        kw = keywords.IcfdBoundaryFreeslip()
        kw.boundaries = pd.DataFrame({"pid": [pid]})

        self._deck.append(kw)
        logger.info(f"Created ICFD_BOUNDARY_FREESLIP keyword with pid={pid}")

    def create_icfd_boundary_prescribed_pre(
        self,
        pid: int,
        lcid: int = 0,
        sf: float = 0.0,
        death: float = 0.0,
        birth: float = 0.0,
    ) -> None:
        """Create ICFD_BOUNDARY_PRESCRIBED_PRE keyword.

        Parameters
        ----------
        pid : int
            Part identification for the prescribed pressure boundary.
        lcid : int, optional
            Load curve ID for pressure. Default is 0.
        sf : float, optional
            Scale factor for pressure. Default is 0.0.
        death : float, optional
            Death time. Default is 0.0.
        birth : float, optional
            Birth time. Default is 0.0.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_BOUNDARY_PRESCRIBED_PRE: pid={pid}, lcid={lcid}, "
            f"sf={sf}, death={death}, birth={birth}"
        )

        kw = keywords.IcfdBoundaryPrescribedPre()
        kw.boundaries = pd.DataFrame(
            {"pid": [pid], "lcid": [lcid], "sf": [sf], "death": [death], "birth": [birth]}
        )

        self._deck.append(kw)
        logger.info(f"Created ICFD_BOUNDARY_PRESCRIBED_PRE keyword with pid={pid}")

    def create_icfd_control_adapt(
        self,
        minh: float = 0.0,
        maxh: float = 0.0,
        err: float = 1.0,
        nit: int = 0,
    ) -> None:
        """Create ICFD_CONTROL_ADAPT keyword.

        Parameters
        ----------
        minh : float, optional
            Minimum mesh size. Default is 0.0.
        maxh : float, optional
            Maximum mesh size. Default is 0.0.
        err : float, optional
            Maximum perceptual error. Default is 1.0.
        nit : int, optional
            Number of iterations before forced remeshing. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_CONTROL_ADAPT: minh={minh}, maxh={maxh}, err={err}, nit={nit}")

        kw = keywords.IcfdControlAdapt()
        kw.minh = minh
        kw.maxh = maxh
        kw.err = err
        kw.nit = nit

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_ADAPT keyword")

    def create_icfd_database_drag(self, pid: int) -> None:
        """Create ICFD_DATABASE_DRAG keyword.

        Parameters
        ----------
        pid : int
            Part identification for drag computation.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_DATABASE_DRAG: pid={pid}")

        kw = keywords.IcfdDatabaseDrag()
        kw.pid = pid
        # Don't set divi - leave as None (reference files have it blank)
        kw.divi = None

        self._deck.append(kw)
        logger.info(f"Created ICFD_DATABASE_DRAG keyword with pid={pid}")

    def create_icfd_database_temp(self, pid: int) -> None:
        """Create ICFD_DATABASE_TEMP keyword.

        Parameters
        ----------
        pid : int
            Part identification for temperature database output.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_DATABASE_TEMP: pid={pid}")

        kw = keywords.IcfdDatabaseTemp()
        kw.pid = pid

        self._deck.append(kw)
        logger.info(f"Created ICFD_DATABASE_TEMP keyword with pid={pid}")

    def create_icfd_boundary_prescribed_temp(
        self,
        pid: int,
        lcid: int = 0,
        sf: float = 0.0,
        death: float = 0.0,
        birth: float = 0.0,
    ) -> None:
        """Create ICFD_BOUNDARY_PRESCRIBED_TEMP keyword.

        Parameters
        ----------
        pid : int
            Part identification for the prescribed temperature boundary.
        lcid : int, optional
            Load curve ID for temperature. Default is 0.
        sf : float, optional
            Scale factor for temperature. Default is 0.0.
        death : float, optional
            Death time. Default is 0.0.
        birth : float, optional
            Birth time. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_BOUNDARY_PRESCRIBED_TEMP: pid={pid}, lcid={lcid}, "
            f"sf={sf}, death={death}, birth={birth}"
        )

        kw = keywords.IcfdBoundaryPrescribedTemp()
        kw.pid = pid
        kw.lcid = lcid
        kw.sf = sf
        kw.death = death
        kw.birth = birth

        self._deck.append(kw)
        logger.info(f"Created ICFD_BOUNDARY_PRESCRIBED_TEMP keyword with pid={pid}")

    def create_mesh_bl(
        self,
        pid: int,
        nelth: int = 0,
    ) -> None:
        """Create MESH_BL keyword for boundary layer mesh.

        Parameters
        ----------
        pid : int
            Part identification for boundary layer.
        nelth : int, optional
            Number of elements in boundary layer minus one. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_BL: pid={pid}, nelth={nelth}")

        kw = keywords.MeshBl()
        kw.pid = pid
        kw.nelth = nelth

        self._deck.append(kw)
        logger.info(f"Created MESH_BL keyword with pid={pid}")

    def create_mesh_volume(
        self,
        volid: int,
        pids: Optional[List[int]] = None,
    ) -> int:
        """Create MESH_VOLUME keyword.

        Parameters
        ----------
        volid : int
            ID assigned to the new volume.
        pids : list of int, optional
            Surface element/part IDs. Default is None.

        Returns
        -------
        int
            The volume ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_VOLUME: volid={volid}, pids={pids}")

        kw = keywords.MeshVolume()
        kw.volid = volid

        # Set the surface part IDs using the elements property (SeriesCard)
        if pids:
            kw.elements = pids

        self._deck.append(kw)
        logger.info(f"Created MESH_VOLUME keyword with volid={volid}")
        return volid

    def create_mesh_size(
        self,
        volid: int,
        pids: Optional[List[int]] = None,
    ) -> None:
        """Create MESH_SIZE keyword.

        Parameters
        ----------
        volid : int
            ID of the volume (from MESH_VOLUME) to apply the size meshes to.
        pids : list of int, optional
            Part IDs for the surface mesh that define local mesh sizes.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_SIZE: volid={volid}, pids={pids}")

        kw = keywords.MeshSize()
        kw.volid = volid

        # Set up to 8 PIDs
        if pids:
            for i, pid in enumerate(pids[:8]):
                setattr(kw, f"pid{i+1}", pid)

        self._deck.append(kw)
        logger.info(f"Created MESH_SIZE keyword with volid={volid}")

    def create_icfd_control_dem_coupling(
        self,
        ctype: int = 0,
        bt: float = 0.0,
        dt: float = 1e28,
        sf: float = 1.0,
        form: int = 0,
    ) -> None:
        """Create ICFD_CONTROL_DEM_COUPLING keyword.

        Parameters
        ----------
        ctype : int, optional
            Coupling direction to the solver. Default is 0.
        bt : float, optional
            Birth time for DEM coupling. Default is 0.0.
        dt : float, optional
            Death time for DEM coupling. Default is 1e28.
        sf : float, optional
            Scale factor for force transmitted by fluid. Default is 1.0.
        form : int, optional
            Formulation type. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_CONTROL_DEM_COUPLING: ctype={ctype}, bt={bt}, dt={dt}, sf={sf}, form={form}")

        kw = keywords.IcfdControlDemCoupling()
        kw.ctype = ctype
        kw.bt = bt
        kw.dt = dt
        kw.sf = sf
        # Note: 'form' field may not exist in keyword class, check if available
        if hasattr(kw, 'form'):
            kw.form = form

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_DEM_COUPLING keyword")

    def create_icfd_initial(
        self,
        pid: int = 0,
        vx: float = 0.0,
        vy: float = 0.0,
        vz: float = 0.0,
        t: float = 0.0,
        p: float = 0.0,
    ) -> None:
        """Create ICFD_INITIAL keyword.

        Parameters
        ----------
        pid : int, optional
            Part ID to apply initial conditions. 0 for all parts. Default is 0.
        vx : float, optional
            Initial velocity in x direction. Default is 0.0.
        vy : float, optional
            Initial velocity in y direction. Default is 0.0.
        vz : float, optional
            Initial velocity in z direction. Default is 0.0.
        t : float, optional
            Initial temperature. Default is 0.0.
        p : float, optional
            Initial pressure. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_INITIAL: pid={pid}, vx={vx}, vy={vy}, vz={vz}, t={t}, p={p}")

        kw = keywords.IcfdInitial()
        kw.pid = pid
        kw.vx = vx
        kw.vy = vy
        kw.vz = vz
        kw.t = t
        kw.p = p

        self._deck.append(kw)
        logger.info("Created ICFD_INITIAL keyword")

    def create_icfd_control_mesh(
        self,
        mgsf: float = 1.41,
        mstrat: int = 0,
        struct2d: int = 0,
        nrmsh: int = 0,
        aver: int = 14,
    ) -> None:
        """Create ICFD_CONTROL_MESH keyword.

        Parameters
        ----------
        mgsf : float, optional
            Mesh Growth Scale Factor. Values between 1 and 2 are allowed.
            Default is 1.41.
        mstrat : int, optional
            Mesh strategy. Default is 0.
        struct2d : int, optional
            2D structured mesh flag. Default is 0.
        nrmsh : int, optional
            Number of remeshing operations. Default is 0.
        aver : int, optional
            Averaging parameter. Default is 14.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_CONTROL_MESH: mgsf={mgsf}, mstrat={mstrat}, struct2d={struct2d}, nrmsh={nrmsh}, aver={aver}")

        kw = keywords.IcfdControlMesh()
        kw.mgsf = mgsf
        kw.mstrat = mstrat
        kw.twodstruc = struct2d
        kw.nrmsh = nrmsh
        kw.aver = aver

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_MESH keyword")

    def create_icfd_control_surfmesh(
        self,
        rsrf: int = 0,
        sadapt: int = 0,
    ) -> None:
        """Create ICFD_CONTROL_SURFMESH keyword.

        Parameters
        ----------
        rsrf : int, optional
            Remesh surface flag. Default is 0.
        sadapt : int, optional
            Surface adaptation flag. Default is 0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_CONTROL_SURFMESH: rsrf={rsrf}, sadapt={sadapt}")

        kw = keywords.IcfdControlSurfmesh()
        kw.rsrf = rsrf
        kw.sadapt = sadapt

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_SURFMESH keyword")

    def create_icfd_database_flux(self, pid: int, dtout: float = 0.0) -> None:
        """Create ICFD_DATABASE_FLUX keyword.

        Parameters
        ----------
        pid : int
            Part ID for flux output.
        dtout : float, optional
            Time interval for flux output. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_DATABASE_FLUX: pid={pid}, dtout={dtout}")

        kw = keywords.IcfdDatabaseFlux()
        # IcfdDatabaseFlux uses a TableCard for boundaries
        boundaries_data = pd.DataFrame([{"pid": pid, "dtout": dtout}])
        kw.boundaries = boundaries_data

        self._deck.append(kw)
        logger.info(f"Created ICFD_DATABASE_FLUX keyword for pid={pid}")

    def create_mesh_bl_sym(self, pid: int) -> None:
        """Create MESH_BL_SYM keyword.

        Parameters
        ----------
        pid : int
            Part ID for boundary layer symmetry surface.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_BL_SYM: pid={pid}")

        kw = keywords.MeshBlSym()
        kw.pid1 = pid

        self._deck.append(kw)
        logger.info(f"Created MESH_BL_SYM keyword for pid={pid}")

    def create_mesh_size_shape(
        self,
        sname: str = "BOX",
        force: int = 1,
        method: int = 0,
        msize: float = 0.0,
        parameter: Optional[List[float]] = None,
    ) -> None:
        """Create MESH_SIZE_SHAPE keyword.

        Parameters
        ----------
        sname : str, optional
            Shape name (BOX, SPHERE, CYLINDER). Default is "BOX".
        force : int, optional
            Flag to force mesh size. Default is 1.
        method : int, optional
            Meshing method. Default is 0.
        msize : float, optional
            Mesh size. Default is 0.0.
        parameter : list of float, optional
            Parameters defining the shape. For BOX: [pminx, pminy, pminz, pmaxx, pmaxy, pmaxz].
            Default is None.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_SIZE_SHAPE: sname={sname}, msize={msize}, parameter={parameter}")

        kw = keywords.MeshSizeShape()
        kw.sname = sname
        kw.force = force
        kw.method = method
        kw.bt = 0.0
        kw.dt = 0.0

        # Set msize and parameters based on shape type
        if sname == "BOX" and parameter and len(parameter) >= 6:
            kw.msize = msize
            kw.pminx = parameter[0]
            kw.pminy = parameter[1]
            kw.pminz = parameter[2]
            kw.pmaxx = parameter[3]
            kw.pmaxy = parameter[4]
            kw.pmaxz = parameter[5]
        else:
            kw.msize = msize

        self._deck.append(kw)
        logger.info(f"Created MESH_SIZE_SHAPE keyword with sname={sname}")

    def create_mesh_embed_shell(
        self,
        volid: int,
        pids: Optional[List[int]] = None,
    ) -> None:
        """Create MESH_EMBEDSHELL keyword.

        Parameters
        ----------
        volid : int
            Volume ID to embed shells into.
        pids : list of int, optional
            Part IDs of shells to embed. Default is None.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_EMBEDSHELL: volid={volid}, pids={pids}")

        kw = keywords.MeshEmbedshell()
        kw.volid = volid
        if pids:
            # Set pids using appropriate property
            if hasattr(kw, 'pids'):
                kw.pids = pids

        self._deck.append(kw)
        logger.info(f"Created MESH_EMBEDSHELL keyword with volid={volid}")
