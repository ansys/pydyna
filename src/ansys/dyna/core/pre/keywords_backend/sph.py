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

"""SPH (Smoothed Particle Hydrodynamics) keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class SPHKeywordsMixin:
    """Mixin class providing SPH keyword creation methods."""

    def create_control_sph(
        self,
        ncbs: int = 1,
        boxid: int = 0,
        dt: float = 1e20,
        idim: int = 3,
        nmneigh: int = 150,
        form: int = 0,
        start: float = 0.0,
        maxv: float = 1e15,
        cont: int = 0,
        deriv: int = 0,
        ini: int = 0,
        ishow: int = 0,
        ierod: int = 0,
        icont: int = 0,
        iavis: int = 0,
        isymp: int = 0,
    ) -> bool:
        """Create a CONTROL_SPH keyword.

        Parameters
        ----------
        ncbs : int
            Number of cycles between particle sorting.
        boxid : int
            Box ID for including particles.
        dt : float
            Death time for particles.
        idim : int
            Dimension (2 or 3).
        nmneigh : int
            Maximum number of neighbors.
        form : int
            Particle approximation.
        start : float
            Start time.
        maxv : float
            Maximum velocity.
        cont : int
            Contact algorithm.
        deriv : int
            Derivative method.
        ini : int
            Initialization method.
        ishow : int
            Show particles in d3plot.
        ierod : int
            Erosion control.
        icont : int
            Contact type.
        iavis : int
            Artificial viscosity.
        isymp : int
            Symmetry plane control.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlSph()
        kw.ncbs = ncbs
        kw.boxid = boxid
        kw.dt = dt
        kw.idim = idim
        kw.nmneigh = nmneigh
        kw.form = form
        kw.start = start
        kw.maxv = maxv
        kw.cont = cont
        kw.deriv = deriv
        kw.ini = ini
        kw.ishow = ishow
        kw.ierod = ierod
        kw.icont = icont
        kw.iavis = iavis
        kw.isymp = isymp

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_SPH with ncbs={ncbs}, boxid={boxid}")
        return True

    def create_database_sphmassflow(
        self,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 0,
    ) -> bool:
        """Create a DATABASE_SPHMASSFLOW keyword.

        Parameters
        ----------
        dt : float
            Time interval between outputs.
        binary : int
            Flag for binary output.
        lcur : int
            Load curve ID.
        ioopt : int
            Option for output.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DatabaseSphmassflow()
        kw.dt = dt
        kw.binary = binary
        kw.lcur = lcur
        kw.ioopt = ioopt

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_SPHMASSFLOW with dt={dt}")
        return True

    def create_section_sph(
        self,
        secid: int,
        cslh: float = 1.0,
        hmin: float = 0.2,
        hmax: float = 2.0,
        sphini: float = 0.0,
        death: float = 0.0,
        start: float = 0.0,
        sphkern: int = 0,
    ) -> bool:
        """Create a SECTION_SPH keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        cslh : float
            Constant for smoothing length calculation.
        hmin : float
            Minimum smoothing length scale factor.
        hmax : float
            Maximum smoothing length scale factor.
        sphini : float
            Initial smoothing length (or inter-particle distance).
        death : float
            Death time.
        start : float
            Start time.
        sphkern : int
            SPH kernel type.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SectionSph()
        kw.secid = secid
        kw.cslh = cslh
        kw.hmin = hmin
        kw.hmax = hmax
        kw.sphini = sphini
        kw.death = death
        kw.start = start
        kw.sphkern = sphkern

        self._deck.append(kw)
        logger.debug(f"Created SECTION_SPH with secid={secid}, sphini={sphini}")
        return True

    def create_mat_sph_incompressible_fluid(
        self,
        mid: int,
        ro: float = 0.0,
        mu: float = 0.0,
        gamma1: float = 0.0,
        gamma2: float = 0.0,
        stens: float = 0.0,
    ) -> bool:
        """Create a MAT_SPH_INCOMPRESSIBLE_FLUID keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        mu : float
            Dynamic viscosity.
        gamma1 : float
            Surface tension coefficient 1.
        gamma2 : float
            Surface tension coefficient 2.
        stens : float
            Surface tension.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatSphIncompressibleFluid()
        kw.mid = mid
        kw.ro = ro
        kw.mu = mu
        kw.gamma1 = gamma1
        kw.gamma2 = gamma2
        kw.stens = stens

        self._deck.append(kw)
        logger.debug(f"Created MAT_SPH_INCOMPRESSIBLE_FLUID with mid={mid}")
        return True

    def create_mat_sph_incompressible_structure(
        self,
        mid: int,
        ro: float = 0.0,
        beta: float = 0.0,
        rough: float = 0.0,
        adh: float = 0.0,
    ) -> bool:
        """Create a MAT_SPH_INCOMPRESSIBLE_STRUCTURE keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        beta : float
            Slip coefficient.
        rough : float
            Roughness.
        adh : float
            Adhesion.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatSphIncompressibleStructure()
        kw.mid = mid
        kw.ro = ro
        kw.beta = beta
        kw.rough = rough
        kw.adh = adh

        self._deck.append(kw)
        logger.debug(f"Created MAT_SPH_INCOMPRESSIBLE_STRUCTURE with mid={mid}")
        return True

    def create_define_box(
        self,
        boxid: int,
        xmn: float = 0.0,
        xmx: float = 0.0,
        ymn: float = 0.0,
        ymx: float = 0.0,
        zmn: float = 0.0,
        zmx: float = 0.0,
    ) -> bool:
        """Create a DEFINE_BOX keyword.

        Parameters
        ----------
        boxid : int
            Box ID.
        xmn, xmx : float
            X min/max coordinates.
        ymn, ymx : float
            Y min/max coordinates.
        zmn, zmx : float
            Z min/max coordinates.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DefineBox()
        kw.boxid = boxid
        kw.xmn = xmn
        kw.xmx = xmx
        kw.ymn = ymn
        kw.ymx = ymx
        kw.zmn = zmn
        kw.zmx = zmx

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_BOX with boxid={boxid}")
        return True

    def create_define_sph_mesh_surface(
        self,
        sid: int,
        type_: int = 0,
        sphpid: int = 0,
        sphxid: int = 0,
        nsid: int = 0,
        space: float = 0.0,
        iout: int = 0,
    ) -> bool:
        """Create a DEFINE_SPH_MESH_SURFACE keyword.

        Parameters
        ----------
        sid : int
            Part set ID or segment set ID.
        type_ : int
            Type of mesh (0=segment set, 1=part set).
        sphpid : int
            SPH part ID.
        sphxid : int
            SPH section ID.
        nsid : int
            Node set ID.
        space : float
            Spacing between particles.
        iout : int
            Output flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DefineSphMeshSurface()
        kw.sid = sid
        kw.type = type_
        kw.sphpid = sphpid
        kw.sphxid = sphxid
        kw.nsid = nsid
        kw.space = space
        kw.iout = iout

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_SPH_MESH_SURFACE with sid={sid}, sphpid={sphpid}")
        return True

    def create_define_sph_mesh_box(
        self,
        xmin: float,
        ymin: float,
        zmin: float,
        xlen: float,
        ylen: float,
        zlen: float,
        ipid: int,
        nx: int,
        ny: int,
        nz: int,
        idseg: int = 0,
        sfsp: float = 0.0,
    ) -> bool:
        """Create a DEFINE_SPH_MESH_BOX keyword.

        Parameters
        ----------
        xmin, ymin, zmin : float
            Minimum coordinates.
        xlen, ylen, zlen : float
            Dimensions in each direction.
        ipid : int
            Part ID for SPH elements.
        nx, ny, nz : int
            Number of particles in each direction.
        idseg : int
            Segment set ID.
        sfsp : float
            Scale factor for spacing.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DefineSphMeshBox()
        kw.xmin = xmin
        kw.ymin = ymin
        kw.zmin = zmin
        kw.xlen = xlen
        kw.ylen = ylen
        kw.zlen = zlen
        kw.ipid = ipid
        kw.nx = nx
        kw.ny = ny
        kw.nz = nz
        kw.idseg = idseg
        kw.sfsp = sfsp

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_SPH_MESH_BOX with ipid={ipid}")
        return True

    def create_define_sph_massflow_plane(
        self,
        prtclsid: int,
        surfsid: int,
        ptype: int = 0,
        stype: int = 0,
    ) -> bool:
        """Create a DEFINE_SPH_MASSFLOW_PLANE keyword.

        Parameters
        ----------
        prtclsid : int
            Particle set ID.
        surfsid : int
            Surface set ID.
        ptype : int
            Particle type.
        stype : int
            Surface type.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DefineSphMassflowPlane()
        kw.prtclsid = prtclsid
        kw.surfsid = surfsid
        kw.ptype = ptype
        kw.stype = stype

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_SPH_MASSFLOW_PLANE with prtclsid={prtclsid}")
        return True

    def create_load_body_z(
        self,
        lcid: int,
        sf: float = 1.0,
        lciddr: int = 0,
        xc: float = 0.0,
        yc: float = 0.0,
        zc: float = 0.0,
        cid: int = 0,
    ) -> bool:
        """Create a LOAD_BODY_Z keyword.

        Parameters
        ----------
        lcid : int
            Load curve ID.
        sf : float
            Scale factor.
        lciddr : int
            Load curve ID for dynamic relaxation.
        xc, yc, zc : float
            Center of rotation.
        cid : int
            Coordinate system ID.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.LoadBodyZ()
        kw.lcid = lcid
        kw.sf = sf
        kw.lciddr = lciddr
        kw.xc = xc
        kw.yc = yc
        kw.zc = zc
        kw.cid = cid

        self._deck.append(kw)
        logger.debug(f"Created LOAD_BODY_Z with lcid={lcid}")
        return True

    def create_constrained_rigid_bodies(
        self,
        pidl: int,
        pidc: int,
        iflag: int = 0,
    ) -> bool:
        """Create a CONSTRAINED_RIGID_BODIES keyword.

        Parameters
        ----------
        pidl : int
            Lead (master) rigid body part ID.
        pidc : int
            Constrained (slave) rigid body part ID.
        iflag : int
            Flag for merge type.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ConstrainedRigidBodies()
        kw.pidl = pidl
        kw.pidc = pidc
        kw.iflag = iflag

        self._deck.append(kw)
        logger.debug(f"Created CONSTRAINED_RIGID_BODIES with pidl={pidl}, pidc={pidc}")
        return True

    def create_part_inertia(
        self,
        pid: int,
        secid: int,
        mid: int,
        xc: float = 0.0,
        yc: float = 0.0,
        zc: float = 0.0,
        tm: float = 0.0,
        ircs: int = 0,
        nodeid: int = 0,
        ixx: float = 0.0,
        ixy: float = 0.0,
        ixz: float = 0.0,
        iyy: float = 0.0,
        iyz: float = 0.0,
        izz: float = 0.0,
        vtx: float = 0.0,
        vty: float = 0.0,
        vtz: float = 0.0,
        vrx: float = 0.0,
        vry: float = 0.0,
        vrz: float = 0.0,
        title: str = "",
    ) -> bool:
        """Create or update a PART_INERTIA keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        secid : int
            Section ID.
        mid : int
            Material ID.
        xc, yc, zc : float
            Center of mass coordinates.
        tm : float
            Total mass.
        ircs : int
            Inertia reference coordinate system.
        nodeid : int
            Node ID for center of mass.
        ixx, ixy, ixz, iyy, iyz, izz : float
            Moments of inertia.
        vtx, vty, vtz : float
            Initial translational velocity.
        vrx, vry, vrz : float
            Initial rotational velocity.
        title : str
            Part title.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        # Check if PART_INERTIA with this pid already exists and update it
        for kw in self._deck:
            if hasattr(kw, "keyword") and kw.keyword == "*PART_INERTIA":
                if hasattr(kw, "pid") and kw.pid == pid:
                    # Update existing PART_INERTIA
                    if title:
                        kw.heading = title
                    kw.secid = secid
                    kw.mid = mid
                    kw.xc = xc
                    kw.yc = yc
                    kw.zc = zc
                    kw.tm = tm
                    kw.ircs = ircs
                    kw.nodeid = nodeid
                    kw.ixx = ixx
                    kw.ixy = ixy
                    kw.ixz = ixz
                    kw.iyy = iyy
                    kw.iyz = iyz
                    kw.izz = izz
                    kw.vtx = vtx
                    kw.vty = vty
                    kw.vtz = vtz
                    kw.vrx = vrx
                    kw.vry = vry
                    kw.vrz = vrz
                    logger.debug(f"Updated existing PART_INERTIA with pid={pid}")
                    return True

        # Create new PART_INERTIA if it doesn't exist
        kw = keywords.PartInertia()
        kw.heading = title
        kw.pid = pid
        kw.secid = secid
        kw.mid = mid
        kw.xc = xc
        kw.yc = yc
        kw.zc = zc
        kw.tm = tm
        kw.ircs = ircs
        kw.nodeid = nodeid
        kw.ixx = ixx
        kw.ixy = ixy
        kw.ixz = ixz
        kw.iyy = iyy
        kw.iyz = iyz
        kw.izz = izz
        kw.vtx = vtx
        kw.vty = vty
        kw.vtz = vtz
        kw.vrx = vrx
        kw.vry = vry
        kw.vrz = vrz

        self._deck.append(kw)
        logger.debug(f"Created PART_INERTIA with pid={pid}")
        return True
