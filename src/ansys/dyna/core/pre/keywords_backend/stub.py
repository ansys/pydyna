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
Keywords Stub
=============

Stub class that mimics the gRPC stub interface for keywords backend.
"""

import logging
import os
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ansys.dyna.core.pre.keywords_backend.backend import KeywordsBackend

logger = logging.getLogger(__name__)


class KeywordsStub:
    """Stub class that mimics the gRPC stub interface for keywords backend.

    This class provides the same method signatures as the gRPC stub but
    implements them using the KeywordsBackend.
    """

    def __init__(self, backend: "KeywordsBackend"):
        """Initialize the stub with a backend.

        Parameters
        ----------
        backend : KeywordsBackend
            The keywords backend instance.
        """
        self._backend = backend

    # =========================================================================
    # Core Methods
    # =========================================================================

    def CreateTermination(self, request):
        """Create termination keyword."""
        self._backend.create_termination(request.endtim)
        return type("Response", (), {"success": True})()

    def SaveFile(self, request):
        """Save the keyword file."""
        output_path = self._backend.save_file(request.name)
        return type("Response", (), {"outpath": output_path})()

    def LoadFile(self, request):
        """Load a keyword file."""
        return type("Response", (), {"success": True})()

    def kwSetFileName(self, request):
        """Set the filename."""
        if request.num == 0:
            self._backend.set_main_filename(request.name)
        return type("Response", (), {"success": True})()

    def Upload(self, chunks):
        """Upload file chunks (no-op for local backend)."""
        # For local backend, files are loaded directly
        return type("Response", (), {"success": True})()

    def Download(self, request):
        """Download file (yields chunks)."""
        # For local backend, read the file and yield chunks
        filepath = request.url
        if os.path.exists(filepath):
            with open(filepath, "rb") as f:
                while True:
                    chunk = f.read(1024 * 1024)
                    if not chunk:
                        break
                    yield type("Chunk", (), {"buffer": chunk})()

    # =========================================================================
    # Database Methods
    # =========================================================================

    def CreateDBBinary(self, request):
        """Create database binary keyword."""
        self._backend.create_database_binary(
            filetype=request.filetype,
            dt=request.dt,
            maxint=request.maxint,
            ieverp=request.ieverp,
            dcomp=request.dcomp,
            nintsld=request.nintsld,
        )
        return type("Response", (), {"success": True})()

    def CreateDBAscii(self, request):
        """Create database ASCII keyword."""
        self._backend.create_database_ascii(
            db_type=request.type,
            dt=request.dt,
            binary=request.binary,
            lcur=request.lcur,
            ioopt=request.ioopt,
        )
        return type("Response", (), {"success": True})()

    # =========================================================================
    # Define Methods
    # =========================================================================

    def CreateDefineCurve(self, request):
        """Create define curve keyword."""
        curve_id = self._backend.create_define_curve(
            sfo=request.sfo,
            abscissa=list(request.abscissa),
            ordinate=list(request.ordinate),
            title=request.title,
        )
        return type("Response", (), {"id": curve_id})()

    # =========================================================================
    # Control Methods
    # =========================================================================

    def CreateControlAccuracy(self, request):
        """Create control accuracy keyword."""
        self._backend.create_control_accuracy(
            osu=request.osu,
            inn=request.inn,
            pidosu=request.pidosu,
            iacc=request.iacc,
            exacc=request.exacc,
        )
        return type("Response", (), {"success": True})()

    def CreateControlEnergy(self, request):
        """Create control energy keyword."""
        self._backend.create_control_energy(
            hgen=request.hgen,
            rwen=request.rwen,
            slnten=request.slnten,
            rylen=request.rylen,
            irgen=request.irgen,
        )
        return type("Response", (), {"success": True})()

    def CreateControlHourgalss(self, request):
        """Create control hourglass keyword."""
        self._backend.create_control_hourglass(
            ihq=request.ihq,
            qh=request.qh,
        )
        return type("Response", (), {"success": True})()

    def CreateControlBulkViscosity(self, request):
        """Create control bulk viscosity keyword."""
        self._backend.create_control_bulk_viscosity(
            q1=request.q1,
            q2=request.q2,
            bulk_type=request.type,
        )
        return type("Response", (), {"success": True})()

    def CreateControlShell(self, request):
        """Create control shell keyword."""
        self._backend.create_control_shell(
            wrpang=request.wrpang,
            esort=request.esort,
            irnxx=request.irnxx,
            istupd=request.istupd,
            theory=request.theory,
            bwc=request.bwc,
            miter=request.miter,
            proj=request.proj,
            irquad=request.irquad,
        )
        return type("Response", (), {"success": True})()

    def CreateControlContact(self, request):
        """Create control contact keyword."""
        self._backend.create_control_contact(
            rwpnal=request.rwpnal,
            shlthk=request.shlthk,
            orien=request.orien,
            ssthk=request.ssthk,
            ignore=request.ignore,
            igactc=request.igactc,
        )
        return type("Response", (), {"success": True})()

    def CreateTimestep(self, request):
        """Create control timestep keyword."""
        self._backend.create_control_timestep(
            tssfac=request.tssfac,
            isdo=request.isdo,
            dt2ms=request.dt2ms,
            lctm=request.lctm,
        )
        return type("Response", (), {"success": True})()

    def CreateControlSolution(self, request):
        """Create control solution keyword."""
        self._backend.create_control_solution(soln=request.soln)
        return type("Response", (), {"success": True})()

    def CreateControlThermalSolver(self, request):
        """Create control thermal solver keyword."""
        self._backend.create_control_thermal_solver(atype=request.atype)
        return type("Response", (), {"success": True})()

    def CreateControlThermalTimestep(self, request):
        """Create control thermal timestep keyword."""
        self._backend.create_control_thermal_timestep(its=request.its)
        return type("Response", (), {"success": True})()

    def CreateControlThermalNonlinear(self, request):
        """Create control thermal nonlinear keyword (stub)."""
        # TODO: Implement CONTROL_THERMAL_NONLINEAR when needed
        return type("Response", (), {"success": True})()

    # =========================================================================
    # Initial Condition Methods
    # =========================================================================

    def CreateInitTemperature(self, request):
        """Create initial temperature keyword."""
        self._backend.create_initial_temperature(
            option=request.option,
            nsid=request.nsid,
            temp=request.temp,
        )
        return type("Response", (), {"success": True})()

    def CreateInitTemperatureSet(self, request):
        """Create INITIAL_TEMPERATURE_SET keyword."""
        self._backend.create_initial_temperature_set(
            nsid=getattr(request, "nsid", 0),
            temp=request.temp,
        )
        return type("Response", (), {"success": True})()

    # =========================================================================
    # Material Methods
    # =========================================================================

    def CreateMatElasticPlasticThermal(self, request):
        """Create MAT_ELASTIC_PLASTIC_THERMAL keyword."""
        mid = self._backend.create_mat_elastic_plastic_thermal(
            ro=request.ro,
            ti=request.ti,
            ei=request.ei,
            pri=request.pri,
            alphai=request.alphai,
            sigyi=request.sigyi,
        )
        return type("Response", (), {"mid": mid})()

    def CreateMatThermalIsotropic(self, request):
        """Create MAT_THERMAL_ISOTROPIC keyword."""
        # Get material ID - either from request or generate new one
        tmid = getattr(request, "mid", None) or self._backend.next_id("thermal_material")
        self._backend.create_mat_thermal_isotropic(
            tmid=tmid,
            ro=request.tro,
            tgrlc=request.tgrlc,
            tgmult=request.tgmult,
            hc=request.hc,
            tc=request.tc,
        )
        return type("Response", (), {"mid": tmid})()

    def CreateMatRigid(self, request):
        """Create MAT_RIGID keyword."""
        from ansys.dyna.core.keywords import keywords

        mid = self._backend.next_id("material")

        kw = keywords.MatRigid()
        kw.mid = mid
        kw.ro = request.ro
        kw.e = request.e
        kw.pr = request.pr
        kw.cmo = getattr(request, "cmo", 0)
        kw.con1 = getattr(request, "con1", 0)
        kw.con2 = getattr(request, "con2", 0)

        self._backend._deck.append(kw)
        return type("Response", (), {"mid": mid})()

    def CreateMatRigidDiscrete(self, request):
        """Create MAT_RIGID_DISCRETE keyword."""
        mid = self._backend.next_id("material")
        self._backend.create_mat_rigid_discrete(
            mid=mid,
            ro=getattr(request, "ro", 0.0),
            e=getattr(request, "e", 0.0),
            pr=getattr(request, "pr", 0.0),
        )
        return type("Response", (), {"mid": mid})()

    # =========================================================================
    # Section Methods
    # =========================================================================

    def CreateSectionSolid(self, request):
        """Create SECTION_SOLID keyword."""
        secid = self._backend.next_id("section")
        self._backend.create_section_solid(secid=secid, elform=request.elform)
        return type("Response", (), {"id": secid})()

    def CreateSectionShell(self, request):
        """Create SECTION_SHELL keyword."""
        secid = self._backend.next_id("section")
        # propt must be 1, 2, 3, or None - convert 0 to None
        propt = getattr(request, "propt", 1)
        if propt == 0:
            propt = None
        self._backend.create_section_shell(
            secid=secid,
            elform=request.elform,
            shrf=getattr(request, "shrf", 1.0),
            nip=getattr(request, "nip", 5),
            propt=propt,
            t1=getattr(request, "t1", 0),
            t2=getattr(request, "t2", 0),
            t3=getattr(request, "t3", 0),
            t4=getattr(request, "t4", 0),
        )
        return type("Response", (), {"id": secid})()

    # =========================================================================
    # Set Methods
    # =========================================================================

    def CreateNodeSet(self, request):
        """Create a node set keyword."""
        # Generate a set ID if not provided
        sid = self._backend.next_id("nodeset")
        nodes = list(request.entities) if request.entities else []

        if request.option == "LIST":
            solver = "MECH"  # Default solver
            self._backend.create_set_node_list_with_solver(sid=sid, nodes=nodes, solver=solver)
        else:
            self._backend.create_set_node_list(sid=sid, nodes=nodes)

        return type("Response", (), {"id": sid})()

    def CreateSegmentSet(self, request):
        """Create SET_SEGMENT keyword."""
        sid = self._backend.next_id("segmentset")
        # Request has n1, n2, n3, n4 arrays - combine them into segments
        n1 = list(request.n1) if request.n1 else []
        n2 = list(request.n2) if request.n2 else []
        n3 = list(request.n3) if request.n3 else []
        n4 = list(request.n4) if request.n4 else []
        segments = list(zip(n1, n2, n3, n4))
        self._backend.create_set_segment(sid=sid, segments=segments, solver="MECH")
        return type("Response", (), {"id": sid})()

    def CreatePartSet(self, request):
        """Create SET_PART_LIST keyword."""
        sid = self._backend.next_id("partset")
        # Handle both 'entities' (from newer API) and 'pids' (from older API)
        parts = []
        if hasattr(request, "entities") and request.entities:
            parts = list(request.entities)
        elif hasattr(request, "pids") and request.pids:
            # pids can be a protobuf repeated container, single int, or list
            try:
                # Try to iterate over it (handles lists and protobuf containers)
                parts = [int(p) for p in request.pids]
            except TypeError:
                # Single value
                parts = [int(request.pids)]
        self._backend.create_set_part_list(sid=sid, parts=parts)
        return type("Response", (), {"id": sid})()

    # =========================================================================
    # Part Methods
    # =========================================================================

    def SetPartProperty(self, request):
        """Set part properties."""
        # Find the part keyword and update it, or create one if needed
        self._backend.set_part_property(
            pid=request.pid,
            secid=request.secid,
            mid=request.mid,
            eosid=getattr(request, "eosid", 0),
            hgid=getattr(request, "hgid", 0),
            grav=getattr(request, "grav", 0),
            adpopt=getattr(request, "adpopt", 0),
            tmid=getattr(request, "tmid", 0),
        )
        return type("Response", (), {"success": True})()

    def CreateHourglass(self, request):
        """Create HOURGLASS keyword."""
        hgid = self._backend.next_id("hourglass")
        # For individual part hourglass, we'll create a HOURGLASS keyword
        from ansys.dyna.core.keywords import keywords

        kw = keywords.Hourglass()
        kw.hgid = hgid
        kw.ihq = request.ihq
        kw.qm = request.qm
        kw.q1 = getattr(request, "q1", 0)
        kw.q2 = getattr(request, "q2", 0)
        kw.qb = getattr(request, "qb", 0)
        kw.qw = getattr(request, "qw", 0)

        self._backend._deck.append(kw)
        return type("Response", (), {"id": hgid})()

    # =========================================================================
    # Boundary Methods
    # =========================================================================

    def CreateBoundaryPrescribedMotionRigid(self, request):
        """Create BOUNDARY_PRESCRIBED_MOTION_RIGID keyword."""
        self._backend.create_boundary_prescribed_motion_rigid(
            pid=request.pid,
            dof=request.dof,
            vad=request.vad,
            lcid=request.lcid,
            sf=getattr(request, "sf", 1.0),
        )
        return type("Response", (), {"success": True})()

    def CreateBdyPrescribedMotion(self, request):
        """Create BOUNDARY_PRESCRIBED_MOTION keyword based on option type."""
        option = getattr(request, "option", "")
        if option == "RIGID":
            self._backend.create_boundary_prescribed_motion_rigid(
                pid=request.typeid,
                dof=request.dof,
                vad=request.vad,
                lcid=request.lcid,
                sf=getattr(request, "sf", 1.0),
                death=getattr(request, "death", 0.0),
                birth=getattr(request, "birth", 0.0),
            )
        # For SET option, we'd need a different backend method
        return type("Response", (), {"success": True})()

    # =========================================================================
    # EM (Electromagnetic) Methods
    # =========================================================================

    def CreateEMControl(self, request):
        """Create EM_CONTROL keyword."""
        self._backend.create_em_control(
            emsol=request.emsol,
            numls=request.numls,
            macrodt=request.macrodt,
            dimtype=getattr(request, "dimtype", 0),
            nperio=getattr(request, "nperio", 2),
            ncylfem=request.ncylfem,
            ncylbem=request.ncylbem,
        )
        return type("Response", (), {"success": True})()

    def CreateEMTimestep(self, request):
        """Create EM_CONTROL_TIMESTEP keyword."""
        self._backend.create_em_timestep(
            tstype=request.tstype,
            dtconst=request.dtconst,
        )
        return type("Response", (), {"success": True})()

    def CreateEMOutput(self, request):
        """Create EM_OUTPUT keyword."""
        self._backend.create_em_output(
            mats=request.mats,
            matf=request.matf,
            sols=request.sols,
            solf=request.solf,
        )
        return type("Response", (), {"success": True})()

    def CreateEMMat001(self, request):
        """Create EM_MAT_001 keyword."""
        self._backend.create_em_mat_001(
            mid=request.mid,
            mtype=request.mtype,
            sigma=request.sigma,
        )
        return type("Response", (), {"success": True})()

    def CreateEMMat(self, request):
        """Create EM_MAT_001 keyword for material EM properties."""
        self._backend.create_em_mat_001(
            mid=request.mid,
            mtype=request.mtype,
            sigma=request.sigma,
        )
        return type("Response", (), {"success": True})()

    def CreateEMMat004(self, request):
        """Create EM_MAT_004 keyword for 2D resistive heating."""
        self._backend.create_em_mat_004(
            mid=request.mid,
            mtype=request.mtype,
            sigma=request.sigma,
        )
        return type("Response", (), {"success": True})()

    def CreateEMIsopotential(self, request):
        """Create EM_ISOPOTENTIAL keyword."""
        isoid = self._backend.create_em_isopotential(
            settype=request.settype,
            setid=request.setid,
            rdltype=getattr(request, "rdltype", 0),
        )
        return type("Response", (), {"id": isoid})()

    def CreateEMIsopotentialConnect(self, request):
        """Create EM_ISOPOTENTIAL_CONNECT keyword."""
        conid = self._backend.create_em_isopotential_connect(
            contype=request.contype,
            isoid1=request.isoid1,
            isoid2=request.isoid2,
            val=request.val,
            lcid=getattr(request, "lcid", 0),
            l=getattr(request, "l", 0),
            c=getattr(request, "c", 0),
            v0=getattr(request, "v0", 0),
        )
        return type("Response", (), {"id": conid})()

    def CreateEMIsopotentialRogo(self, request):
        """Create EM_ISOPOTENTIAL_ROGO keyword."""
        isoid = self._backend.next_id("isopotential")
        self._backend.create_em_isopotential_rogo(
            isoid=isoid,
            settype=request.settype,
            setid=request.setid,
        )
        return type("Response", (), {"id": isoid})()

    def CreateEMCircuitRogo(self, request):
        """Create EM_CIRCUIT_ROGO keyword (maps to EM_ISOPOTENTIAL_ROGO)."""
        # The old API uses CreateEMCircuitRogo but we use EM_ISOPOTENTIAL_ROGO
        # First, create an isopotential and then the rogo connection
        isoid = self._backend.create_em_isopotential(
            settype=request.settype,
            setid=request.setid,
            rdltype=0,
        )
        self._backend.create_em_isopotential_rogo(
            isoid=isoid,
            settype=request.settype,
            setid=request.setid,
        )
        return type("Response", (), {"id": isoid})()

    def CreateEMSolverFem(self, request):
        """Create EM_SOLVER_FEM keyword."""
        self._backend.create_em_solver_fem(
            reltol=request.reltol,
            maxite=request.maxite,
            stype=request.stype,
            precon=getattr(request, "precon", 1),
            uselast=getattr(request, "uselast", 1),
            ncyclfem=getattr(request, "ncylbem", 3),
        )
        return type("Response", (), {"success": True})()

    # =========================================================================
    # ICFD (Incompressible CFD) Methods
    # =========================================================================

    def ICFDCreateSection(self, request):
        """Create ICFD_SECTION keyword."""
        self._backend.create_icfd_section(sid=request.sid)
        return type("Response", (), {"success": True})()

    def ICFDCreateControlTime(self, request):
        """Create ICFD_CONTROL_TIME keyword."""
        self._backend.create_icfd_control_time(
            ttm=request.tim,
            dt=getattr(request, "dt", 0.0),
            cfl=getattr(request, "cfl", 1.0),
            lcidsf=getattr(request, "lcidsf", 0),
            dtmin=getattr(request, "dtmin", 0.0),
            dtmax=getattr(request, "dtmax", 0.0),
            dtinit=getattr(request, "dtinit", 0.0),
            tdeath=getattr(request, "tdeath", 0.0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreatePartVol(self, request):
        """Create ICFD_PART_VOL keyword."""
        # Use "icfd_part" category to share IDs with regular ICFD parts
        pid = self._backend.next_id("icfd_part")
        self._backend.create_icfd_part_vol(
            pid=pid,
            secid=request.secid,
            mid=getattr(request, "mid", 0),
            spids=list(request.spids) if request.spids else None,
        )
        return type("Response", (), {"id": pid})()

    def MESHCreateVolume(self, request):
        """Create MESH_VOLUME keyword."""
        volid = self._backend.next_id("mesh_volume")
        self._backend.create_mesh_volume(
            volid=volid,
            pids=list(request.pids) if request.pids else None,
        )
        return type("Response", (), {"id": volid})()

    def MESHCreateSize(self, request):
        """Create MESH_SIZE keyword."""
        self._backend.create_mesh_size(
            volid=request.volid,
            pids=list(request.pids) if request.pids else None,
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlGeneral(self, request):
        """Create ICFD_CONTROL_GENERAL keyword."""
        self._backend.create_icfd_control_general(
            atype=getattr(request, "atype", 0),
            mtype=getattr(request, "mtype", 0),
            dvcl=getattr(request, "dvcl", 0),
            rdvcl=getattr(request, "rdvcl", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlOutput(self, request):
        """Create ICFD_CONTROL_OUTPUT keyword."""
        self._backend.create_icfd_control_output(
            msgl=getattr(request, "msgl", 0),
            itout=getattr(request, "itout", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlSteady(self, request):
        """Create ICFD_CONTROL_STEADY keyword."""
        self._backend.create_icfd_control_steady(
            its=getattr(request, "its", 1000000),
            tol1=getattr(request, "tol1", 0.001),
            tol2=getattr(request, "tol2", 0.001),
            tol3=getattr(request, "tol3", 0.001),
            rel1=getattr(request, "rel1", 0.3),
            rel2=getattr(request, "rel2", 0.7),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateMat(self, request):
        """Create ICFD_MAT keyword."""
        mid = self._backend.next_id("icfd_mat")
        self._backend.create_icfd_mat(
            mid=mid,
            flg=getattr(request, "flg", 1),
            ro=getattr(request, "ro", 0.0),
            vis=getattr(request, "vis", 0.0),
            hc=getattr(request, "hc", 0.0),
            tc=getattr(request, "tc", 0.0),
            beta=getattr(request, "beta", 0.0),
            prt=getattr(request, "prt", 0.0),
        )
        return type("Response", (), {"id": mid})()

    def ICFDCreatePart(self, request):
        """Create ICFD_PART keyword."""
        # Track the part ID to keep the counter in sync with ICFDPartVol
        pid = request.pid
        self._backend.ensure_min_id("icfd_part", pid)
        self._backend.create_icfd_part(
            pid=pid,
            secid=request.secid,
            mid=getattr(request, "mid", 0),
        )
        return type("Response", (), {"success": True})()

    def SetICFDPartProperty(self, request):
        """Set ICFD_PART properties (secid and mid)."""
        # Track the part ID to keep the counter in sync with ICFDPartVol
        self._backend.ensure_min_id("icfd_part", request.pid)
        self._backend.set_icfd_part_property(
            pid=request.pid,
            secid=request.secid,
            mid=request.mid,
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateBdyNonSlip(self, request):
        """Create ICFD_BOUNDARY_NONSLIP keyword."""
        self._backend.create_icfd_boundary_nonslip(pid=request.pid)
        return type("Response", (), {"success": True})()

    def ICFDCreateBdyPrescribedVel(self, request):
        """Create ICFD_BOUNDARY_PRESCRIBED_VEL keyword."""
        self._backend.create_icfd_boundary_prescribed_vel(
            pid=request.pid,
            dof=getattr(request, "dof", 1),
            vad=getattr(request, "vad", 1),
            lcid=getattr(request, "lcid", 0),
            sf=getattr(request, "sf", 1.0),
            death=getattr(request, "death", 0.0),
            birth=getattr(request, "birth", 0.0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateBdyFreeSlip(self, request):
        """Create ICFD_BOUNDARY_FREESLIP keyword."""
        self._backend.create_icfd_boundary_freeslip(pid=request.pid)
        return type("Response", (), {"success": True})()

    def ICFDCreateBdyPrescribedPre(self, request):
        """Create ICFD_BOUNDARY_PRESCRIBED_PRE keyword."""
        self._backend.create_icfd_boundary_prescribed_pre(
            pid=request.pid,
            lcid=getattr(request, "lcid", 0),
            sf=getattr(request, "sf", 0.0),
            death=getattr(request, "death", 0.0),
            birth=getattr(request, "birth", 0.0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlAdapt(self, request):
        """Create ICFD_CONTROL_ADAPT keyword."""
        self._backend.create_icfd_control_adapt(
            minh=getattr(request, "minh", 0.0),
            maxh=getattr(request, "maxh", 0.0),
            err=getattr(request, "err", 1.0),
            nit=getattr(request, "nit", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateDBDrag(self, request):
        """Create ICFD_DATABASE_DRAG keyword."""
        self._backend.create_icfd_database_drag(pid=request.pid)
        return type("Response", (), {"success": True})()

    def ICFDCreateDBTemp(self, request):
        """Create ICFD_DATABASE_TEMP keyword."""
        self._backend.create_icfd_database_temp(pid=request.pid)
        return type("Response", (), {"success": True})()

    def ICFDCreateBdyPrescribedTemp(self, request):
        """Create ICFD_BOUNDARY_PRESCRIBED_TEMP keyword."""
        self._backend.create_icfd_boundary_prescribed_temp(
            pid=request.pid,
            lcid=getattr(request, "lcid", 0),
            sf=getattr(request, "sf", 0.0),
            death=getattr(request, "death", 0.0),
            birth=getattr(request, "birth", 0.0),
        )
        return type("Response", (), {"success": True})()

    def MESHCreateBl(self, request):
        """Create MESH_BL keyword."""
        self._backend.create_mesh_bl(
            pid=request.pid,
            nelth=getattr(request, "nelth", 0),
        )
        return type("Response", (), {"success": True})()

    def CreateControlDiscreteElement(self, request):
        """Create CONTROL_DISCRETE_ELEMENT keyword."""
        self._backend.create_control_discrete_element(
            ndamp=getattr(request, "ndamp", 0.0),
            tdamp=getattr(request, "tdamp", 0.0),
            frics=getattr(request, "frics", 0.0),
            fricr=getattr(request, "fricr", 0.0),
            normk=getattr(request, "normk", 0.01),
            sheark=getattr(request, "sheark", 0.2857),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlDEMCoupling(self, request):
        """Create ICFD_CONTROL_DEM_COUPLING keyword."""
        self._backend.create_icfd_control_dem_coupling(
            ctype=getattr(request, "ctype", 0),
            bt=getattr(request, "bt", 0.0),
            dt=getattr(request, "dt", 1e28),
            sf=getattr(request, "sf", 1.0),
            form=getattr(request, "form", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateInit(self, request):
        """Create ICFD_INITIAL keyword."""
        self._backend.create_icfd_initial(
            pid=getattr(request, "pid", 0),
            vx=getattr(request, "vx", 0.0),
            vy=getattr(request, "vy", 0.0),
            vz=getattr(request, "vz", 0.0),
            t=getattr(request, "t", 0.0),
            p=getattr(request, "p", 0.0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlMesh(self, request):
        """Create ICFD_CONTROL_MESH keyword."""
        self._backend.create_icfd_control_mesh(
            mgsf=getattr(request, "mgsf", 1.41),
            mstrat=getattr(request, "mstrat", 0),
            struct2d=getattr(request, "struct2d", 0),
            nrmsh=getattr(request, "nrmsh", 0),
            aver=getattr(request, "aver", 14),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlSurfMesh(self, request):
        """Create ICFD_CONTROL_SURFMESH keyword."""
        self._backend.create_icfd_control_surfmesh(
            rsrf=getattr(request, "rsrf", 0),
            sadapt=getattr(request, "sadapt", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateDBFlux(self, request):
        """Create ICFD_DATABASE_FLUX keyword."""
        self._backend.create_icfd_database_flux(
            pid=getattr(request, "pid", 0),
            dtout=getattr(request, "dtout", 0.0),
        )
        return type("Response", (), {"success": True})()

    def MESHCreateBlSym(self, request):
        """Create MESH_BL_SYM keyword."""
        self._backend.create_mesh_bl_sym(
            pid=getattr(request, "pid", 0),
        )
        return type("Response", (), {"success": True})()

    def ICFDCreateControlImposedMove(self, request):
        """Create ICFD_CONTROL_IMPOSED_MOVE keyword."""
        self._backend.create_icfd_control_imposed_move(
            pid=getattr(request, "pid", 0),
            lcvx=getattr(request, "lcvx", 0),
            lcvy=getattr(request, "lcvy", 0),
            lcvz=getattr(request, "lcvz", 0),
            vadt=getattr(request, "vadt", 0),
            idr=getattr(request, "idr", 0),
        )
        return type("Response", (), {"success": True})()

    def CreateDefineCurveFunction(self, request):
        """Create DEFINE_CURVE_FUNCTION keyword."""
        curve_id = self._backend.create_define_curve_function(
            function=getattr(request, "function", ""),
            sfo=getattr(request, "sfo", 1.0),
            title=getattr(request, "title", ""),
        )
        return type("Response", (), {"id": curve_id})()

    def MESHCreateSizeShape(self, request):
        """Create MESH_SIZE_SHAPE keyword."""
        self._backend.create_mesh_size_shape(
            sname=getattr(request, "sname", "BOX"),
            force=getattr(request, "force", 1),
            method=getattr(request, "method", 0),
            msize=getattr(request, "msize", 0.0),
            parameter=list(request.parameter) if hasattr(request, "parameter") and request.parameter else None,
        )
        return type("Response", (), {"success": True})()

    def MESHCreateEmbedShell(self, request):
        """Create MESH_EMBEDSHELL keyword."""
        self._backend.create_mesh_embed_shell(
            volid=request.volid,
            pids=list(request.pids) if hasattr(request, "pids") and request.pids else None,
        )
        return type("Response", (), {"success": True})()

    def CreateLoadBody(self, request):
        """Create LOAD_BODY_* keyword."""
        option = getattr(request, "option", "Y")
        lcid = getattr(request, "lcid", 0)

        # Map option to specific load body method
        if option == "Y":
            self._backend.create_load_body_y(lcid=lcid)
        else:
            logger.warning(f"LOAD_BODY_{option} not yet implemented")

        return type("Response", (), {"success": True})()

