"""
ICFD API
==========

Module to create Incompressible Computational Fluid Dynamics(ICFD) dyna input deck
"""

import logging

from .dynabase import *  # noqa : F403


class DynaICFD(DynaBase):
    """Contain methods to create keyword related to ICFD."""

    def __init__(self):
        DynaBase.__init__(self)
        self.create_section_icfd(1)
        self.timestep = 0
        self.termination = 1e28

    def set_timestep(self, timestep=0):
        """Set time step for the fluid problem.

        Parameters
        ----------
        dt : float
            Time step for the fluid problem.
        """
        self.timestep = timestep

    def set_termination(self, termination_time):
        """Set total time of simulation for the fluid problem.

        Parameters
        ----------
        termination_time : float
            Total time of simulation for the fluid problem.
        """
        self.termination = termination_time

    def create_control_general(self, atype=0, mtype=0, dvcl=0, rdvcl=0):
        """Specify the type of CFD analysis.

        Parameters
        ----------
        atype : int
            Analysis type.
        mtype : int
            Solving method type.
        dvcl : int
            Divergence cleaning flag.
        rdvcl : int
            Remeshing divergence cleaning.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateControlGeneral(
            ICFDControlGeneralRequest(atype=atype, mtype=mtype, dvcl=dvcl, rdvcl=rdvcl)
        )
        logging.info("ICFD control general Created...")
        return ret

    def create_control_output(self, msgl):
        """Modify default values for screen and file outputs related to this fluid solver only.

        Parameters
        ----------
        msgl : int
            Message level.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateControlOutput(ICFDControlOutputRequest(msgl=msgl))
        logging.info("ICFD control output Created...")
        return ret

    def create_control_turbulence(self, tmod):
        """Modify the default values for the turbulence model.

        Parameters
        ----------
        tmod : int
            Indicates what turbulence model will be used.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateControlTurbulence(ICFDControlTurbulenceRequest(tmod=tmod))
        logging.info("ICFD control turbulence Created...")
        return ret

    def create_control_dem_coupling(self, ctype=0, bt=0, dt=1e28, sf=1):
        """Activate coupling between the ICFD and DEM solvers.

        Parameters
        ----------
        ctype : int
            Indicates the coupling direction to the solver.
        bt : float
            Birth time for the DEM coupling.
        dt : float
            Death time for the DEM coupling.
        sf : float
            Scale factor applied to the force transmitted by the fluid to the structure.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateControlDEMCoupling(ICFDControlDEMCouplingRequest(ctype=ctype, bt=bt, dt=dt, sf=sf))
        logging.info("ICFD control dem coupling Created...")
        return ret

    def create_section_icfd(self, sid):
        """Define a section for the incompressible flow solver.

        Parameters
        ----------
        sid : int
            Section identifier.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateSection(ICFDSectionRequest(sid=sid))
        logging.info("ICFD Section Created...")
        return ret

    def create_part_icfd(self, pid, secid, mid):
        """Define parts for this incompressible flow solver.

        Parameters
        ----------
        pid : int
            Part identifier for fluid surfaces.
        secid : int
            Section identifier defined with the \*ICFD_SECTION card.
        mid : int
            Material identifier defined with the \*ICFD_MAT card.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreatePart(ICFDPartRequest(pid=pid, secid=secid, mid=mid))
        logging.info("ICFD part Created...")
        return ret

    def create_solver_tol_mmov(self, atol=1e-8, rtol=1e-8):
        """Allow the user to change the default tolerance values for the mesh movement algorithm.

        Parameters
        ----------
        atol : float
            Absolute convergence criteria.
        rtol : float
            Relative convergence criteria.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateSolverTolMMOV(ICFDSolverTolMMOVRequest(atol=atol, rtol=rtol))
        logging.info("tolerance values for the mesh movement algorithm changed...")
        return ret

    def save_file(self):
        """Save keyword files."""
        self.create_section_icfd(1)
        DynaBase.save_file(self)


class ICFD_SurfRemeshMethod(Enum):
    LAPLACIAN_SMOOTHING = 1
    CURVATURE_PRESERVING = 2


class ICFDAnalysis:
    """Activate ICFD analysis and define associated control parameters."""

    def __init__(self):
        self.defined_timestep = False
        self.defined_volumemesh = False
        self.defined_surfmesh = False
        self.stub = DynaBase.get_stub()

    def set_timestep(self, timestep=0):
        """Set time step for the fluid problem.

        Parameters
        ----------
        dt : float
            Time step for the fluid problem.
        """
        self.defined_timestep = True
        self.timestep = timestep

    def set_volume_mesh(self, mesh_growth_scale_factor=1.41):
        """Modify default values for the automatic volume mesh generation.

        Parameters
        ----------
        mesh_growth_scale_factor : float
            Specifies the maximum mesh size that the volume mesher is allowed to use when generating the volume mesh.
        """
        self.defined_volumemesh = True
        self.mgsf = mesh_growth_scale_factor

    def set_surface_mesh(self, remesh_method=ICFD_SurfRemeshMethod.LAPLACIAN_SMOOTHING):
        """Enable automatic surface re-meshing.

        Parameters
        ----------
        remesh_method : ICFD_SurfRemeshMethod
            Indicates whether or not to perform a surface re-meshing.
        """
        self.defined_surfmesh = True
        self.rsrf = remesh_method.value

    def create(self):
        """Create ICFD analysis."""
        if self.defined_timestep:
            self.stub.ICFDCreateControlTime(ICFDControlTimeRequest(tim=DynaSolution.termination_time, dt=self.timestep))
        if self.defined_volumemesh:
            self.stub.ICFDCreateControlMesh(ICFDControlMeshRequest(mgsf=self.mgsf))
        if self.defined_surfmesh:
            self.stub.ICFDCreateControlSurfMesh(ICFDControlSurfMeshRequest(rsrf=self.rsrf))


class Compressible(Enum):
    VACUUM = 0
    FULLY_INCOMPRESSIBLE_FLUID = 1


class MatICFD:
    """Specify physical properties for the fluid material.

    Parameters
    ----------
        flag : int
            Flag to choose between fully incompressible, slightly compressible, or barotropic flows:
            EQ.0: Vacuum (free surface problems only)
            EQ.1: Fully incompressible fluid.
        flow_density : float
            Flow density.
        dynamic_viscosity : float
            Dynamic viscosity.
    """

    def __init__(self, flag=Compressible.FULLY_INCOMPRESSIBLE_FLUID, flow_density=0, dynamic_viscosity=0):
        self.stub = DynaBase.get_stub()
        self.flag = flag.value
        self.flow_density = flow_density
        self.dynamic_viscosity = dynamic_viscosity

    def create(self, stub):
        """Create ICFD material."""
        ret = self.stub.ICFDCreateMat(ICFDMatRequest(flg=self.flag, ro=self.flow_density, vis=self.dynamic_viscosity))
        self.material_id = ret.id
        logging.info(f"ICFD material {self.material_id} Created...")


class ICFDDOF(Enum):
    X = 1
    Y = 2
    Z = 3


class Vel(Enum):
    LINEAR_VELOCITY = 1
    ANGULAR_VELOCITY = 2


class ICFDPart:
    """Define part for the incompressible flow solver."""

    def __init__(self, id):
        self.stub = DynaBase.get_stub()
        self.type = "ICFD"
        self.id = id
        self.secid = 1
        self.mid = 0

    def set_material(self, mat):
        """Set material."""
        mat.create(self.stub)
        self.mid = mat.material_id

    def set_prescribed_velocity(self, motion, dof=ICFDDOF.X, velocity_flag=Vel.LINEAR_VELOCITY):
        """Impose the fluid velocity on the boundary.

        Parameters
        ----------
        dof : int
            Applicable degrees of freedom:
            EQ.1: x-degree of freedom.
            EQ.2: y-degree of freedom.
            EQ.3: z-degree of freedom.
            EQ.4: Normal direction degree of freedom.
        velocity_flag : int
            Velocity flag:
            EQ.1: Linear velocity.
            EQ.2: Angular velocity.
            EQ.3: Parabolic velocity profile.
            EQ.4: Activates synthetic turbulent field on part.
        motion : Curve
            Load curve used to describe motion value versus time.
        """
        motion.create(self.stub)
        lcid = motion.id
        ret = self.stub.ICFDCreateBdyPrescribedVel(
            ICFDBdyPrescribedVelRequest(pid=self.id, dof=dof.value, vad=velocity_flag.value, lcid=lcid)
        )
        logging.info("ICFD boundary prescribed velocity Created...")

    def set_prescribed_pressure(self, pressure):
        """Impose a fluid pressure on the boundary.

        Parameters
        ----------
        pressure : Curve
            Load curve to describe the pressure value versus time.
        """
        pressure.create(self.stub)
        lcid = pressure.id
        ret = self.stub.ICFDCreateBdyPrescribedPre(ICFDBdyPrescribedPreRequest(pid=self.id, lcid=lcid))
        logging.info("ICFD boundary prescribed pressure Created...")
        return ret

    def set_free_slip(self):
        """Specify the fluid boundary with free-slip boundary condition."""
        ret = self.stub.ICFDCreateBdyFreeSlip(ICFDBdyFreeSlipRequest(pid=self.id))
        logging.info("ICFD boundary freeslip Created...")
        return ret

    def set_non_slip(self):
        """Specify the fluid boundary with a non-slip boundary condition."""
        ret = self.stub.ICFDCreateBdyNonSlip(ICFDBdyNonSlipRequest(pid=self.id))
        logging.info("ICFD boundary nonslip Created...")
        return ret

    def compute_drag_force(self):
        """Enable the computation of drag forces over given surface parts of the model."""
        ret = self.stub.ICFDCreateDBDrag(ICFDDBDragRequest(pid=self.id))
        logging.info("ICFD database drag Created...")
        return ret

    def compute_flux(self):
        """Enable the computation of the flow rate and average pressure over given parts of the model."""
        ret = self.stub.ICFDCreateDBFlux(ICFDDBFluxRequest(pid=self.id))
        logging.info("ICFD database flux Created...")
        return ret

    def set_boundary_layer(self, number=3):
        """Define a boundary-layer mesh as a refinement on volume-mesh.

        Parameters
        ----------
        number : int
            Number of elements normal to the surface (in the boundary layer).
        """
        ret = self.stub.MESHCreateBl(MeshBlRequest(pid=self.id, nelth=number - 1))
        logging.info("MESH boundary-layer Created...")
        return ret

    def set_boundary_layer_symmetry_condition(self):
        """Specify the part that will have symmetry conditions for the boundary layer."""
        ret = self.stub.MESHCreateBlSym(MeshBlSymRequest(pid=self.id))
        return ret

    def set_property(self):
        """Set properties for ICFD part."""
        secid = 1
        self.stub.SetICFDPartProperty(ICFDPartPropertyRequest(pid=self.id, secid=secid, mid=self.mid))


class ICFDVolumePart:
    """Assign material properties to the nodes enclosed by surface ICFD parts.

    Parameters
    ----------
    surfaces : list
        List of Part IDs for the surface elements that define the volume mesh.
    """

    def __init__(self, surfaces):
        self.stub = DynaBase.get_stub()
        self.type = "ICFDVOLUME"
        self.id = id
        self.secid = 1
        self.mid = 0
        self.surfaces = surfaces

    def set_material(self, mat):
        """Set material."""
        self.mid = mat.material_id

    def create(self):
        """Create ICFD volume part."""
        ret = self.stub.ICFDCreatePartVol(ICFDPartVolRequest(secid=1, mid=self.mid, spids=self.surfaces))
        self.id = ret.id
        logging.info(f"ICFD part volume {self.id} Created...")
        return ret


class MeshedVolume:
    """Define the volume space that will be meshed.

    Parameters
    ----------
    surfaces : list
            list of Part IDs for the surface elements that are used to define the volume.
    """

    def __init__(self, surfaces):
        self.surfaces = surfaces
        self.stub = DynaBase.get_stub()
        self.meshsizeshape = []
        self.embeded_surf = []

    def embed_shell(self, embeded):
        """Define surfaces that the mesher will embed inside the volume mesh.

        Parameters
        ----------
        embeded : list
            Part IDs for the surface elements that will be embedded in the volume mesh.
        """
        self.embeded_surf = embeded

    def meshsize_box(self, size, min_point, max_point):
        """Define a local mesh size in specific zones corresponding to given geometrical shapes.

        Parameters
        ----------
        size : float
            Mesh size that needs to be applied in the zone of the shape defined by SNAME
        parameter : list
            The parameters to define shape.
        """
        parameter = [min_point.x, min_point.y, min_point.z, max_point.x, max_point.y, max_point.z]
        self.meshsizeshape.append(["BOX", size, parameter])

    def create(self):
        """Create mesh volume."""
        ret = self.stub.MESHCreateVolume(MeshVolumeRequest(pids=self.surfaces))
        self.id = ret.id
        logging.info(f"MESH volume {self.id} Created...")
        if len(self.embeded_surf) > 0:
            self.stub.MESHCreateEmbedShell(MeshEmbedShellRequest(volid=self.id, pids=self.embeded_surf))
            logging.info("Embed surfaces Created...")
        for i in range(len(self.meshsizeshape)):
            self.stub.MESHCreateSizeShape(
                MeshSizeShapeRequest(
                    sname=self.meshsizeshape[i][0],
                    force=1,
                    method=0,
                    msize=self.meshsizeshape[i][1],
                    parameter=self.meshsizeshape[i][2],
                )
            )
        logging.info("MESH size shape Created...")
